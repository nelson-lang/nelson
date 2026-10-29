//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenPolynomial.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "FieldNames.hpp"
#include "ExprEngine.hpp"
#include "SimEngineTypes.hpp"
#include "SimEngineBusTypes.hpp"
#include "BlockRegistry.hpp"
#include <algorithm>
#include <cctype>
#include <charconv>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <limits>
#include <queue>
#include <set>
#include <sstream>
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
//=============================================================================
// Defined in continuous/physicalIsland.cpp (same module): true when a
// physicalIsland lowers to an embedded-Newton solver in C.
// planarIslandOdeGeneratable (continuous/planarIsland.cpp): true when a
// planar island is a joint-free pure ODE the generators can lower.
namespace Nelson {
//=============================================================================
namespace NFlow {
    bool
    physicalIslandAlgebraicGeneratable(const nlohmann::json& params);
    bool
    planarIslandOdeGeneratable(const nlohmann::json& params, std::string* whyKind);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace nflow {
//=============================================================================
// JSON helpers
//=============================================================================
std::string
jstr(const json& obj, std::string_view key, std::string_view def)
{
    if (!obj.is_object()) {
        return std::string(def);
    }
    std::string kstr(key);
    auto it = obj.find(kstr);
    if (it != obj.end() && it->is_string()) {
        return it->get<std::string>();
    }
    return std::string(def);
}
//=============================================================================
int
jint(const json& obj, std::string_view key, int def)
{
    if (!obj.is_object()) {
        return def;
    }
    std::string kstr(key);
    auto it = obj.find(kstr);
    if (it != obj.end() && it->is_number()) {
        return it->get<int>();
    }
    return def;
}
//=============================================================================
json
jval(const json& obj, std::string_view key)
{
    if (!obj.is_object()) {
        return json();
    }
    std::string kstr(key);
    auto it = obj.find(kstr);
    if (it != obj.end()) {
        return *it;
    }
    return json();
}
//=============================================================================
// String helpers
//=============================================================================
std::string
sanitizeId(const std::string& id)
{
    std::string result;
    result.reserve(id.size());
    for (char c : id) {
        if (std::isalnum(static_cast<unsigned char>(c)) || c == '_') {
            result += c;
        } else {
            result += '_';
        }
    }
    return result;
}
//=============================================================================
std::string
replaceLatexVars(const std::string& expr)
{
    std::string result;
    size_t i = 0;
    while (i < expr.size()) {
        if (expr[i] == '\\' && i + 1 < expr.size()
            && std::isalpha(static_cast<unsigned char>(expr[i + 1]))) {
            i++;
            while (i < expr.size() && std::isalpha(static_cast<unsigned char>(expr[i]))) {
                result += expr[i++];
            }
        } else {
            result += expr[i++];
        }
    }
    return result;
}
//=============================================================================
static bool
isWordChar(char c)
{
    return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
}
//=============================================================================
std::string
replaceWord(const std::string& text, const std::string& word, const std::string& replacement)
{
    std::string result;
    size_t i = 0;
    while (i < text.size()) {
        if (text.compare(i, word.size(), word) == 0) {
            bool startBound = (i == 0) || !isWordChar(text[i - 1]);
            size_t end = i + word.size();
            bool endBound = (end >= text.size()) || !isWordChar(text[end]);
            if (startBound && endBound) {
                result += replacement;
                i = end;
                continue;
            }
        }
        result += text[i++];
    }
    return result;
}
//=============================================================================
std::string
sanitizeExpression(const std::string& expr)
{
    std::string s = replaceLatexVars(expr);
    return replaceWord(s, "abs", "fabs");
}
//=============================================================================
bool
isSafeUserExpression(const std::string& expr, const VarMap& vars)
{
    // 1. Character allowlist. Everything a scalar math expression needs and
    //    nothing that can break out of `out = <expr>;`: no ; { } to inject
    //    statements/scopes, no " ' to build string arguments, no \ [ ] etc.
    for (unsigned char c : expr) {
        bool ok = std::isalnum(c) || c == '_' || c == ' ' || c == '\t' || c == '.' || c == '+'
            || c == '-' || c == '*' || c == '/' || c == '^' || c == '%' || c == '(' || c == ')'
            || c == ',';
        if (!ok) {
            return false;
        }
    }
    // 2. No comment sequences (the only injection reachable with allowlisted
    //    characters: `//` and `/*` could comment out the trailing `;` or
    //    following generated lines).
    if (expr.find("//") != std::string::npos || expr.find("/*") != std::string::npos
        || expr.find("*/") != std::string::npos) {
        return false;
    }
    // 3. Must parse and evaluate as a well-formed math expression through the
    //    shared ExprEngine. This rejects the top-level comma operator
    //    (`u, f(u)`), unknown function calls and other malformed input that the
    //    permissive lexer would otherwise let through. All referenced symbols
    //    are probed with a dummy value.
    VarMap probe = vars;
    probe["u"] = 0.0;
    probe["t"] = 0.0;
    probe["dt"] = 0.0;
    try {
        // Evaluation is only used to force a full parse; the value is discarded
        // (it may legitimately be NaN, e.g. sqrt(u-1) with the probe u=0).
        (void)Nelson::NFlow::evalExpressionValue(expr, probe);
    } catch (...) {
        return false;
    }
    return true;
}
//=============================================================================
// Expression evaluation - delegates to the module-wide ExprEngine so the
// UI binding, the simulation and the code generators share one grammar.
//=============================================================================
double
evalExpression(const std::string& expr, const VarMap& vars)
{
    if (expr.empty()) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    try {
        return Nelson::NFlow::evalExpressionValue(expr, vars);
    } catch (...) {
        return std::numeric_limits<double>::quiet_NaN();
    }
}
//=============================================================================
// Label helpers
//=============================================================================
std::string
getLabelName(const json& block)
{
    json params = jval(block, nflow::kParams);
    std::string name = jstr(params, nflow::kGotoTag);
    if (name.empty()) {
        name = jstr(block, nflow::kId);
    }
    return sanitizeId(name);
}
//=============================================================================
LabelList
buildUniqueLabelList(const std::vector<json>& blocks)
{
    LabelList result;
    for (const auto& block : blocks) {
        std::string name = getLabelName(block);
        if (result.map.find(name) == result.map.end()) {
            result.map[name] = static_cast<int>(result.list.size());
            result.list.push_back(name);
        }
    }
    return result;
}
//=============================================================================
// Numeric resolution
//=============================================================================
static std::string
trimStr(const std::string& s)
{
    auto start = s.find_first_not_of(" \t\n\r");
    if (start == std::string::npos) {
        return "";
    }
    auto end = s.find_last_not_of(" \t\n\r");
    return s.substr(start, end - start + 1);
}
//=============================================================================
double
resolveNumeric(const json& value, const VarMap& vars)
{
    if (value.is_null()) {
        return 0.0;
    }
    if (value.is_number()) {
        double v = value.get<double>();
        // Infinity is a legitimate value (an integrator's "-inf" / "inf" saturation
        // limits): clamping it to 0 would pin the state to 0.
        // Only NaN collapses to 0 (callers that need a fallback check isnan).
        return std::isnan(v) ? 0.0 : v;
    }
    std::string text;
    if (value.is_string()) {
        text = value.get<std::string>();
    } else {
        return 0.0;
    }
    text = trimStr(text);
    if (text.empty()) {
        return 0.0;
    }

    char* endptr = nullptr;
    double direct = std::strtod(text.c_str(), &endptr);
    if (endptr != text.c_str() && *endptr == '\0' && !std::isnan(direct)) {
        return direct;
    }

    VarMap merged = {
        { std::string(nflow::kPi), M_PI },
        { std::string(nflow::kE), M_E },
        { std::string("inf"), std::numeric_limits<double>::infinity() },
        { std::string("infinity"), std::numeric_limits<double>::infinity() },
    };
    for (const auto& kv : vars) {
        merged[kv.first] = kv.second;
    }

    auto it = merged.find(text);
    if (it != merged.end()) {
        return std::isnan(it->second) ? 0.0 : it->second;
    }

    if (!text.empty() && text[0] == '\\') {
        std::string stripped = text.substr(1);
        it = merged.find(stripped);
        if (it != merged.end()) {
            return std::isnan(it->second) ? 0.0 : it->second;
        }
    }

    double evaluated = evalExpression(text, merged);
    return std::isnan(evaluated) ? 0.0 : evaluated;
}
//=============================================================================
bool
resolvesNumeric(const json& value, const VarMap& vars)
{
    // Same steps as resolveNumeric, but also reports whether the value resolved
    // at all. resolveNumeric turns a failure into 0.0, so a mistyped parameter
    // silently becomes zero. Kept next to it so the two cannot drift.
    if (value.is_null() || value.is_number()) {
        return true;
    }
    if (!value.is_string()) {
        return false;
    }
    std::string text = trimStr(value.get<std::string>());
    if (text.empty()) {
        return false;
    }
    char* endptr = nullptr;
    const double direct = std::strtod(text.c_str(), &endptr);
    if (endptr != text.c_str() && *endptr == 0 && !std::isnan(direct)) {
        return true;
    }
    VarMap merged = {
        { std::string(nflow::kPi), M_PI },
        { std::string(nflow::kE), M_E },
        { std::string("inf"), std::numeric_limits<double>::infinity() },
        { std::string("infinity"), std::numeric_limits<double>::infinity() },
    };
    for (const auto& kv : vars) {
        merged[kv.first] = kv.second;
    }
    if (merged.find(text) != merged.end()) {
        return true;
    }
    if (text[0] == 0x5c && merged.find(text.substr(1)) != merged.end()) {
        return true;
    }
    return !std::isnan(evalExpression(text, merged));
}
//=============================================================================
std::string
numToStr(double v)
{
    if (std::isinf(v)) {
        return v > 0 ? "INFINITY" : "-INFINITY";
    }
    if (std::isnan(v)) {
        return "0.0";
    }
    // Shortest round-trip representation
    char buf[64];
    for (int p = 1; p <= 17; ++p) {
        std::snprintf(buf, sizeof(buf), "%.*g", p, v);
        char* end = nullptr;
        double rt = std::strtod(buf, &end);
        if (rt == v) {
            return buf;
        }
    }
    std::snprintf(buf, sizeof(buf), "%.17g", v);
    return buf;
}
//=============================================================================
std::string
formatNumber(double value)
{
    if (std::isinf(value)) {
        return value > 0 ? "INFINITY" : "-INFINITY";
    }
    if (std::isnan(value)) {
        return "0.0";
    }
    return numToStr(value);
}
//=============================================================================
std::vector<double>
parseList(const json& value, const VarMap& vars)
{
    if (value.is_array()) {
        std::vector<double> result;
        result.reserve(value.size());
        for (const auto& v : value) {
            if (v.is_array()) {
                // Nested rows (a 2-D matrix literal like [[0,1],[-1,-0.4]]) are
                // flattened row-major. Without this a nested matrix collapses to
                // one 0.0 per row and silently simulates the wrong system.
                std::vector<double> inner = parseList(v, vars);
                result.insert(result.end(), inner.begin(), inner.end());
            } else {
                result.push_back(resolveNumeric(v, vars));
            }
        }
        return result;
    }
    if (value.is_number()) {
        return { value.get<double>() };
    }
    if (value.is_string()) {
        std::vector<double> result;
        std::string s = trimStr(value.get<std::string>());
        // Tolerate a bracketed literal ("[1 2 3]") and ';' separators so the
        // usual vector notations parse as lists.
        if (!s.empty() && s.front() == '[' && s.back() == ']') {
            s = s.substr(1, s.size() - 2);
        }
        bool hasSeparator = s.find(',') != std::string::npos || s.find(';') != std::string::npos;
        if (!hasSeparator && s.find(' ') != std::string::npos) {
            // Space-separated only when it does not look like an expression
            // ("1 2 3" yes, "a * 2" no).
            bool looksExpr = s.find_first_of("+-*/^(") != std::string::npos;
            if (!looksExpr) {
                for (char& c : s) {
                    if (c == ' ' || c == '\t') {
                        c = ',';
                    }
                }
                hasSeparator = true;
            }
        }
        for (char& c : s) {
            if (c == ';') {
                c = ',';
            }
        }
        std::istringstream iss(s);
        std::string token;
        while (std::getline(iss, token, ',')) {
            token = trimStr(token);
            if (!token.empty()) {
                json jtoken(token);
                result.push_back(resolveNumeric(jtoken, vars));
            }
        }
        return result;
    }
    return {};
}
//=============================================================================
// Codegen feedthrough (latch eligibility)
//=============================================================================
bool
blockHasCodegenFeedthrough(const json& block, const VarMap& vars)
{
    const std::string type = jstr(block, nflow::kType);
    const json params = jval(block, nflow::kParams);
    // Parameter-aware refinements for the stateful blocks whose static
    // directFeedthrough flag is a coarse approximation (see the header note).
    if (type == "tf") {
        // Feeds through only when the tf is not strictly proper (nonzero direct
        // term D); a strictly proper tf reads purely from its continuous state.
        std::vector<double> aLast, cVec, initS;
        double d = 0.0;
        int n = 0;
        buildTfState(
            jval(params, nflow::kNum), jval(params, nflow::kDen), aLast, cVec, d, initS, n, vars);
        return d != 0.0;
    }
    if (type == "pid") {
        // The proportional and derivative terms are direct feedthrough; a pure
        // integral controller (P == 0 and D == 0) reads purely from its state.
        BlockDescriptor bd(block, vars);
        return bd.paramDouble(nflow::kKp, 0.0) != 0.0 || bd.paramDouble(nflow::kKd, 0.0) != 0.0;
    }
    if (type == "stateSpace") {
        // Feeds through iff the D matrix (y = C x + D u) is nonzero.
        BlockDescriptor bd(block, vars);
        for (double v : bd.paramList(nflow::kD)) {
            if (v != 0.0) {
                return true;
            }
        }
        return false;
    }
    return Nelson::NFlow::BlockRegistry::instance().metadataOf(type).directFeedthrough;
}
//=============================================================================
// Execution order (topological sort)
//=============================================================================
std::vector<std::string>
buildExecutionOrder(const std::vector<json>& blocks, const std::vector<json>& connections,
    const std::vector<json>& extraEdges, const std::set<std::string>& latchedIds,
    std::vector<std::string>* unresolvedCycleIds)
{
    std::vector<std::string> ids;
    ids.reserve(blocks.size());
    for (const auto& b : blocks) {
        ids.push_back(jstr(b, nflow::kId));
    }

    std::unordered_map<std::string, int> indeg;
    std::unordered_map<std::string, std::vector<std::string>> adj;
    for (const auto& id : ids) {
        indeg[id] = 0;
        adj[id] = {};
    }

    auto addEdge = [&](const json& conn) {
        std::string from = jstr(conn, nflow::kFrom);
        std::string to = jstr(conn, nflow::kTo);
        if (adj.find(from) == adj.end() || adj.find(to) == adj.end()) {
            return;
        }
        adj[from].push_back(to);
        indeg[to]++;
    };
    for (const auto& c : connections) {
        addEdge(c);
    }
    for (const auto& c : extraEdges) {
        addEdge(c);
    }

    std::queue<std::string> q;
    for (const auto& id : ids) {
        if (indeg[id] == 0) {
            q.push(id);
        }
    }

    std::vector<std::string> order;
    std::set<std::string> visited;
    while (!q.empty()) {
        std::string id = q.front();
        q.pop();
        order.push_back(id);
        visited.insert(id);
        for (const auto& to : adj[id]) {
            indeg[to]--;
            if (indeg[to] == 0) {
                q.push(to);
            }
        }
    }

    // Remaining nodes form one or more cycles. Re-sort the remainder with the
    // latched blocks' outgoing edges dropped (their output does not depend on
    // same-step inputs, so the cycle legitimately breaks there); the latched
    // block itself then lands after its input chain, so its fused
    // output-then-update emission reads fresh inputs. Consumers ordered before
    // a latched block read its pre-loaded output (see the generators).
    if (visited.size() < ids.size() && !latchedIds.empty()) {
        std::unordered_map<std::string, int> indeg2;
        std::unordered_map<std::string, std::vector<std::string>> adj2;
        for (const auto& id : ids) {
            if (visited.find(id) == visited.end()) {
                indeg2[id] = 0;
                adj2[id] = {};
            }
        }
        auto addEdge2 = [&](const json& conn) {
            std::string from = jstr(conn, nflow::kFrom);
            std::string to = jstr(conn, nflow::kTo);
            if (adj2.find(from) == adj2.end() || adj2.find(to) == adj2.end()
                || latchedIds.count(from) > 0) {
                return;
            }
            adj2[from].push_back(to);
            indeg2[to]++;
        };
        for (const auto& c : connections) {
            addEdge2(c);
        }
        for (const auto& c : extraEdges) {
            addEdge2(c);
        }
        std::queue<std::string> q2;
        for (const auto& id : ids) {
            if (indeg2.find(id) != indeg2.end() && indeg2[id] == 0) {
                q2.push(id);
            }
        }
        while (!q2.empty()) {
            std::string id = q2.front();
            q2.pop();
            order.push_back(id);
            visited.insert(id);
            for (const auto& to : adj2[id]) {
                indeg2[to]--;
                if (indeg2[to] == 0) {
                    q2.push(to);
                }
            }
        }
    }

    // Append any remaining (genuine algebraic loop) nodes; report them so
    // callers can reject rather than ship straight-line code for a loop the
    // simulator iterates to a fixed point (SimEngineAlgebraic).
    for (const auto& id : ids) {
        if (visited.find(id) == visited.end()) {
            if (unresolvedCycleIds != nullptr) {
                unresolvedCycleIds->push_back(id);
            }
            order.push_back(id);
        }
    }
    return order;
}
//=============================================================================
// Transfer function discretization
//=============================================================================
DiscreteTf
buildDiscreteTf(const std::vector<double>& numArr, const std::vector<double>& denArr)
{
    auto numRes = normalizePoly(numArr);
    auto denRes = normalizePoly(denArr);
    auto safeDen = denRes.allZero ? std::vector<double> { 1.0 } : denRes.trimmed;
    double a0 = safeDen.empty() ? 1.0 : (safeDen[0] != 0.0 ? safeDen[0] : 1.0);
    auto numNorm = numRes.trimmed.empty() ? std::vector<double> { 0.0 } : numRes.trimmed;
    // Numerator and denominator are polynomials in z in descending powers, so a
    // shorter numerator is padded on the LEFT: [0.2] over [1 -0.8] is
    // 0.2 / (z - 0.8), which answers one step after its input. Read as z^-1
    // lists, the same coefficients describe a system with no delay.
    while (numNorm.size() < safeDen.size()) {
        numNorm.insert(numNorm.begin(), 0.0);
    }
    DiscreteTf result;
    for (double v : numNorm) {
        result.num.push_back(v / a0);
    }
    for (double v : safeDen) {
        result.den.push_back(v / a0);
    }
    return result;
}
//=============================================================================
TustinResult
tustinDiscretize(const std::vector<double>& numArr, const std::vector<double>& denArr, double dt)
{
    auto numRes = normalizePoly(numArr);
    auto denRes = normalizePoly(denArr);
    if (denRes.allZero) {
        return { { 0.0 }, { 1.0 } };
    }

    auto num = numRes.trimmed;
    auto den = denRes.trimmed;
    int n = static_cast<int>(den.size()) - 1;
    int m = static_cast<int>(num.size()) - 1;
    if (m > n) {
        int pad = m - n;
        std::vector<double> padded(pad, 0.0);
        padded.insert(padded.end(), den.begin(), den.end());
        den = padded;
        n = m;
    }
    while (static_cast<int>(num.size()) < n + 1) {
        num.insert(num.begin(), 0.0);
    }

    double k = 2.0 / std::fmax(dt, 1e-6);
    std::vector<double> baseP = { 1.0, -1.0 };
    std::vector<double> baseM = { 1.0, 1.0 };

    std::vector<double> numZ(n + 1, 0.0);
    std::vector<double> denZ(n + 1, 0.0);

    for (int i = 0; i <= n; ++i) {
        int p = n - i;
        double scale = std::pow(k, p);
        auto poly = polyMul(polyPow(baseP, p), polyPow(baseM, n - p));
        numZ = polyAdd(
            numZ, polyScale(poly, (i < static_cast<int>(num.size()) ? num[i] : 0.0) * scale));
        denZ = polyAdd(
            denZ, polyScale(poly, (i < static_cast<int>(den.size()) ? den[i] : 0.0) * scale));
    }

    double a0 = denZ.empty() ? 1.0 : (denZ[0] != 0.0 ? denZ[0] : 1.0);
    TustinResult result;
    for (double v : numZ) {
        result.b.push_back(v / a0);
    }
    for (double v : denZ) {
        result.a.push_back(v / a0);
    }
    return result;
}
//=============================================================================
// Input expressions
//=============================================================================
bool
buildTfState(const json& numJ, const json& denJ, std::vector<double>& A_last_row,
    std::vector<double>& C_vec, double& D, std::vector<double>& initState, int& n,
    const VarMap& vars)
{
    auto toPoly = [&](const json& j) -> std::vector<double> {
        std::vector<double> v;
        if (j.is_array()) {
            for (const auto& el : j) {
                v.push_back(resolveNumeric(el, vars));
            }
        } else {
            v.push_back(resolveNumeric(j, vars));
        }
        return v;
    };
    auto trimLeading = [](std::vector<double> v) -> std::vector<double> {
        size_t i = 0;
        while (i + 1 < v.size() && std::abs(v[i]) < 1e-12) {
            ++i;
        }
        return std::vector<double>(v.begin() + i, v.end());
    };

    std::vector<double> numArr = trimLeading(toPoly(numJ));
    std::vector<double> denArr = trimLeading(toPoly(denJ));

    if (numArr.empty()) {
        numArr = { 0.0 };
    }
    if (denArr.empty()) {
        denArr = { 1.0 };
    }

    bool denAllZero = true;
    for (double v : denArr) {
        if (std::abs(v) > 1e-12) {
            denAllZero = false;
            break;
        }
    }
    if (denAllZero) {
        return false;
    }

    double a0 = denArr[0];
    for (double& v : denArr) {
        v /= a0;
    }
    for (double& v : numArr) {
        v /= a0;
    }

    n = (int)denArr.size() - 1;
    if (n == 0) {
        D = (numArr.empty() ? 0.0 : numArr[0]);
        A_last_row.clear();
        C_vec.clear();
        initState.clear();
        return true;
    }

    while ((int)numArr.size() < n + 1) {
        numArr.insert(numArr.begin(), 0.0);
    }

    std::vector<double> a(denArr.begin() + 1, denArr.end());

    D = numArr[0];
    C_vec.resize(n);
    for (int i = 0; i < n; ++i) {
        double bi = numArr[i + 1];
        double ai = a[i];
        C_vec[n - 1 - i] = bi - ai * D;
    }

    A_last_row.resize(n);
    for (int j = 0; j < n; ++j) {
        A_last_row[j] = -a[n - 1 - j];
    }

    initState.assign(n, 0.0);
    return true;
}
//=============================================================================
double
tfOutput(const std::vector<double>& C, const std::vector<double>& state, double D, double input)
{
    if (C.empty()) {
        return D * input;
    }
    double y = D * input;
    for (int i = 0; i < (int)C.size(); ++i) {
        y += C[i] * state[i];
    }
    return y;
}
//=============================================================================
void
tfDerivative(const std::vector<double>& A_last, const std::vector<double>& state, double input,
    std::vector<double>& xdotOut)
{
    const int n = (int)state.size();
    xdotOut.assign(n, 0.0);
    if (n == 0) {
        return;
    }
    for (int i = 0; i < n - 1; ++i) {
        xdotOut[i] = state[i + 1];
    }
    double last = input;
    for (int j = 0; j < n; ++j) {
        last += A_last[j] * state[j];
    }
    xdotOut[n - 1] = last;
}
//=============================================================================
std::vector<double>
tfRK4(const std::vector<double>& A_last, const std::vector<double>& state, double input, double dt)
{
    const int n = (int)state.size();
    if (n == 0) {
        return {};
    }

    auto derivative = [&](const std::vector<double>& x) -> std::vector<double> {
        std::vector<double> dx;
        tfDerivative(A_last, x, input, dx);
        return dx;
    };
    auto addVec = [&](const std::vector<double>& a, const std::vector<double>& b) {
        std::vector<double> r(n);
        for (int i = 0; i < n; ++i) {
            r[i] = a[i] + b[i];
        }
        return r;
    };
    auto scaleVec = [&](const std::vector<double>& a, double s) {
        std::vector<double> r(n);
        for (int i = 0; i < n; ++i) {
            r[i] = a[i] * s;
        }
        return r;
    };

    const auto k1 = derivative(state);
    const auto k2 = derivative(addVec(state, scaleVec(k1, dt / 2)));
    const auto k3 = derivative(addVec(state, scaleVec(k2, dt / 2)));
    const auto k4 = derivative(addVec(state, scaleVec(k3, dt)));

    std::vector<double> next(n);
    for (int i = 0; i < n; ++i) {
        next[i] = state[i] + (dt / 6.0) * (k1[i] + 2 * k2[i] + 2 * k3[i] + k4[i]);
    }
    return next;
}
//=============================================================================
double
evalDiscreteTf(const std::vector<double>& num, const std::vector<double>& den,
    const std::vector<double>& xHist, const std::vector<double>& yHist)
{
    double y = 0.0;
    for (int i = 0; i < (int)num.size(); ++i) {
        y += num[i] * (i < (int)xHist.size() ? xHist[i] : 0.0);
    }
    for (int i = 1; i < (int)den.size(); ++i) {
        y -= den[i] * (i - 1 < (int)yHist.size() ? yHist[i - 1] : 0.0);
    }
    return y;
}
//=============================================================================
std::string
doubleToStr(double v)
{
    if (std::isnan(v)) {
        return "null";
    }
    if (std::isinf(v)) {
        return (v > 0) ? "1e308" : "-1e308";
    }
    char buffer[32];
    const auto result
        = std::to_chars(buffer, buffer + sizeof(buffer), v, std::chars_format::general, 17);
    return result.ec == std::errc() ? std::string(buffer, result.ptr) : "0";
}
//=============================================================================
std::string
codegenOutVar(const std::string& blockId, int port)
{
    std::string v = "out_" + sanitizeId(blockId);
    if (port > 0) {
        v += "_p" + std::to_string(port);
    }
    return v;
}
//=============================================================================
std::string
getInputExpr(const CodegenInputsMap& inputs, const std::string& blockId, int idx,
    const std::string& fallback)
{
    auto it = inputs.find(blockId);
    if (it == inputs.end()) {
        return fallback;
    }
    const auto& list = it->second;
    if (idx < 0 || idx >= static_cast<int>(list.size()) || list[idx].id.empty()) {
        return fallback;
    }
    return codegenOutVar(list[idx].id, list[idx].port);
}
//=============================================================================
bool
isSimpleExpr(const std::string& expr)
{
    std::string t = trimStr(expr);
    if (t.empty()) {
        return false;
    }
    size_t i = 0;
    if (t[i] == '-') {
        ++i;
    }
    bool hasDigit = false;
    while (i < t.size() && std::isdigit(static_cast<unsigned char>(t[i]))) {
        hasDigit = true;
        ++i;
    }
    if (i < t.size() && t[i] == '.') {
        ++i;
        while (i < t.size() && std::isdigit(static_cast<unsigned char>(t[i]))) {
            hasDigit = true;
            ++i;
        }
    }
    if (hasDigit && i < t.size() && (t[i] == 'e' || t[i] == 'E')) {
        ++i;
        if (i < t.size() && (t[i] == '+' || t[i] == '-')) {
            ++i;
        }
        while (i < t.size() && std::isdigit(static_cast<unsigned char>(t[i]))) {
            ++i;
        }
    }
    if (hasDigit && i == t.size()) {
        return true;
    }
    i = 0;
    if (std::isalpha(static_cast<unsigned char>(t[i])) || t[i] == '_') {
        ++i;
        while (i < t.size() && (std::isalnum(static_cast<unsigned char>(t[i])) || t[i] == '_')) {
            ++i;
        }
        if (i == t.size()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::string
wrapExpr(const std::string& expr)
{
    return isSimpleExpr(expr) ? expr : "(" + expr + ")";
}
//=============================================================================
// Variable map builder
//=============================================================================
VarMap
buildVarMap(const json& variables)
{
    VarMap vars;
    if (!variables.is_object()) {
        return vars;
    }
    for (auto it = variables.begin(); it != variables.end(); ++it) {
        const std::string& key = it.key();
        const json& val = it.value();
        if (val.is_number()) {
            vars[key] = val.get<double>();
        } else if (val.is_string()) {
            char* end = nullptr;
            double d = std::strtod(val.get<std::string>().c_str(), &end);
            if (end && *end == '\0' && !std::isnan(d)) {
                vars[key] = d;
            } else {
                vars[key] = 0.0;
            }
        } else {
            vars[key] = 0.0;
        }
    }
    return vars;
}
//=============================================================================
std::string
expandBlockTemplate(
    const std::string& tpl, const std::function<std::string(const std::string&)>& resolve)
{
    auto isTokenChar = [](char c) {
        // '-' is allowed so a negative numeric default survives, e.g.
        // {param:LowerLimit:-1e308}. Without it the placeholder failed validation
        // and an unsubstituted "{param:...}" token reached the generated source.
        // A literal brace block still starts with "{ " (space), which stays an
        // invalid token and is passed through unchanged.
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
            || c == '_' || c == ':' || c == '.' || c == '-';
    };
    std::string out;
    out.reserve(tpl.size());
    size_t i = 0;
    while (i < tpl.size()) {
        char c = tpl[i];
        if (c == '{') {
            size_t close = tpl.find('}', i + 1);
            if (close != std::string::npos && close > i + 1) {
                std::string token = tpl.substr(i + 1, close - i - 1);
                bool valid = true;
                for (char tc : token) {
                    if (!isTokenChar(tc)) {
                        valid = false;
                        break;
                    }
                }
                if (valid) {
                    out += resolve(token);
                    i = close + 1;
                    continue;
                }
            }
            // literal '{' (e.g. a C block scope in the template): copy and keep
            // scanning so nested placeholders still expand.
        }
        out += c;
        ++i;
    }
    return out;
}
//=============================================================================
std::string
firstNonGeneratableBlockId(const json& diagram, std::string* outType, std::string* outDetail)
{
    if (!diagram.contains(kBlocks) || !diagram[kBlocks].is_array()) {
        return "";
    }
    for (const auto& b : diagram[kBlocks]) {
        if (b.contains(kType) && b[kType].is_string()) {
            const std::string type = b[kType].get<std::string>();
            // physicalIsland: a non-linear island lowers to an embedded-Newton
            // solve with a backward-Euler step in both C and Rust. An island with
            // an unsupported component kind stays rejected (the template emits a
            // typed marker).
            if (type == "physicalIsland") {
                const json params = jval(b, kParams);
                if (!params.is_null()
                    && Nelson::NFlow::physicalIslandAlgebraicGeneratable(params)) {
                    continue;
                }
                if (outType != nullptr) {
                    *outType = type;
                }
                return jstr(b, kId);
            }
            // planarMechanicalIsland: joint-free islands are a pure ODE and
            // lower through the RK4 seam; joints (Lagrange multipliers) and
            // acceleration sensors (cached DERIVATIVE residual) need the
            // runtime DAE solver, so those islands are rejected with the
            // offending component kind named.
            if (type == "planarMechanicalIsland") {
                const json params = jval(b, kParams);
                std::string whyKind;
                if (!params.is_null()
                    && Nelson::NFlow::planarIslandOdeGeneratable(params, &whyKind)) {
                    continue;
                }
                if (outType != nullptr) {
                    *outType = type;
                }
                if (outDetail != nullptr) {
                    *outDetail = whyKind;
                }
                return jstr(b, kId);
            }
            // integrator: an external reset (or an external initial condition)
            // is a discrete event on a continuous state, which the emitters have
            // no lowering for. Emitting an integrator that never resets would be
            // code that does not match the model, so name it instead.
            if (type == "integrator") {
                const json params = jval(b, kParams);
                const std::string extReset = (!params.is_null() && params.contains("ExternalReset")
                                                 && params["ExternalReset"].is_string())
                    ? params["ExternalReset"].get<std::string>()
                    : std::string("none");
                const std::string icSource
                    = (!params.is_null() && params.contains("InitialConditionSource")
                          && params["InitialConditionSource"].is_string())
                    ? params["InitialConditionSource"].get<std::string>()
                    : std::string("internal");
                if (extReset != "none" || icSource == "external") {
                    if (outType != nullptr) {
                        *outType = type;
                    }
                    if (outDetail != nullptr) {
                        *outDetail = (extReset != "none") ? extReset : icSource;
                    }
                    return jstr(b, kId);
                }
                continue;
            }
            // constraint: algebraic DAE state solved by the IDAS runtime;
            // there is no explicit C / Rust lowering for a differential-algebraic
            // system, so reject rather than emit a wrong constant-0 output.
            // modelica: compiled to an FMU by OpenModelica at simulation time
            // (the bridge). Like an FMU it cannot be lowered to standalone
            // C / Rust, so reject rather than emit a wrong constant-0 output.
            // mechanicalTranslationalIsland is not rejected: it is a pure ODE and
            // lowers through the RK4 seam. physicalIsland is handled above.
            if (type == kFmu || type == kFmuMe || type == "nelsonFunction" || type == "constraint"
                || type == "modelica") {
                if (outType != nullptr) {
                    *outType = type;
                }
                return jstr(b, kId);
            }
        }
    }
    return "";
}
//=============================================================================
static std::string
firstConditionalBlockIdImpl(
    const json& diagram, std::string* outType, bool rk4Solver, bool adaptiveSolver)
{
    if (!diagram.contains(kBlocks) || !diagram[kBlocks].is_array()) {
        return "";
    }
    for (const auto& b : diagram[kBlocks]) {
        const std::string type = jstr(b, kType);
        // Function-call routing blocks: a caller-driven schedule has no
        // straight-line C/Rust lowering. The action selectors (if, switchCase)
        // and Merge do: a selector is a chain of comparisons driving one port at
        // a time, and a Merge takes the input whose gate is open.
        // A function-call generator that fires ONCE per step is a subsystem run
        // in the diagram's own order, which straight-line code expresses exactly.
        // More than once per step it is a loop that carries state between calls,
        // and the flattener inlines a body once (a three-call accumulator added
        // 1 per step in generated code where the simulation added 3). So the
        // count decides.
        if (type == "functionCallGenerator" || type == "functionCallSplit") {
            long long calls = 1;
            if (b.contains(kParams) && b[kParams].is_object()) {
                const json& fp = b[kParams];
                if (fp.contains("NumberOfIterations") && fp["NumberOfIterations"].is_number()) {
                    calls = (long long)std::llround(fp["NumberOfIterations"].get<double>());
                }
            }
            if (calls != 1) {
                if (outType != nullptr) {
                    *outType = type;
                }
                return jstr(b, kId);
            }
        }
        if (b.contains(kParams) && b[kParams].is_object()) {
            const json& p = b[kParams];
            // Conditional subsystem: an enable gate lowers at any nesting depth
            // (the flattener wraps the members in the captured gate chain and
            // holds the boundary outputs), provided the block is not a for-each.
            // Under the RK4 seam only enable/action lower: disabled continuous
            // members freeze via rhs masking (their packed derivative is forced
            // to zero) and the boundary holds track/hold once per step.
            if (p.contains("controlPort") && p["controlPort"].is_object()
                && !p["controlPort"].empty()) {
                const std::string ck = p["controlPort"].value("kind", std::string());
                // forIterator stays out: its body runs N times within one step and
                // carries state between iterations, while the flattener inlines a
                // body once. Let through, a 3-iteration accumulator added 1 per
                // step in generated code where the simulation added 3.
                const bool kindLowered = (ck == "enable" || ck == "action" || ck == "trigger"
                    || ck == "enableTrigger" || ck == "reset" || ck == "functionCall");
                // reset lowers under the FIXED-step RK4 seam: the stepper re-reads
                // the packed state after the reporting evaluation, so a re-seeded
                // member restarts the step from its initial value. Under ode45 a
                // step can be rejected and retried, which would replay the
                // re-seed, so that one keeps the reject.
                // A triggered subsystem cannot hold a continuous state (the engine
                // refuses one), so its members are discrete and the RK seam has
                // nothing to freeze. enableTrigger is the same shape.
                const bool kindRk4Safe = (ck == "enable" || ck == "action" || ck == "trigger"
                    || ck == "enableTrigger" || (ck == "reset" && !adaptiveSolver));
                // A for-each is inlined once with its vector wires and the
                // vector-expansion pass makes one scalar replica per element.
                // Each replica carries the gate chain like any other member.
                bool lowerable = kindLowered && (!rk4Solver || kindRk4Safe);
                if (lowerable && p.contains("subsystem") && p["subsystem"].is_object()) {
                    // The gated spec is vetted by the same rules: a gate inside
                    // a gate lowers, the flattener handing the inner members the
                    // whole chain of gates to run under (params.__nfgGates).
                    std::string t2;
                    lowerable = firstConditionalBlockIdImpl(
                        p["subsystem"], &t2, rk4Solver, adaptiveSolver)
                                    .empty();
                }
                if (!lowerable) {
                    if (outType != nullptr) {
                        *outType = "conditional subsystem";
                    }
                    return jstr(b, kId);
                }
                continue; // the spec was vetted by the recursion above
            }
            // Recurse into a nested subsystem spec (its inner blocks carry their
            // own controlPort / action-selection). A control port on the spec
            // ITSELF (at its root or under spec-level params, as a spec exported
            // through the block packager has) is read by the flattener wherever
            // it sits. It is vetted below against the kinds that lower.
            if (p.contains("subsystem") && p["subsystem"].is_object()) {
                const json& spec = p["subsystem"];
                json specCp = spec.contains("controlPort") ? spec["controlPort"] : json();
                if (!(specCp.is_object() && !specCp.empty()) && spec.contains(kParams)
                    && spec[kParams].is_object() && spec[kParams].contains("controlPort")) {
                    specCp = spec[kParams]["controlPort"];
                }
                if (specCp.is_object() && !specCp.empty()) {
                    const std::string sk = specCp.value("kind", std::string());
                    const bool skLowered = (sk == "enable" || sk == "action" || sk == "trigger"
                        || sk == "enableTrigger" || sk == "reset");
                    const bool skRk4Safe = (sk == "enable" || sk == "action" || sk == "trigger"
                        || (sk == "reset" && !adaptiveSolver));
                    if (!skLowered || (rk4Solver && !skRk4Safe)) {
                        if (outType != nullptr) {
                            *outType = "conditional subsystem";
                        }
                        return jstr(b, kId);
                    }
                }
                std::string inner
                    = firstConditionalBlockIdImpl(spec, outType, rk4Solver, adaptiveSolver);
                if (!inner.empty()) {
                    return inner;
                }
            }
        }
    }
    return "";
}
//=============================================================================
std::string
firstConditionalBlockId(const json& diagram, std::string* outType)
{
    const std::string solver
        = (diagram.is_object() && diagram.contains("solver") && diagram["solver"].is_string())
        ? diagram["solver"].get<std::string>()
        : std::string();
    const bool rk4Solver = (solver == "ode4" || solver == "ode45");
    return firstConditionalBlockIdImpl(diagram, outType, rk4Solver, solver == "ode45");
}
//=============================================================================
CodegenWidths
computeCodegenWidths(const json& diagram)
{
    using Nelson::NFlow::Block;
    using Nelson::NFlow::BlockId;
    using Nelson::NFlow::SignalConn;
    using Nelson::NFlow::SignalLayout;

    CodegenWidths out;

    std::vector<Block> blocks;
    std::unordered_map<std::string, BlockId> nameToId;
    if (diagram.contains(kBlocks) && diagram[kBlocks].is_array()) {
        for (const auto& bj : diagram[kBlocks]) {
            Block b;
            b.id = jstr(bj, kId);
            b.type = jstr(bj, kType);
            b.inputs = jint(bj, kInputs, 0);
            b.outputs = jint(bj, kOutputs, 1);
            asJson(b.params) = bj.contains(kParams) ? bj[kParams] : json::object();
            if (!b.id.empty() && !b.type.empty()) {
                b.nid = static_cast<BlockId>(blocks.size());
                nameToId[b.id] = b.nid;
                blocks.push_back(std::move(b));
            }
        }
    }
    Nelson::NFlow::ValMap variables;
    variables["pi"] = M_PI;
    variables["e"] = M_E;
    if (diagram.contains(kVariables) && diagram[kVariables].is_object()) {
        for (const auto& [k, v] : diagram[kVariables].items()) {
            if (v.is_number()) {
                variables[k] = v.get<double>();
            }
        }
    }

    std::vector<SignalConn> conns;
    if (diagram.contains(kConnections) && diagram[kConnections].is_array()) {
        for (const auto& cj : diagram[kConnections]) {
            auto fromIt = nameToId.find(jstr(cj, kFrom));
            auto toIt = nameToId.find(jstr(cj, kTo));
            if (fromIt == nameToId.end() || toIt == nameToId.end()) {
                continue;
            }
            conns.push_back(
                { fromIt->second, jint(cj, kFromIndex, 0), toIt->second, jint(cj, kToIndex, 0) });
        }
    }

    std::unordered_map<std::string, std::string> labelSinks;
    for (const Block& b : blocks) {
        if (b.type == kLabelSink && asJson(b.params).contains(kGotoTag)
            && asJson(b.params)[kGotoTag].is_string()) {
            const std::string name = asJson(b.params)[kGotoTag].get<std::string>();
            if (!name.empty()) {
                labelSinks[name] = b.id;
            }
        }
    }

    // Bus type declarations feed the bus-aware propagation rules.
    Nelson::NFlow::BusRegistry busRegistry;
    {
        std::string busErr;
        if (!Nelson::NFlow::parseBusRegistry(diagram, busRegistry, busErr)) {
            out.ok = false;
            out.error = busErr;
            return out;
        }
    }
    SignalLayout layout = Nelson::NFlow::buildSignalLayout(
        blocks, conns, labelSinks, nameToId, variables, nullptr, &busRegistry);
    if (!layout.ok) {
        out.ok = false;
        out.error = layout.error;
        return out;
    }

    for (int i = 0; i < (int)blocks.size(); ++i) {
        const Block& b = blocks[i];
        const int nOut = std::max(0, b.outputs);
        std::vector<int>& ow = out.outWidths[b.id];
        std::vector<std::string>& ot = out.outTypes[b.id];
        std::vector<std::pair<int, int>>& od = out.outDims[b.id];
        std::vector<std::vector<int>>& odN = out.outDimsN[b.id];
        std::vector<char>& ocx = out.outComplex[b.id];
        std::vector<int>& obus = out.outBusIds[b.id];
        for (int p = 0; p < nOut; ++p) {
            const auto& ps = layout.sigs[layout.portBase[i] + p];
            ow.push_back(ps.width());
            ot.push_back(Nelson::NFlow::sigTypeName(ps.type));
            od.push_back({ ps.rows(), ps.cols() });
            odN.push_back(ps.dims);
            ocx.push_back(ps.isComplex ? 1 : 0);
            obus.push_back(ps.busTypeId);
            out.anyComplex = out.anyComplex || ps.isComplex;
            out.anyBus = out.anyBus || (ps.busTypeId >= 0);
            if (ps.width() > 1) {
                out.allScalar = false;
            }
        }
        // Resolved member offsets for the busSelector outputs (the vector
        // expansion collapses the selector to direct element wires).
        if (b.type == "busSelector") {
            std::vector<int>& offs = out.busSelectorOffsets[b.id];
            int busId = -1;
            if (!layout.inputSigs[i].empty() && layout.inputSigs[i][0] >= 0) {
                busId = layout.sigs[layout.inputSigs[i][0]].busTypeId;
            }
            std::vector<std::string> paths;
            if (asJson(b.params).contains(kSelectedSignals)
                && asJson(b.params)[kSelectedSignals].is_array()) {
                for (const auto& s : asJson(b.params)[kSelectedSignals]) {
                    if (s.is_string()) {
                        paths.push_back(s.get<std::string>());
                    }
                }
            }
            for (int p = 0; p < nOut; ++p) {
                int off = -1;
                if (busId >= 0 && p < (int)paths.size()) {
                    off = busRegistry.memberOffset(busId, paths[(size_t)p]);
                }
                offs.push_back(off);
            }
        }
        // Resolved member regions for busAssignment (replacement port k+1
        // overwrites the packed region of assigned path k; the vector
        // expansion rewires covered elements to the replacement sources).
        if (b.type == "busAssignment") {
            std::vector<std::pair<int, int>>& regs = out.busAssignmentRegions[b.id];
            int busId = -1;
            if (!layout.inputSigs[i].empty() && layout.inputSigs[i][0] >= 0) {
                busId = layout.sigs[layout.inputSigs[i][0]].busTypeId;
            }
            std::vector<std::string> paths;
            for (const char* key : { "AssignedSignals", kSelectedSignals }) {
                if (asJson(b.params).contains(key) && asJson(b.params)[key].is_array()) {
                    for (const auto& s : asJson(b.params)[key]) {
                        if (s.is_string()) {
                            paths.push_back(s.get<std::string>());
                        }
                    }
                    if (!paths.empty()) {
                        break;
                    }
                }
            }
            for (const std::string& path : paths) {
                int off = -1;
                int w = 0;
                if (busId >= 0) {
                    off = busRegistry.memberOffset(busId, path);
                    const Nelson::NFlow::PortSig* ms = busRegistry.resolvePath(busId, path);
                    if (ms) {
                        w = (ms->busTypeId >= 0) ? busRegistry.packedWidth(ms->busTypeId)
                                                 : ms->width();
                    }
                }
                regs.push_back({ off, w });
            }
        }
        std::vector<int>& iw = out.inWidths[b.id];
        std::vector<std::pair<int, int>>& id = out.inDims[b.id];
        std::vector<std::vector<int>>& idN = out.inDimsN[b.id];
        for (size_t p = 0; p < layout.inputSigs[i].size(); ++p) {
            const int gpi = layout.inputSigs[i][p];
            const int w = (gpi >= 0) ? layout.sigs[gpi].width() : 1;
            iw.push_back(w);
            id.push_back(gpi >= 0
                    ? std::pair<int, int> { layout.sigs[gpi].rows(), layout.sigs[gpi].cols() }
                    : std::pair<int, int> { 1, 1 });
            idN.push_back(gpi >= 0 ? layout.sigs[gpi].dims : std::vector<int> { 1 });
            if (w > 1) {
                out.allScalar = false;
            }
        }
    }
    return out;
}
//=============================================================================
BusStructPlan
buildBusStructPlan(const json& diagram)
{
    using Nelson::NFlow::BusRegistry;
    using Nelson::NFlow::BusType;
    BusStructPlan plan;
    auto hasGroups = [&](const char* key) {
        return diagram.is_object() && diagram.contains(key) && diagram[key].is_array()
            && !diagram[key].empty();
    };
    if (!hasGroups(kNonvirtualBusOutputs) && !hasGroups(kNonvirtualBusInputs)) {
        return plan;
    }
    BusRegistry reg;
    std::string err;
    if (!Nelson::NFlow::parseBusRegistry(diagram, reg, err)) {
        return plan;
    }
    // Generated wires are double except the exact 64-bit lane, so struct
    // leaves normalize the same way the port variables do.
    auto leafTypeName = [](const Nelson::NFlow::PortSig& s) -> std::string {
        const std::string tn = Nelson::NFlow::sigTypeName(s.type);
        return (tn == "int64" || tn == "uint64") ? tn : std::string("double");
    };
    // Depth-first leaf walk: fills accesses/leafTypes in packed order.
    std::function<void(int, const std::string&, BusStructOutput&, int)> walkLeaves
        = [&](int id, const std::string& prefix, BusStructOutput& o, int depth) {
              const BusType* t = reg.get(id);
              if (!t || depth > 32) {
                  return;
              }
              for (const auto& m : t->members) {
                  const std::string base = prefix + "." + sanitizeId(m.name);
                  if (m.sig.busTypeId >= 0) {
                      walkLeaves(m.sig.busTypeId, base, o, depth + 1);
                      continue;
                  }
                  const int w = std::max(1, m.sig.width());
                  const std::string tn = leafTypeName(m.sig);
                  for (int k = 0; k < w; ++k) {
                      o.accesses.push_back((w > 1) ? base + "[" + std::to_string(k) + "]" : base);
                      o.leafTypes.push_back(tn);
                  }
              }
          };
    // Post-order type registration so nested struct declarations precede
    // their dependents; deduplicated across outputs sharing a type.
    std::set<int> seenTypes;
    std::function<void(int, int)> addType = [&](int id, int depth) {
        const BusType* t = reg.get(id);
        if (!t || depth > 32 || !seenTypes.insert(id).second) {
            return;
        }
        for (const auto& m : t->members) {
            if (m.sig.busTypeId >= 0) {
                addType(m.sig.busTypeId, depth + 1);
            }
        }
        BusStructType st;
        st.name = sanitizeId(t->name);
        for (const auto& m : t->members) {
            BusStructMember sm;
            sm.name = sanitizeId(m.name);
            if (m.sig.busTypeId >= 0) {
                sm.isBus = true;
                const BusType* nt = reg.get(m.sig.busTypeId);
                sm.busName = sanitizeId(nt ? nt->name : std::string {});
                sm.width = reg.packedWidth(m.sig.busTypeId);
            } else {
                sm.typeName = leafTypeName(m.sig);
                sm.width = std::max(1, m.sig.width());
                sm.dims = m.sig.dims;
            }
            st.members.push_back(std::move(sm));
        }
        plan.types.push_back(std::move(st));
    };
    auto collectGroups = [&](const char* key, std::vector<BusStructOutput>& target) {
        if (!hasGroups(key)) {
            return;
        }
        for (const auto& mj : diagram[key]) {
            const std::string busName = jstr(mj, kBus);
            const int id = reg.idOf(busName);
            const BusType* t = reg.get(id);
            if (!t || !t->nonvirtual) {
                continue;
            }
            BusStructOutput o;
            o.name = sanitizeId(jstr(mj, kName));
            o.busName = sanitizeId(busName);
            if (mj.is_object() && mj.contains(kElements) && mj[kElements].is_array()) {
                for (const auto& e : mj[kElements]) {
                    if (e.is_string()) {
                        o.elements.push_back(sanitizeId(e.get<std::string>()));
                    }
                }
            }
            walkLeaves(id, "", o, 0);
            if (o.name.empty() || o.accesses.empty() || o.accesses.size() != o.elements.size()) {
                continue; // layout mismatch: keep the flattened per-element fields
            }
            addType(id, 0);
            target.push_back(std::move(o));
        }
    };
    collectGroups(kNonvirtualBusOutputs, plan.outputs);
    collectGroups(kNonvirtualBusInputs, plan.inputs);
    return plan;
}
//=============================================================================
std::string
firstVectorSignalBlockId(const json& diagram)
{
    CodegenWidths w = computeCodegenWidths(diagram);
    if (!w.ok || w.allScalar) {
        return "";
    }
    // Report the first block (in diagram order) with a wide port.
    if (diagram.contains(kBlocks) && diagram[kBlocks].is_array()) {
        for (const auto& bj : diagram[kBlocks]) {
            const std::string id = jstr(bj, kId);
            auto oit = w.outWidths.find(id);
            if (oit != w.outWidths.end()) {
                for (int wd : oit->second) {
                    if (wd > 1) {
                        return id;
                    }
                }
            }
            auto iit = w.inWidths.find(id);
            if (iit != w.inWidths.end()) {
                for (int wd : iit->second) {
                    if (wd > 1) {
                        return id;
                    }
                }
            }
        }
    }
    return "";
}
//=============================================================================
} // namespace nflow
//=============================================================================
