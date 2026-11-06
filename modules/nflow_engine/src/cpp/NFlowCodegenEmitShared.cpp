//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenEmitShared.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include "BlockRegistry.hpp"
#include <algorithm>
#include <cctype>
#include <cstring>
#include <set>
//=============================================================================
namespace nflow {
//=============================================================================
using json = nlohmann::json;
//=============================================================================
CodegenCrossScan
buildCodegenCrossScan(const std::vector<json>& blocks, const char* lang,
    const std::unordered_map<std::string, double>& variables)
{
    CodegenCrossScan scan;
    for (const auto& b : blocks) {
        const std::string ctype = jstr(b, nflow::kType);
        Nelson::NFlow::BlockCodegenTemplate xtpl;
        if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(ctype, lang, xtpl)
            || xtpl.notImplemented || !xtpl.crossingCount || !xtpl.emitRk4) {
            continue;
        }
        const std::string rawId = jstr(b, nflow::kId);
        json bp = jval(b, nflow::kParams);
        scan.paramsStore.push_back(bp.is_object() ? bp : json::object());
        Nelson::NFlow::BlockCodegenArgs xa;
        xa.id = sanitizeId(rawId);
        xa.block = &b;
        xa.params = &scan.paramsStore.back();
        xa.variables = &variables;
        const int nsurf = xtpl.crossingCount(xa);
        if (nsurf > 0) {
            scan.list.push_back({ rawId, ctype, &b, scan.total });
            scan.params.push_back(&scan.paramsStore.back());
            scan.total += nsurf;
        } else {
            scan.paramsStore.pop_back();
        }
    }
    return scan;
}
//=============================================================================
std::vector<std::string>
condGatesOf(const json& block)
{
    std::vector<std::string> gates;
    const json gts = jval(jval(block, nflow::kParams), "__nfgGates");
    if (gts.is_array()) {
        for (const auto& g : gts) {
            if (g.is_string()) {
                gates.push_back(sanitizeId(g.get<std::string>()));
            }
        }
    }
    return gates;
}
//=============================================================================
void
appendCondExecutionEdges(const std::vector<json>& blocks, std::vector<json>& extraEdges)
{
    // A guarded member reads out_<gate> without a data wire, so the gate goes before
    // every tagged block. A trigger's end-of-step latch rewrites the gate's edge
    // memory, so it goes after its gate.
    for (const auto& block : blocks) {
        const json gts = jval(jval(block, nflow::kParams), "__nfgGates");
        if (gts.is_array()) {
            for (const auto& g : gts) {
                if (g.is_string()) {
                    extraEdges.push_back({ { nflow::kFrom, g.get<std::string>() },
                        { nflow::kTo, jstr(block, nflow::kId) } });
                }
            }
        }
        if (jstr(block, nflow::kType) == "condLatch") {
            const std::string g = jval(jval(block, nflow::kParams), "gate").is_string()
                ? jval(jval(block, nflow::kParams), "gate").get<std::string>()
                : std::string();
            if (!g.empty()) {
                extraEdges.push_back(
                    { { nflow::kFrom, g }, { nflow::kTo, jstr(block, nflow::kId) } });
            }
        }
        // The gate's edge re-seeds the states of reset members first.
        {
            const json rg = jval(jval(block, nflow::kParams), "__nfgReset");
            if (rg.is_string()) {
                extraEdges.push_back({ { nflow::kFrom, rg.get<std::string>() },
                    { nflow::kTo, jstr(block, nflow::kId) } });
            }
        }
        // A data store read must see what THIS step wrote, so every write goes before
        // every read. Neither has a wire to the other, so without this ordering the
        // read, having no input, lands in the first wave and returns the previous value.
        if (jstr(block, nflow::kType) == "dataStoreRead") {
            const json rn = jval(jval(block, nflow::kParams), "DataStoreName");
            const std::string name = rn.is_string() ? rn.get<std::string>() : std::string("A");
            for (const auto& other : blocks) {
                if (jstr(other, nflow::kType) != "dataStoreWrite") {
                    continue;
                }
                const json wn = jval(jval(other, nflow::kParams), "DataStoreName");
                const std::string wname = wn.is_string() ? wn.get<std::string>() : std::string("A");
                if (wname == name) {
                    extraEdges.push_back({ { nflow::kFrom, jstr(other, nflow::kId) },
                        { nflow::kTo, jstr(block, nflow::kId) } });
                }
            }
        }
    }
}
//=============================================================================
// Dead-code cleanup of the emitted source
//=============================================================================
namespace {
    //=============================================================================
    bool
    isIdentChar(char c)
    {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    }
    //=============================================================================
    std::string
    trimmed(const std::string& s)
    {
        const size_t b = s.find_first_not_of(" \t");
        if (b == std::string::npos) {
            return "";
        }
        const size_t e = s.find_last_not_of(" \t");
        return s.substr(b, e - b + 1);
    }
    //=============================================================================
    // Code part of a generated line: `//` and single-line `/* */` comments
    // removed, string literals kept.
    std::string
    codeOf(const std::string& line)
    {
        std::string r;
        r.reserve(line.size());
        bool inStr = false;
        for (size_t i = 0; i < line.size(); ++i) {
            const char c = line[i];
            const char n = (i + 1 < line.size()) ? line[i + 1] : '\0';
            if (inStr) {
                r += c;
                if (c == '\\' && n != '\0') {
                    r += n;
                    ++i;
                } else if (c == '"') {
                    inStr = false;
                }
                continue;
            }
            if (c == '"') {
                inStr = true;
                r += c;
                continue;
            }
            if (c == '/' && n == '/') {
                break;
            }
            if (c == '/' && n == '*') {
                const size_t end = line.find("*/", i + 2);
                if (end == std::string::npos) {
                    break;
                }
                i = end + 1;
                continue;
            }
            r += c;
        }
        return r;
    }
    //=============================================================================
    // Positions of `name` as a whole identifier, field accesses (`.name`,
    // `->name`) excluded.
    std::vector<size_t>
    identUses(const std::string& code, const std::string& name)
    {
        std::vector<size_t> r;
        size_t pos = code.find(name);
        while (pos != std::string::npos) {
            const size_t after = pos + name.size();
            const bool okLeft = pos == 0
                || (!isIdentChar(code[pos - 1]) && code[pos - 1] != '.'
                    && !(code[pos - 1] == '>' && pos >= 2 && code[pos - 2] == '-'));
            const bool okRight = after >= code.size() || !isIdentChar(code[after]);
            if (okLeft && okRight) {
                r.push_back(pos);
            }
            pos = code.find(name, pos + 1);
        }
        return r;
    }
    //=============================================================================
    // Rust methods of the generated code that neither modify their receiver nor
    // have side effects.
    bool
    isPureMethod(const std::string& name)
    {
        static const std::set<std::string> pure
            = { "abs", "min", "max", "sqrt", "floor", "ceil", "round", "trunc", "signum", "powi",
                  "powf", "clamp", "mul_add", "is_nan", "is_finite", "is_infinite", "wrapping_add",
                  "wrapping_sub", "wrapping_mul", "saturating_add", "saturating_sub",
                  "saturating_mul", "rem_euclid", "div_euclid", "to_bits", "len", "is_empty" };
        return pure.count(name) > 0;
    }
    //=============================================================================
    // True when the use of a local at `pos` modifies it in place: compound or
    // element/field assignment, increment, `&mut`, or a mutating method call.
    bool
    isMutatingUse(const std::string& code, size_t pos, const std::string& name)
    {
        size_t b = pos;
        while (b > 0 && code[b - 1] == ' ') {
            --b;
        }
        if (b >= 3 && code.compare(b - 3, 3, "mut") == 0) {
            return true; // &mut name
        }
        size_t i = pos + name.size();
        auto skipSpaces = [&]() {
            while (i < code.size() && code[i] == ' ') {
                ++i;
            }
        };
        skipSpaces();
        bool method = false;
        std::string member;
        while (i < code.size() && (code[i] == '[' || code[i] == '.')) {
            method = code[i] == '.';
            member.clear();
            if (code[i] == '[') {
                int depth = 0;
                for (; i < code.size(); ++i) {
                    if (code[i] == '[') {
                        ++depth;
                    } else if (code[i] == ']' && --depth == 0) {
                        ++i;
                        break;
                    }
                }
            } else {
                ++i;
                while (i < code.size() && isIdentChar(code[i])) {
                    member += code[i++];
                }
            }
            skipSpaces();
        }
        if (method && i < code.size() && code[i] == '(') {
            // A method call may take `&mut self`, unless it is a known pure one.
            return !isPureMethod(member);
        }
        if (i + 1 < code.size()
            && ((code[i] == '+' && code[i + 1] == '+') || (code[i] == '-' && code[i + 1] == '-'))) {
            return true;
        }
        if (b >= 2
            && ((code[b - 1] == '+' && code[b - 2] == '+')
                || (code[b - 1] == '-' && code[b - 2] == '-'))) {
            return true;
        }
        if (i < code.size() && code[i] == '=') {
            return !(i + 1 < code.size() && (code[i + 1] == '=' || code[i + 1] == '>'));
        }
        static const char* const ops[]
            = { "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=" };
        for (const char* op : ops) {
            if (code.compare(i, std::char_traits<char>::length(op), op) == 0) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    hasImpureCall(const std::string& e, bool rust);
    //=============================================================================
    // Side-effect-free expression: no assignment, increment or `&mut`, and every
    // call is to a known pure function (math library or generated pure helper).
    // Anything else is treated as impure.
    bool
    isPureExpr(const std::string& e, bool rust)
    {
        if (e.find("&mut") != std::string::npos || e.find("++") != std::string::npos
            || e.find("--") != std::string::npos) {
            return false;
        }
        for (size_t i = 0; i < e.size(); ++i) {
            if (e[i] != '=') {
                continue;
            }
            const char p = (i > 0) ? e[i - 1] : ' ';
            const char n = (i + 1 < e.size()) ? e[i + 1] : ' ';
            if (n == '=') {
                ++i;
                continue;
            }
            if (rust && n == '>') {
                continue; // match arm
            }
            if (p == '!' || ((p == '<' || p == '>') && !(i >= 2 && e[i - 2] == p))) {
                continue; // != <= >=
            }
            return false;
        }
        return !hasImpureCall(e, rust);
    }
    //=============================================================================
    // True when `e` calls anything but a known pure function (math library,
    // generated pure helper, pure method) or a macro.
    bool
    hasImpureCall(const std::string& e, bool rust)
    {
        static const std::set<std::string> pureC = { "sin", "cos", "tan", "asin", "acos", "atan",
            "atan2", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "exp", "exp2", "expm1",
            "log", "log10", "log2", "log1p", "sqrt", "cbrt", "pow", "fabs", "floor", "ceil",
            "round", "trunc", "fmod", "fmin", "fmax", "hypot", "copysign", "rint", "nearbyint",
            "remainder", "erf", "erfc", "tgamma", "lgamma", "fdim", "fma", "lround", "llround",
            "lrint", "isnan", "isinf", "isfinite", "signbit", "sizeof" };
        static const std::set<std::string> pureHelpers
            = { "tf_out", "nflow_quant_i", "nflow_from_workspace" };
        static const std::set<std::string> keywords
            = { "if", "while", "for", "match", "return", "else", "as", "in" };
        for (size_t i = 0; i < e.size(); ++i) {
            if (e[i] != '(') {
                continue;
            }
            size_t j = i;
            while (j > 0 && e[j - 1] == ' ') {
                --j;
            }
            if (j > 0 && e[j - 1] == '!') {
                if (j >= 2 && isIdentChar(e[j - 2])) {
                    return true; // macro call
                }
                continue; // logical not
            }
            size_t k = j;
            while (k > 0 && (isIdentChar(e[k - 1]) || e[k - 1] == ':')) {
                --k;
            }
            while (k < j && e[k] == ':') {
                ++k; // a ternary `:`, not a path
            }
            const std::string token = e.substr(k, j - k);
            if (token.empty() || keywords.count(token)) {
                continue; // grouping, cast or control flow
            }
            const bool method = k > 0 && e[k - 1] == '.';
            if (method) {
                if (isPureMethod(token)) {
                    continue;
                }
                return true;
            }
            if (rust
                && (token.rfind("libm::", 0) == 0 || token.rfind("f64::", 0) == 0
                    || token.rfind("i64::", 0) == 0 || token.rfind("u64::", 0) == 0)) {
                continue;
            }
            if (!rust && pureC.count(token)) {
                continue;
            }
            if (pureHelpers.count(token) || token.rfind("nflow_i64_", 0) == 0
                || token.rfind("nflow_u64_", 0) == 0 || token.rfind("nflow_d_to_", 0) == 0) {
                continue;
            }
            return true;
        }
        return false;
    }
    //=============================================================================
    // `text` with comments and string-literal contents blanked out (same length,
    // newlines kept), so offsets in the mask address the original text.
    std::string
    maskText(const std::string& text)
    {
        std::string m = text;
        bool inStr = false;
        for (size_t i = 0; i < text.size(); ++i) {
            const char c = text[i];
            const char n = (i + 1 < text.size()) ? text[i + 1] : '\0';
            if (inStr) {
                if (c == '\\' && n != '\0') {
                    m[i] = ' ';
                    m[i + 1] = (n == '\n') ? '\n' : ' ';
                    ++i;
                } else if (c == '"') {
                    inStr = false;
                } else if (c != '\n') {
                    m[i] = ' ';
                }
                continue;
            }
            if (c == '"') {
                inStr = true;
            } else if (c == '/' && n == '/') {
                while (i < text.size() && text[i] != '\n') {
                    m[i++] = ' ';
                }
            } else if (c == '/' && n == '*') {
                size_t e = text.find("*/", i + 2);
                e = (e == std::string::npos) ? text.size() : e + 2;
                for (; i < e; ++i) {
                    if (text[i] != '\n') {
                        m[i] = ' ';
                    }
                }
                --i;
            }
        }
        return m;
    }
    //=============================================================================
    enum class ItemKind
    {
        Open, // block header up to and including `{`
        Close, // `}`
        Stmt // statement (up to `;`, or up to the `}` closing its block)
    };
    enum class BlockKind
    {
        Plain, // `{ ... }`: always runs
        Cond, // if / else / match / ...
        Loop // for / while / loop / do
    };
    struct Item
    {
        ItemKind kind;
        size_t begin; // [begin, end) in the body text
        size_t end;
        BlockKind block; // Open only
        std::vector<BlockKind> ctx; // enclosing blocks, outermost first
    };
    //=============================================================================
    // True when `prefix` (text before a `{` at bracket level 0) is the start of an
    // expression statement (`x = if .. {`, `let y = {`, `T a[] = {`), i.e. it
    // holds an assignment at bracket level 0.
    bool
    isExpressionPrefix(const std::string& prefix)
    {
        int depth = 0;
        for (size_t i = 0; i < prefix.size(); ++i) {
            const char c = prefix[i];
            if (c == '(' || c == '[') {
                ++depth;
            } else if (c == ')' || c == ']') {
                --depth;
            } else if (c == '=' && depth == 0) {
                const char p = (i > 0) ? prefix[i - 1] : ' ';
                const char n = (i + 1 < prefix.size()) ? prefix[i + 1] : ' ';
                if (n == '=' || n == '>') {
                    ++i;
                    continue;
                }
                if (p == '!' || p == '<' || p == '>' || p == '=') {
                    continue;
                }
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    BlockKind
    blockKindOf(const std::string& header)
    {
        const std::string t = trimmed(header);
        if (t.empty()) {
            return BlockKind::Plain;
        }
        for (const char* kw : { "for", "while", "loop", "do" }) {
            const size_t n = std::char_traits<char>::length(kw);
            if (t.compare(0, n, kw) == 0 && (t.size() == n || !isIdentChar(t[n]))) {
                return BlockKind::Loop;
            }
        }
        return BlockKind::Cond;
    }
    //=============================================================================
    std::vector<Item>
    parseItems(const std::string& m)
    {
        std::vector<Item> items;
        std::vector<BlockKind> ctx;
        size_t p = 0;
        while (true) {
            while (p < m.size() && std::isspace(static_cast<unsigned char>(m[p]))) {
                ++p;
            }
            if (p >= m.size()) {
                break;
            }
            if (m[p] == '}') {
                items.push_back({ ItemKind::Close, p, p + 1, BlockKind::Plain, ctx });
                if (!ctx.empty()) {
                    ctx.pop_back();
                }
                ++p;
                continue;
            }
            int depth = 0; // ( and [
            int braces = 0;
            size_t q = p;
            bool done = false;
            for (; q < m.size(); ++q) {
                const char c = m[q];
                if (c == '(' || c == '[') {
                    ++depth;
                } else if (c == ')' || c == ']') {
                    --depth;
                } else if (c == '{') {
                    if (depth == 0 && braces == 0 && !isExpressionPrefix(m.substr(p, q - p))) {
                        const BlockKind k = blockKindOf(m.substr(p, q - p));
                        items.push_back({ ItemKind::Open, p, q + 1, k, ctx });
                        ctx.push_back(k);
                        p = q + 1;
                        done = true;
                        break;
                    }
                    ++braces;
                } else if (c == '}') {
                    if (braces == 0) {
                        // Last expression of a block, or a match arm: no `;`.
                        items.push_back({ ItemKind::Stmt, p, q, BlockKind::Plain, ctx });
                        p = q;
                        done = true;
                        break;
                    }
                    --braces;
                } else if (c == ';' && depth == 0 && braces == 0) {
                    items.push_back({ ItemKind::Stmt, p, q + 1, BlockKind::Plain, ctx });
                    p = q + 1;
                    done = true;
                    break;
                }
            }
            if (!done) {
                items.push_back({ ItemKind::Stmt, p, m.size(), BlockKind::Plain, ctx });
                break;
            }
        }
        return items;
    }
    //=============================================================================
    // A local declaration statement.
    struct LocalDecl
    {
        std::string name;
        bool hasInit = false;
        bool hasType = false; // Rust: explicit `: T`
        bool isConst = false; // C: `const` qualifier
        bool isMut = false; // Rust: `let mut`
        size_t eqOffset = std::string::npos; // offset of `=` in the statement
    };
    //=============================================================================
    bool
    parseLocalDecl(const std::string& stmt, bool rust, LocalDecl& d)
    {
        std::string t = trimmed(stmt);
        if (t.empty() || t.back() != ';') {
            return false;
        }
        t.pop_back();
        const size_t lead = stmt.find_first_not_of(" \t\r\n");
        if (rust) {
            if (t.rfind("let ", 0) != 0) {
                return false;
            }
            std::string rest = trimmed(t.substr(4));
            d.isMut = rest.rfind("mut ", 0) == 0;
            if (d.isMut) {
                rest = trimmed(rest.substr(4));
            }
            size_t k = 0;
            while (k < rest.size() && isIdentChar(rest[k])) {
                ++k;
            }
            d.name = rest.substr(0, k);
            if (d.name.empty() || d.name == "_") {
                return false;
            }
            const std::string tail = trimmed(rest.substr(k));
            if (!(tail.empty() || tail[0] == ':' || tail[0] == '=')) {
                return false;
            }
            d.hasType = !tail.empty() && tail[0] == ':';
            const size_t eq = t.find('=');
            d.hasInit = eq != std::string::npos;
            d.eqOffset = d.hasInit ? lead + eq : std::string::npos;
            return true;
        }
        const size_t eq = t.find('=');
        std::string lhs = trimmed(eq == std::string::npos ? t : t.substr(0, eq));
        if (!lhs.empty() && lhs.back() == ']') {
            lhs = trimmed(lhs.substr(0, lhs.find('['))); // array declarator
        }
        if (lhs.empty() || lhs.find_first_of("()[]*&,.-") != std::string::npos) {
            return false;
        }
        std::vector<std::string> words;
        std::string w;
        for (char c : lhs + " ") {
            if (std::isspace(static_cast<unsigned char>(c))) {
                if (!w.empty()) {
                    words.push_back(w);
                }
                w.clear();
            } else {
                w += c;
            }
        }
        if (words.size() < 2) {
            return false;
        }
        static const std::set<std::string> types = { "double", "float", "int", "long", "char",
            "int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t", "uint32_t",
            "uint64_t", "size_t", "unsigned", "signed", "short", "const", "bool", "static" };
        for (size_t k = 0; k + 1 < words.size(); ++k) {
            if (!types.count(words[k])) {
                return false;
            }
        }
        d.name = words.back();
        d.isConst = std::find(words.begin(), words.end(), "const") != words.end();
        d.hasInit = eq != std::string::npos;
        d.hasType = true;
        d.eqOffset = d.hasInit ? lead + eq : std::string::npos;
        return true;
    }
    //=============================================================================
    enum class UseKind
    {
        Read,
        Write
    };
    struct Use
    {
        size_t item;
        UseKind kind;
        bool uncond; // runs whenever the declaration's block runs
        bool inLoop; // inside a loop opened in the declaration's block
    };
    //=============================================================================
    // Offset of `=` when statement `s` is `name = ...;`, else npos.
    size_t
    assignmentOf(const std::string& s, const std::string& name)
    {
        const size_t b = s.find_first_not_of(" \t\r\n");
        if (b == std::string::npos || s.compare(b, name.size(), name) != 0) {
            return std::string::npos;
        }
        size_t i = b + name.size();
        if (i < s.size() && isIdentChar(s[i])) {
            return std::string::npos;
        }
        while (i < s.size() && (s[i] == ' ' || s[i] == '\t')) {
            ++i;
        }
        if (i + 1 < s.size() && s[i] == '=' && s[i + 1] != '=' && s[i + 1] != '>') {
            return i;
        }
        return std::string::npos;
    }
    //=============================================================================
    // Uses of the local declared by item `d` within its scope.
    std::vector<Use>
    collectUses(const std::vector<Item>& items, const std::string& m, size_t d,
        const std::string& name, bool& mutated)
    {
        std::vector<Use> uses;
        mutated = false;
        const size_t level = items[d].ctx.size();
        for (size_t k = d + 1; k < items.size(); ++k) {
            const Item& it = items[k];
            if (it.kind == ItemKind::Close && it.ctx.size() == level) {
                break; // end of the declaration's block
            }
            bool uncond = true;
            bool inLoop = false;
            for (size_t c = level; c < it.ctx.size(); ++c) {
                uncond = uncond && it.ctx[c] == BlockKind::Plain;
                inLoop = inLoop || it.ctx[c] == BlockKind::Loop;
            }
            if (it.kind == ItemKind::Open && it.block == BlockKind::Loop) {
                inLoop = true; // loop header (condition, increment)
            }
            const std::string s = m.substr(it.begin, it.end - it.begin);
            size_t from = 0;
            const size_t eq
                = (it.kind == ItemKind::Stmt) ? assignmentOf(s, name) : std::string::npos;
            if (eq != std::string::npos) {
                from = eq + 1;
            }
            for (size_t p : identUses(s, name)) {
                if (p < from) {
                    continue;
                }
                if (isMutatingUse(s, p, name)) {
                    mutated = true;
                }
                uses.push_back({ k, UseKind::Read, false, inLoop });
            }
            if (eq != std::string::npos) {
                uses.push_back({ k, UseKind::Write, uncond, inLoop });
            }
        }
        return uses;
    }
    //=============================================================================
    // True when the value stored by write `w` of `uses` (or by the initializer
    // when `w` is -1) can never be read: no read before the end of the scope or
    // before an unconditional overwrite.
    bool
    isDeadStore(const std::vector<Use>& uses, long w)
    {
        if (w >= 0 && uses[static_cast<size_t>(w)].inLoop) {
            return false;
        }
        for (size_t k = static_cast<size_t>(w + 1); k < uses.size(); ++k) {
            if (uses[k].kind == UseKind::Read) {
                return false;
            }
            if (uses[k].uncond && !uses[k].inLoop) {
                return true;
            }
        }
        return true;
    }
    //=============================================================================
    struct TextEdit
    {
        size_t pos;
        size_t len;
        std::string text;
    };
    //=============================================================================
    // Edit removing [b, e); the whole line when nothing but blanks (and comments)
    // would be left on it.
    TextEdit
    removalEdit(const std::string& m, size_t b, size_t e)
    {
        size_t ls = b;
        while (ls > 0 && m[ls - 1] != '\n') {
            --ls;
        }
        size_t le = e;
        while (le < m.size() && m[le] != '\n') {
            ++le;
        }
        const bool blankBefore = m.find_first_not_of(" \t\r", ls) >= b;
        const bool blankAfter = m.find_first_not_of(" \t\r", e) >= le;
        if (blankBefore && blankAfter) {
            return { ls, std::min(le + 1, m.size()) - ls, "" };
        }
        return { b, e - b, "" };
    }
    //=============================================================================
    // Edits discarding the value of write statement `it` (`name = rhs;`): the
    // statement goes when `rhs` is pure, else only its value is dropped
    // (`let _ = rhs;` / `(void)(rhs);`). False when it cannot be rewritten.
    bool
    discardWrite(const std::string& m, const Item& it, const std::string& name, bool rust,
        std::vector<TextEdit>& edits)
    {
        const std::string s = m.substr(it.begin, it.end - it.begin);
        const size_t eq = assignmentOf(s, name);
        if (eq == std::string::npos || s.empty() || s.back() != ';') {
            return false;
        }
        const std::string rhs = s.substr(eq + 1, s.size() - eq - 2);
        if (isPureExpr(rhs, rust)) {
            edits.push_back(removalEdit(m, it.begin, it.end));
            return true;
        }
        const size_t at = it.begin + s.find(name);
        const size_t valueAt = it.begin + eq + 1;
        if (rust) {
            edits.push_back({ at, valueAt - at, "let _ =" });
        } else {
            size_t v = valueAt;
            while (v < it.end && m[v] == ' ') {
                ++v;
            }
            edits.push_back({ at, v - at, "(void)(" });
            edits.push_back({ it.end - 1, 0, ")" });
        }
        return true;
    }
    //=============================================================================
    std::string
    applyEdits(const std::string& text, std::vector<TextEdit> edits)
    {
        std::sort(edits.begin(), edits.end(),
            [](const TextEdit& a, const TextEdit& b) { return a.pos > b.pos; });
        std::string r = text;
        size_t limit = r.size();
        for (const auto& e : edits) {
            if (e.pos + e.len > limit) {
                continue; // overlaps an edit already applied
            }
            r.replace(e.pos, e.len, e.text);
            limit = e.pos;
        }
        return r;
    }
    //=============================================================================
    // Base identifier of the lvalue ending just before offset `op` in `s` (`a`,
    // `a[i]`, `a.b`, `a->b`); `viaField` is set when it goes through a field or a
    // dereference, i.e. may write outside the local itself.
    std::string
    lvalueBase(const std::string& s, size_t op, bool& viaField)
    {
        viaField = false;
        size_t i = op;
        auto skipBack = [&]() {
            while (i > 0 && std::isspace(static_cast<unsigned char>(s[i - 1]))) {
                --i;
            }
        };
        skipBack();
        std::string base;
        while (i > 0) {
            if (s[i - 1] == ']') {
                int depth = 0;
                while (i > 0) {
                    --i;
                    if (s[i] == ']') {
                        ++depth;
                    } else if (s[i] == '[' && --depth == 0) {
                        break;
                    }
                }
                skipBack();
                continue;
            }
            if (!isIdentChar(s[i - 1])) {
                break;
            }
            const size_t e = i;
            while (i > 0 && isIdentChar(s[i - 1])) {
                --i;
            }
            base = s.substr(i, e - i);
            skipBack();
            if (i > 0 && s[i - 1] == '.') {
                viaField = true;
                --i;
                skipBack();
                continue;
            }
            if (i > 1 && s[i - 1] == '>' && s[i - 2] == '-') {
                viaField = true;
                i -= 2;
                skipBack();
                continue;
            }
            break;
        }
        if (i > 0 && s[i - 1] == '*') {
            viaField = true; // *p = ...
        }
        return base;
    }
    //=============================================================================
    // Locals a statement (or block header) writes, and whether it has an effect
    // observable outside the function's locals: a write through a parameter, a
    // field or a pointer, a call that is not known pure, or a jump.
    struct StmtEffects
    {
        std::set<std::string> defs;
        bool effect = false;
    };
    //=============================================================================
    StmtEffects
    stmtEffects(const std::string& stmt, bool rust, const std::set<std::string>& locals)
    {
        StmtEffects fx;
        const std::string t = trimmed(stmt);
        // An explicit discard `(void)x` / `let _ = x` marks x as observed: a visualization sink pulls its input into the graph without a later read, so keep its feeding chain.
        if ((!rust && t.rfind("(void)", 0) == 0) || (rust && t.rfind("let _ =", 0) == 0)) {
            fx.effect = true;
        }
        for (const char* kw : { "return", "break", "continue", "goto" }) {
            fx.effect = fx.effect || !identUses(t, kw).empty();
        }
        fx.effect = fx.effect || hasImpureCall(t, rust);
        auto target = [&](const std::string& base, bool viaField) {
            if (!base.empty() && !viaField && locals.count(base)) {
                fx.defs.insert(base);
            } else {
                fx.effect = true;
            }
        };
        // A declaration defines its name; its `=` is not an assignment target.
        size_t declEq = std::string::npos;
        LocalDecl decl;
        if (parseLocalDecl(t, rust, decl)) {
            fx.defs.insert(decl.name);
            declEq = decl.hasInit ? decl.eqOffset : std::string::npos;
        }
        for (size_t i = 0; i < t.size(); ++i) {
            const char c = t[i];
            const char n = (i + 1 < t.size()) ? t[i + 1] : '\0';
            const char p = (i > 0) ? t[i - 1] : '\0';
            if (c == '=') {
                if (n == '=') {
                    ++i;
                    continue;
                }
                if (n == '>' || p == '!' || p == '=' || i == declEq) {
                    continue;
                }
                const bool shift = (p == '<' || p == '>') && i >= 2 && t[i - 2] == p;
                if ((p == '<' || p == '>') && !shift) {
                    continue; // <= >=
                }
                size_t opStart = i;
                if (shift) {
                    opStart = i - 2;
                } else if (p != '\0' && std::strchr("+-*/%&|^", p) != nullptr) {
                    opStart = i - 1;
                }
                bool viaField = false;
                const std::string base = lvalueBase(t, opStart, viaField);
                target(base, viaField);
            } else if ((c == '+' && n == '+') || (c == '-' && n == '-')) {
                size_t b = i;
                while (b > 0 && t[b - 1] == ' ') {
                    --b;
                }
                bool viaField = false;
                std::string base;
                if (b > 0 && (isIdentChar(t[b - 1]) || t[b - 1] == ']')) {
                    base = lvalueBase(t, i, viaField); // postfix
                } else {
                    size_t a = i + 2;
                    while (a < t.size() && t[a] == ' ') {
                        ++a;
                    }
                    size_t e = a;
                    while (e < t.size() && isIdentChar(t[e])) {
                        ++e;
                    }
                    base = t.substr(a, e - a); // prefix
                    viaField = e < t.size()
                        && (t[e] == '.' || t[e] == '[' || (t[e] == '-' && e + 1 < t.size()));
                }
                target(base, viaField);
                ++i;
            } else if (c == '&') {
                if (n == '&') {
                    ++i;
                    continue;
                }
                size_t a = i + 1;
                while (a < t.size() && t[a] == ' ') {
                    ++a;
                }
                if (rust) {
                    if (t.compare(a, 4, "mut ") != 0) {
                        continue; // shared borrow
                    }
                    a += 4;
                    while (a < t.size() && t[a] == ' ') {
                        ++a;
                    }
                } else if (i > 0) {
                    size_t b = i;
                    while (b > 0 && t[b - 1] == ' ') {
                        --b;
                    }
                    if (b > 0 && (isIdentChar(t[b - 1]) || t[b - 1] == ')' || t[b - 1] == ']')) {
                        continue; // bitwise and
                    }
                }
                size_t e = a;
                while (e < t.size() && isIdentChar(t[e])) {
                    ++e;
                }
                if (e > a) {
                    // The address escapes: whatever receives it may write anywhere.
                    target(t.substr(a, e - a), true);
                }
            }
        }
        return fx;
    }
    //=============================================================================
    // Variable-level dead-code elimination over a function body: starting from
    // the statements with an observable effect, keep the locals they read (and
    // the control headers they depend on), transitively; remove every other
    // statement, declaration and block. True when something was removed.
    bool
    eliminateDeadCode(std::string& body, bool rust, const std::set<std::string>& params)
    {
        const std::string m = maskText(body);
        const std::vector<Item> items = parseItems(m);
        const size_t n = items.size();
        std::vector<std::string> text(n);
        for (size_t k = 0; k < n; ++k) {
            text[k] = m.substr(items[k].begin, items[k].end - items[k].begin);
        }
        // Locals: declared names (statements and C `for (T i = ...)` headers),
        // minus mutable C statics (state kept across calls) and parameter names.
        std::set<std::string> locals;
        std::set<std::string> pinned;
        for (size_t k = 0; k < n; ++k) {
            LocalDecl decl;
            std::string t = trimmed(text[k]);
            if (items[k].kind == ItemKind::Open && t.rfind("for", 0) == 0 && !rust) {
                const size_t open = t.find('(');
                const size_t semi = t.find(';');
                if (open != std::string::npos && semi != std::string::npos && semi > open) {
                    t = t.substr(open + 1, semi - open);
                } else {
                    continue;
                }
            } else if (items[k].kind != ItemKind::Stmt) {
                continue;
            }
            if (!parseLocalDecl(t, rust, decl)) {
                continue;
            }
            locals.insert(decl.name);
            if (!rust && t.rfind("static ", 0) == 0 && t.find("const") == std::string::npos) {
                pinned.insert(decl.name);
            }
        }
        for (const auto& p : params) {
            pinned.insert(p);
        }
        for (const auto& p : pinned) {
            locals.erase(p);
        }
        if (locals.empty()) {
            return false;
        }
        // Per item: locals written, locals read, effect, enclosing headers.
        std::vector<StmtEffects> fx(n);
        std::vector<std::set<std::string>> reads(n);
        std::vector<long> parent(n, -1);
        std::vector<long> closeOf(n, -1);
        std::vector<long> chainHead(n, -1);
        {
            std::vector<size_t> stack;
            long lastClosedOpen = -1;
            for (size_t k = 0; k < n; ++k) {
                parent[k] = stack.empty() ? -1 : static_cast<long>(stack.back());
                if (items[k].kind == ItemKind::Close) {
                    if (!stack.empty()) {
                        closeOf[stack.back()] = static_cast<long>(k);
                        lastClosedOpen = static_cast<long>(stack.back());
                        stack.pop_back();
                    }
                    continue;
                }
                const std::string t = trimmed(text[k]);
                if (items[k].kind == ItemKind::Open) {
                    // `else` continues the chain of the block closed just before.
                    const bool isElse = t.rfind("else", 0) == 0;
                    chainHead[k] = (isElse && lastClosedOpen >= 0 && k > 0
                                       && items[k - 1].kind == ItemKind::Close)
                        ? chainHead[static_cast<size_t>(lastClosedOpen)]
                        : static_cast<long>(k);
                    stack.push_back(k);
                }
                fx[k] = stmtEffects(t, rust, locals);
                if (items[k].kind == ItemKind::Stmt && (t.empty() || t.back() != ';')) {
                    fx[k].effect = true; // block tail expression or match arm
                }
                for (const auto& v : locals) {
                    if (!identUses(t, v).empty()) {
                        reads[k].insert(v);
                    }
                }
            }
        }
        std::vector<bool> live(n, false);
        std::set<std::string> liveVars;
        for (size_t k = 0; k < n; ++k) {
            live[k] = items[k].kind != ItemKind::Close && fx[k].effect;
        }
        bool changed = true;
        while (changed) {
            changed = false;
            for (size_t k = 0; k < n; ++k) {
                if (!live[k] && items[k].kind != ItemKind::Close) {
                    for (const auto& d : fx[k].defs) {
                        if (liveVars.count(d)) {
                            live[k] = true;
                            changed = true;
                            break;
                        }
                    }
                }
                if (!live[k]) {
                    continue;
                }
                for (const auto& v : reads[k]) {
                    changed = liveVars.insert(v).second || changed;
                }
                if (parent[k] >= 0 && !live[static_cast<size_t>(parent[k])]) {
                    live[static_cast<size_t>(parent[k])] = true;
                    changed = true;
                }
                if (items[k].kind == ItemKind::Open) {
                    // A live branch keeps every header of its if / else chain.
                    for (size_t j = 0; j < n; ++j) {
                        if (chainHead[j] == chainHead[k] && !live[j]) {
                            live[j] = true;
                            changed = true;
                        }
                    }
                }
            }
        }
        std::vector<TextEdit> edits;
        for (size_t k = 0; k < n; ++k) {
            if (live[k] || items[k].kind == ItemKind::Close) {
                continue;
            }
            if (items[k].kind == ItemKind::Stmt) {
                edits.push_back(removalEdit(m, items[k].begin, items[k].end));
                continue;
            }
            const long c = closeOf[k];
            if (c < 0) {
                return false;
            }
            edits.push_back(removalEdit(m, items[k].begin, items[static_cast<size_t>(c)].end));
            k = static_cast<size_t>(c); // the block's items go with it
        }
        if (edits.empty()) {
            return false;
        }
        body = applyEdits(body, edits);
        return true;
    }
    //=============================================================================
    // One cleanup round over a function body; true when something changed.
    bool
    pruneBodyOnce(std::string& body, bool rust, const std::set<std::string>& params)
    {
        if (eliminateDeadCode(body, rust, params)) {
            return true;
        }
        const std::string m = maskText(body);
        const std::vector<Item> items = parseItems(m);
        // An empty plain block `{ }` is dropped.
        for (size_t k = 0; k + 1 < items.size(); ++k) {
            if (items[k].kind == ItemKind::Open && items[k].block == BlockKind::Plain
                && items[k + 1].kind == ItemKind::Close) {
                body = applyEdits(body,
                    { removalEdit(m, items[k].begin, items[k].end),
                        removalEdit(m, items[k + 1].begin, items[k + 1].end) });
                return true;
            }
        }
        for (size_t d = 0; d < items.size(); ++d) {
            const Item& di = items[d];
            LocalDecl decl;
            if (di.kind != ItemKind::Stmt
                || !parseLocalDecl(m.substr(di.begin, di.end - di.begin), rust, decl)) {
                continue;
            }
            bool mutated = false;
            const std::vector<Use> uses = collectUses(items, m, d, decl.name, mutated);
            size_t reads = 0;
            for (const auto& u : uses) {
                reads += (u.kind == UseKind::Read) ? 1 : 0;
            }
            std::vector<TextEdit> edits;
            if (reads == 0) {
                // Never read: the declaration goes and every write is discarded.
                bool ok = true;
                for (const auto& u : uses) {
                    ok = ok
                        && (u.kind != UseKind::Write
                            || discardWrite(m, items[u.item], decl.name, rust, edits));
                }
                const std::string init = decl.hasInit
                    ? m.substr(di.begin + decl.eqOffset + 1, di.end - di.begin - decl.eqOffset - 2)
                    : std::string();
                if (!ok || (decl.hasInit && !isPureExpr(init, rust))) {
                    continue;
                }
                edits.push_back(removalEdit(m, di.begin, di.end));
            } else {
                for (size_t k = 0; k < uses.size(); ++k) {
                    if (uses[k].kind == UseKind::Write && isDeadStore(uses, static_cast<long>(k))) {
                        discardWrite(m, items[uses[k].item], decl.name, rust, edits);
                    }
                }
                // Initializer overwritten before any read: declare without it.
                if (decl.hasInit && decl.hasType && !decl.isConst && isDeadStore(uses, -1)) {
                    const std::string init = m.substr(
                        di.begin + decl.eqOffset + 1, di.end - di.begin - decl.eqOffset - 2);
                    if (isPureExpr(init, rust)) {
                        size_t cut = di.begin + decl.eqOffset;
                        while (cut > di.begin && m[cut - 1] == ' ') {
                            --cut;
                        }
                        edits.push_back({ cut, di.end - 1 - cut, "" });
                    }
                }
            }
            if (!edits.empty()) {
                body = applyEdits(body, edits);
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    // Rust: drop `mut` from locals never modified after their initialization
    // (or assigned exactly once when declared without one).
    void
    fixRustMut(std::string& body)
    {
        const std::string m = maskText(body);
        const std::vector<Item> items = parseItems(m);
        std::vector<TextEdit> edits;
        for (size_t d = 0; d < items.size(); ++d) {
            const Item& di = items[d];
            LocalDecl decl;
            if (di.kind != ItemKind::Stmt
                || !parseLocalDecl(m.substr(di.begin, di.end - di.begin), true, decl)
                || !decl.isMut) {
                continue;
            }
            bool mutated = false;
            const std::vector<Use> uses = collectUses(items, m, d, decl.name, mutated);
            size_t writes = 0;
            bool loopWrite = false;
            for (const auto& u : uses) {
                if (u.kind == UseKind::Write) {
                    ++writes;
                    loopWrite = loopWrite || u.inLoop;
                }
            }
            if (mutated || loopWrite || (decl.hasInit ? writes >= 1 : writes >= 2)) {
                continue;
            }
            const size_t at = m.find("mut ", di.begin);
            if (at != std::string::npos && at < di.end) {
                edits.push_back({ at, 4, "" });
            }
        }
        body = applyEdits(body, edits);
    }
    //=============================================================================
    // Parameter names of a function header `name(p1, p2, ...)`.
    std::vector<std::string>
    paramNames(const std::string& header, bool rust)
    {
        std::vector<std::string> names;
        const size_t open = header.find('(');
        const size_t close = header.rfind(')');
        if (open == std::string::npos || close == std::string::npos || close < open) {
            return names;
        }
        const std::string list = header.substr(open + 1, close - open - 1);
        int depth = 0;
        std::string cur;
        auto flush = [&]() {
            std::string p = trimmed(cur);
            cur.clear();
            if (p.empty()) {
                return;
            }
            if (rust) {
                const size_t colon = p.find(':');
                if (colon != std::string::npos) {
                    names.push_back(trimmed(p.substr(0, colon)));
                }
            } else {
                size_t e = p.size();
                while (e > 0 && !isIdentChar(p[e - 1])) {
                    --e;
                }
                size_t b = e;
                while (b > 0 && isIdentChar(p[b - 1])) {
                    --b;
                }
                if (b < e) {
                    names.push_back(p.substr(b, e - b));
                }
            }
        };
        for (char c : list) {
            if (c == '<' || c == '[' || c == '(') {
                ++depth;
            } else if (c == '>' || c == ']' || c == ')') {
                --depth;
            }
            if (c == ',' && depth == 0) {
                flush();
            } else {
                cur += c;
            }
        }
        flush();
        return names;
    }
    //=============================================================================
    // Line closing the brace block opened at or after `first` (braces in strings
    // and comments ignored), or lines.size().
    size_t
    blockEnd(const std::vector<std::string>& lines, size_t first)
    {
        int balance = 0;
        bool opened = false;
        for (size_t i = first; i < lines.size(); ++i) {
            const std::string code = codeOf(lines[i]);
            bool inStr = false;
            for (size_t k = 0; k < code.size(); ++k) {
                const char c = code[k];
                if (inStr) {
                    if (c == '\\') {
                        ++k;
                    } else if (c == '"') {
                        inStr = false;
                    }
                    continue;
                }
                if (c == '"') {
                    inStr = true;
                } else if (c == '{') {
                    ++balance;
                    opened = true;
                } else if (c == '}') {
                    --balance;
                }
            }
            if (opened && balance <= 0) {
                return i;
            }
        }
        return lines.size();
    }
    //=============================================================================
    // Name of the function a column-0 line starts, or "".
    std::string
    functionStartName(const std::string& line, bool rust)
    {
        std::string t = line;
        if (rust) {
            if (t.rfind("pub ", 0) == 0) {
                t = t.substr(4);
            }
            if (t.rfind("fn ", 0) != 0) {
                return "";
            }
            t = t.substr(3);
        } else {
            if (t.empty() || !isIdentChar(t[0]) || t.find('(') == std::string::npos
                || t.find('=') < t.find('(') || t.rfind("typedef", 0) == 0
                || t.rfind("struct", 0) == 0 || t.rfind("return", 0) == 0) {
                return "";
            }
        }
        const size_t open = t.find('(');
        if (open == std::string::npos) {
            return "";
        }
        size_t e = open;
        while (e > 0 && t[e - 1] == ' ') {
            --e;
        }
        size_t b = e;
        while (b > 0 && isIdentChar(t[b - 1])) {
            --b;
        }
        return t.substr(b, e - b);
    }
    //=============================================================================
} // namespace
//=============================================================================
void
pruneDeadStepCode(std::vector<std::string>& lines, bool rust)
{
    static const std::set<std::string> targets = { "nflow_step", "nflow_rhs", "nflow_rk4sub" };
    for (size_t h = 0; h < lines.size(); ++h) {
        if (!targets.count(functionStartName(lines[h], rust))) {
            continue;
        }
        // Header up to the line opening the body.
        size_t open = h;
        std::string header;
        while (open < lines.size()) {
            header += codeOf(lines[open]) + " ";
            const std::string t = trimmed(codeOf(lines[open]));
            if (!t.empty() && t.back() == '{') {
                break;
            }
            ++open;
        }
        if (open >= lines.size()) {
            return;
        }
        const size_t close = blockEnd(lines, h);
        if (close >= lines.size() || close <= open || trimmed(lines[close]) != "}") {
            continue;
        }
        std::string body;
        for (size_t i = open + 1; i < close; ++i) {
            body += lines[i] + "\n";
        }
        const std::vector<std::string> paramList = paramNames(header, rust);
        const std::set<std::string> params(paramList.begin(), paramList.end());
        while (pruneBodyOnce(body, rust, params)) { }
        if (rust) {
            fixRustMut(body);
        }
        // A parameter the body never uses (or shadows with a local) is marked
        // as unused, like `(void)s;`.
        const std::string masked = maskText(body);
        const std::vector<Item> items = parseItems(masked);
        std::vector<std::string> newBody;
        for (const auto& p : paramList) {
            bool shadowed = false;
            for (const auto& it : items) {
                LocalDecl decl;
                shadowed = shadowed
                    || (it.kind == ItemKind::Stmt
                        && parseLocalDecl(masked.substr(it.begin, it.end - it.begin), rust, decl)
                        && decl.name == p);
            }
            if (shadowed || identUses(masked, p).empty()) {
                newBody.push_back(rust ? "    let _ = " + p + ";" : "  (void)" + p + ";");
            }
        }
        std::string cur;
        for (char c : body) {
            if (c != '\n') {
                cur += c;
                continue;
            }
            const size_t e = cur.find_last_not_of(" \t\r");
            newBody.push_back(e == std::string::npos ? std::string() : cur.substr(0, e + 1));
            cur.clear();
        }
        // A block's `// <type> <id>` banner whose code was entirely removed is
        // followed by the next banner or by the end of the body: drop it.
        auto isBanner = [](const std::string& l) {
            const std::string t = trimmed(l);
            if (t.rfind("// ", 0) != 0) {
                return false;
            }
            const std::string rest = trimmed(t.substr(3));
            const size_t sp = rest.find(' ');
            return sp != std::string::npos
                && rest.find(' ', rest.find_first_not_of(' ', sp)) == std::string::npos;
        };
        for (size_t i = newBody.size(); i-- > 0;) {
            if (!isBanner(newBody[i])) {
                continue;
            }
            size_t n = i + 1;
            while (n < newBody.size() && trimmed(newBody[n]).empty()) {
                ++n;
            }
            if (n == newBody.size() || isBanner(newBody[n])) {
                newBody.erase(newBody.begin() + static_cast<std::ptrdiff_t>(i));
            }
        }
        while (!newBody.empty() && trimmed(newBody.back()).empty()) {
            newBody.pop_back();
        }
        lines.erase(lines.begin() + static_cast<std::ptrdiff_t>(open) + 1,
            lines.begin() + static_cast<std::ptrdiff_t>(close));
        lines.insert(
            lines.begin() + static_cast<std::ptrdiff_t>(open) + 1, newBody.begin(), newBody.end());
        h = open + newBody.size();
    }
}
//=============================================================================
void
pruneUnusedHelpers(std::vector<std::string>& lines, bool rust, const std::string& extraText)
{
    // Private items: Rust `fn` / `const` / `static` (not `pub`), C `static`
    // functions and constants. A function ends with its body's closing brace, a
    // constant with its `;`.
    struct Helper
    {
        std::string name;
        bool function;
    };
    auto helperOf = [rust](const std::string& line) -> Helper {
        if (rust) {
            if (line.rfind("fn ", 0) == 0) {
                return { functionStartName(line, true), true };
            }
            for (const char* kw : { "const ", "static " }) {
                if (line.rfind(kw, 0) == 0) {
                    size_t b = std::char_traits<char>::length(kw);
                    size_t e = b;
                    while (e < line.size() && isIdentChar(line[e])) {
                        ++e;
                    }
                    return { line.substr(b, e - b), false };
                }
            }
            return { "", false };
        }
        if (line.rfind("static ", 0) != 0) {
            return { "", false };
        }
        const std::string fn = functionStartName(line, false);
        if (!fn.empty()) {
            return { fn, true };
        }
        const size_t stop = line.find_first_of("=[;");
        if (stop == std::string::npos) {
            return { "", false };
        }
        size_t e = stop;
        while (e > 0 && !isIdentChar(line[e - 1])) {
            --e;
        }
        size_t b = e;
        while (b > 0 && isIdentChar(line[b - 1])) {
            --b;
        }
        return { line.substr(b, e - b), false };
    };
    auto constEnd = [&lines](size_t first) {
        int balance = 0;
        for (size_t i = first; i < lines.size(); ++i) {
            const std::string code = codeOf(lines[i]);
            for (char c : code) {
                if (c == '(' || c == '[' || c == '{') {
                    ++balance;
                } else if (c == ')' || c == ']' || c == '}') {
                    --balance;
                }
            }
            const std::string t = trimmed(code);
            if (balance <= 0 && !t.empty() && t.back() == ';') {
                return i;
            }
        }
        return lines.size();
    };
    std::vector<std::string> extraCode;
    {
        std::string cur;
        for (char c : extraText + "\n") {
            if (c != '\n') {
                cur += c;
                continue;
            }
            extraCode.push_back(codeOf(cur));
            cur.clear();
        }
    }
    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t first = 0; first < lines.size() && !changed; ++first) {
            const Helper h = helperOf(lines[first]);
            if (h.name.empty()) {
                continue;
            }
            const size_t last = h.function ? blockEnd(lines, first) : constEnd(first);
            if (last == lines.size()) {
                continue;
            }
            size_t top = first;
            while (top > 0 && lines[top - 1].rfind("#[", 0) == 0) {
                --top;
            }
            bool used = false;
            for (const auto& l : extraCode) {
                used = used || !identUses(l, h.name).empty();
            }
            for (size_t i = 0; i < lines.size() && !used; ++i) {
                if ((i < top || i > last) && !identUses(codeOf(lines[i]), h.name).empty()) {
                    used = true;
                }
            }
            if (used) {
                continue;
            }
            lines.erase(lines.begin() + static_cast<std::ptrdiff_t>(top),
                lines.begin() + static_cast<std::ptrdiff_t>(last) + 1);
            if (top > 0 && top < lines.size() && lines[top - 1].empty() && lines[top].empty()) {
                lines.erase(lines.begin() + static_cast<std::ptrdiff_t>(top));
            }
            changed = true;
        }
    }
}
//=============================================================================
} // namespace nflow
//=============================================================================
