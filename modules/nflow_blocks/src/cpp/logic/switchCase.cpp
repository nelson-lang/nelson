//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenLang.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <cstdlib>
#include <string>
#include <vector>
#include "logic_blocks.hpp"
//=============================================================================
// switchCase: routes control to one of several action ports. The scalar input
// is truncated toward zero to an integer and matched against CaseConditions (a
// cell literal such as "{1, [7 9 4]}"); the first matching case drives its
// output to 1.0 and every other output to 0.0. With ShowDefaultCase on, an
// unmatched value drives the last (default) output. No fall-through. These
// outputs are meant to gate action subsystems.
//=============================================================================
namespace {
// Parse a case-conditions cell literal, e.g. "{1, [7 9 4]}", into a list of
// integer sets. Tolerant of spaces/commas/semicolons inside bracket lists and
// of a missing outer brace.
std::vector<std::vector<long long>>
parseCaseConditions(const std::string& s)
{
    std::vector<std::vector<long long>> cases;
    const char* p = s.c_str();
    auto skipws = [&]() {
        while (*p == ' ' || *p == '\t') {
            ++p;
        }
    };
    skipws();
    if (*p == '{') {
        ++p;
    }
    while (*p) {
        skipws();
        if (*p == '}' || *p == '\0') {
            break;
        }
        std::vector<long long> one;
        if (*p == '[') {
            ++p;
            while (*p && *p != ']') {
                skipws();
                if (*p == ']') {
                    break;
                }
                char* end = nullptr;
                double v = std::strtod(p, &end);
                if (end == p) {
                    ++p;
                    continue;
                }
                one.push_back(static_cast<long long>(v));
                p = end;
                skipws();
                if (*p == ',' || *p == ';') {
                    ++p;
                }
            }
            if (*p == ']') {
                ++p;
            }
        } else {
            char* end = nullptr;
            double v = std::strtod(p, &end);
            if (end != p) {
                one.push_back(static_cast<long long>(v));
                p = end;
            } else {
                ++p;
                continue;
            }
        }
        cases.push_back(std::move(one));
        skipws();
        if (*p == ',') {
            ++p;
        }
    }
    return cases;
}
//=============================================================================
// Read the CaseConditions from either the cell-literal string or a native JSON
// array of numbers / arrays.
std::vector<std::vector<long long>>
readCaseConditions(const Nelson::NFlow::json& params)
{
    if (params.contains("CaseConditions")) {
        const auto& c = params["CaseConditions"];
        if (c.is_string()) {
            return parseCaseConditions(c.get<std::string>());
        }
        if (c.is_array()) {
            std::vector<std::vector<long long>> cases;
            for (const auto& ci : c) {
                std::vector<long long> one;
                if (ci.is_number()) {
                    one.push_back(static_cast<long long>(ci.get<double>()));
                } else if (ci.is_array()) {
                    for (const auto& cv : ci) {
                        if (cv.is_number()) {
                            one.push_back(static_cast<long long>(cv.get<double>()));
                        }
                    }
                }
                cases.push_back(std::move(one));
            }
            return cases;
        }
    }
    return {};
}
//=============================================================================
bool
showDefaultCase(const Nelson::NFlow::json& params)
{
    if (!params.contains("ShowDefaultCase")) {
        return false;
    }
    const auto& v = params["ShowDefaultCase"];
    if (v.is_boolean()) {
        return v.get<bool>();
    }
    if (v.is_string()) {
        const std::string s = v.get<std::string>();
        return s == "on" || s == "true" || s == "1";
    }
    if (v.is_number()) {
        return v.get<double>() != 0.0;
    }
    return false;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleSwitchCase(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    const int nOut = std::max(1, b.outputs);
    const double u = getInput(ctx, b.nid, 0, 0.0);
    const long long v = static_cast<long long>(std::trunc(u));
    const bool showDefault = showDefaultCase(b.params);
    const int nCases = showDefault ? std::max(0, nOut - 1) : nOut;

    const std::vector<std::vector<long long>> cases = readCaseConditions(b.params);
    int selected = -1;
    for (int i = 0; i < nCases && i < static_cast<int>(cases.size()) && selected < 0; ++i) {
        for (long long cv : cases[static_cast<size_t>(i)]) {
            if (cv == v) {
                selected = i;
                break;
            }
        }
    }
    if (selected < 0 && showDefault) {
        selected = nOut - 1; // the default port is last
    }
    for (int p = 0; p < nOut; ++p) {
        setOutput(ctx, b.nid, (p == selected) ? 1.0 : 0.0, p);
    }
    return false;
}
//=============================================================================
// Code generation. The case sets are read once, here, and become a chain of
// comparisons against the truncated input - the same first-match-wins order the
// handler walks, with no fall-through.
//=============================================================================
namespace {
Nelson::NFlow::BlockCodegenTemplate
makeSwitchCaseCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitStep = [L](const BlockCodegenArgs& a) {
        const json& p = *a.params;
        const int nOut = std::max(1, a.block->value(nflow::kOutputs, 1));
        const bool showDefault = showDefaultCase(p);
        const int nCases = showDefault ? std::max(0, nOut - 1) : nOut;
        const std::vector<std::vector<long long>> cases = readCaseConditions(p);
        // The control, truncated toward zero, computed once into a local.
        const std::string v = "__sc_" + a.id;
        // The generated Rust is libm-based (no std method on f64 there).
        a.line(L.declLocal(v, L.call("trunc", a.in[0])));
        std::string taken;
        for (int k = 0; k < nOut; ++k) {
            const bool isDefault = showDefault && (k == nOut - 1);
            std::string cond;
            if (isDefault) {
                cond = taken.empty()
                    ? std::string("1.0")
                    : ("(" + L.ternary("(" + taken + ") != 0.0", "0.0", "1.0") + ")");
            } else if (k < nCases && k < static_cast<int>(cases.size())
                && !cases[static_cast<size_t>(k)].empty()) {
                std::string match;
                for (long long cv : cases[static_cast<size_t>(k)]) {
                    if (!match.empty()) {
                        match += " || ";
                    }
                    match += v + " == " + a.fmt(static_cast<double>(cv));
                }
                cond = "(" + L.ternary(match, "1.0", "0.0") + ")";
                if (!taken.empty()) {
                    cond = "("
                        + L.ternary("(" + taken + ") == 0.0 && (" + cond + ") != 0.0", "1.0", "0.0")
                        + ")";
                }
            } else {
                cond = "0.0"; // a port with no case behind it is never selected
            }
            a.line(nflow::codegenOutVar(a.id, k) + " = " + cond + ";");
            if (!isDefault) {
                taken = taken.empty()
                    ? cond
                    : ("("
                          + L.ternary(
                              "(" + taken + ") != 0.0 || (" + cond + ") != 0.0", "1.0", "0.0")
                          + ")");
            }
        }
    };
    return t;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSwitchCase()
{
    return makeSwitchCaseCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSwitchCase()
{
    return makeSwitchCaseCodegen({ true });
}
//=============================================================================
