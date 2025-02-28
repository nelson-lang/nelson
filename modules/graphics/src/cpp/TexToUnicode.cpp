//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <regex>
#include <unordered_map>
#include "nlsBuildConfig.h"
#include "TexToUnicode.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// This is not a TeX parser
// Only used to replace commons Tex by Unicode equivalent
// Use natively unicode support is the recommended way
//=============================================================================
static std::wstring
replaceTexSpecialCharacters(const std::wstring& text);
//=============================================================================
wstringVector
texToUnicode(const wstringVector& strs)
{
    wstringVector results;
    results.resize(strs.size());
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)strs.size(); ++k) {
        results[k] = texToUnicode(strs[k]);
    }
    return results;
}
//=============================================================================
static std::wstring
replaceSubScript(const std::wstring& str)
{
    // Some characters have not unicode equivalent with Unicode 4.0
    // only available are listed
    std::unordered_map<std::wstring, std::wstring> subcript;
    subcript[L"0"] = L"\u2080";
    subcript[L"1"] = L"\u2081";
    subcript[L"2"] = L"\u2082";
    subcript[L"3"] = L"\u2083";
    subcript[L"4"] = L"\u2084";
    subcript[L"5"] = L"\u2085";
    subcript[L"6"] = L"\u2086";
    subcript[L"7"] = L"\u2087";
    subcript[L"8"] = L"\u2088";
    subcript[L"9"] = L"\u2089";
    subcript[L"a"] = L"\u2090";
    subcript[L"e"] = L"\u2091";
    subcript[L"h"] = L"\u2095";
    subcript[L"i"] = L"\u1d62";
    subcript[L"j"] = L"\u2c7c";
    subcript[L"k"] = L"\u2096";
    subcript[L"l"] = L"\u2097";
    subcript[L"m"] = L"\u2098";
    subcript[L"n"] = L"\u2099";
    subcript[L"o"] = L"\u2092";
    subcript[L"p"] = L"\u209a";
    subcript[L"r"] = L"\u1d63";
    subcript[L"s"] = L"\u209b";
    subcript[L"t"] = L"\u209c";
    subcript[L"u"] = L"\u1d64";
    subcript[L"v"] = L"\u1d65";
    subcript[L"x"] = L"\u2093";
    subcript[L"+"] = L"\u208A";
    subcript[L"-"] = L"\u208B";
    subcript[L"="] = L"\u208C";
    subcript[L"("] = L"\u208D";
    subcript[L")"] = L"\u208E";
    subcript[L"β"] = L"\u1d66";
    subcript[L"ψ"] = L"\u1d69";
    subcript[L"χ"] = L"\u1d6a";

    std::wstring result = str;
    if (StringHelpers::contains(result, L"_{")) {
        std::wstring exp = L"(.*)_\\{(.*)\\}";
        try {
            std::wregex r(exp, std::regex::ECMAScript);
            std::wstring s = result;
            std::wsmatch match;
            if (std::regex_match(s, match, r)) {
                if (match.size() == 3) {
                    std::wstring exponent = match[2];
                    for (size_t k = 0; k < exponent.size(); ++k) {
                        std::wstring search = std::wstring(1, exponent[k]);
                        if (subcript.count(search) > 0) {
                            StringHelpers::replace_all(exponent, search, subcript[search]);
                        }
                    }
                    result = match[1].str() + exponent;
                }
            }
        } catch (const std::regex_error&) {
        }
    }
    return result;
}
//=============================================================================
static std::wstring
replaceSuperScript(const std::wstring& str)
{
    // Some characters have not unicode equivalent with Unicode 4.0
    // only available are listed
    std::unordered_map<std::wstring, std::wstring> superscript;
    superscript[L"0"] = L"\u2070";
    superscript[L"1"] = L"\u00B9";
    superscript[L"2"] = L"\u00B2";
    superscript[L"3"] = L"\u00B3";
    superscript[L"4"] = L"\u2074";
    superscript[L"5"] = L"\u2075";
    superscript[L"6"] = L"\u2076";
    superscript[L"7"] = L"\u2077";
    superscript[L"8"] = L"\u2078";
    superscript[L"9"] = L"\u2079";
    superscript[L"a"] = L"\u1d43";
    superscript[L"b"] = L"\u1d47";
    superscript[L"c"] = L"\u1d9c";
    superscript[L"d"] = L"\u1d48";
    superscript[L"e"] = L"\u1d49";
    superscript[L"f"] = L"\u1da0";
    superscript[L"g"] = L"\u1d4d";
    superscript[L"h"] = L"\u02b0";
    superscript[L"i"] = L"\u2071";
    superscript[L"j"] = L"\u02b2";
    superscript[L"k"] = L"\u1d4f";
    superscript[L"l"] = L"\u02e1";
    superscript[L"m"] = L"\u1d50";
    superscript[L"n"] = L"\u207f";
    superscript[L"o"] = L"\u1d52";
    superscript[L"p"] = L"\u1d56";
    // superscript[L"q"] = L"?";
    superscript[L"r"] = L"\u02b3";
    superscript[L"s"] = L"\u02e2";
    superscript[L"t"] = L"\u1d57";
    superscript[L"u"] = L"\u1d58";
    superscript[L"v"] = L"\u1d5b";
    superscript[L"w"] = L"\u02b7";
    superscript[L"x"] = L"\u02e3";
    superscript[L"y"] = L"\u02b8";
    superscript[L"z"] = L"\u1DBB";
    superscript[L"A"] = L"\u1d2c";
    superscript[L"B"] = L"\u1d2e";
    // superscript[L"C"] = L"\ua7fd";
    superscript[L"D"] = L"\u1d30";
    superscript[L"E"] = L"\u1d31";
    // superscript[L"F"] = "?";
    superscript[L"G"] = L"\u1d33";
    superscript[L"H"] = L"\u1d34";
    superscript[L"I"] = L"\u1d35";
    superscript[L"J"] = L"\u1d36";
    superscript[L"K"] = L"\u1d37";
    superscript[L"L"] = L"\u1d38";
    superscript[L"M"] = L"\u1d39";
    superscript[L"N"] = L"\u1d3a";
    superscript[L"O"] = L"\u1d3c";
    superscript[L"P"] = L"\u1d3e";
    // superscript[L"Q"] = "?";
    superscript[L"R"] = L"\u1d3f";
    // superscript[L"S"] = "?";
    superscript[L"T"] = L"\u1d40";
    superscript[L"U"] = L"\u1d41";
    superscript[L"V"] = L"\u2c7d";
    superscript[L"W"] = L"\u1d42";
    // superscript[L"X"] = "?";
    // superscript[L"Y"] = "?";
    // superscript[L"Z"] = "?";
    superscript[L"+"] = L"\u207A";
    superscript[L"-"] = L"\u207B";
    superscript[L"="] = L"\u207C";
    superscript[L"("] = L"\u207D";
    superscript[L")"] = L"\u207E";
    superscript[L"α"] = L"\u1d45";
    superscript[L"β"] = L"\u1d5d";
    superscript[L"γ"] = L"\u1d5e";
    superscript[L"δ"] = L"\u1d5f";
    superscript[L"ϵ"] = L"\u1d4b";
    superscript[L"θ"] = L"\u1dbf";
    superscript[L"ι"] = L"\u1da5";
    // superscript[L"\\pho"] = L"?";
    superscript[L"ϕ"] = L"\u1db2";
    superscript[L"ψ"] = L"\u1d60";
    superscript[L"χ"] = L"\u1d61";
    std::wstring result = str;
    if (StringHelpers::contains(result, L"^{")) {
        std::wstring exp = L"(.*)\\^\\{(.*)\\}";
        try {
            std::wregex r(exp, std::regex::ECMAScript);
            std::wstring s = result;
            std::wsmatch match;
            if (std::regex_match(s, match, r)) {
                if (match.size() == 3) {
                    std::wstring exponent = match[2];
                    for (size_t k = 0; k < exponent.size(); ++k) {
                        std::wstring search = std::wstring(1, exponent[k]);
                        if (superscript.count(search) > 0) {
                            StringHelpers::replace_all(exponent, search, superscript[search]);
                        }
                    }
                    result = match[1].str() + exponent;
                }
            }
        } catch (const std::regex_error&) {
        }
    }
    return result;
}
//=============================================================================
std::wstring
texToUnicode(const std::wstring& str)
{
    std::wstring result = replaceTexSpecialCharacters(str);
    result = replaceSuperScript(result);
    return replaceSubScript(result);
}
//=============================================================================
std::wstring
replaceTexSpecialCharacters(const std::wstring& text)
{
    std::wstring modifiedText(text);
    StringHelpers::replace_all(modifiedText, L"\\alpha", L"α");
    StringHelpers::replace_all(modifiedText, L"\\upsilon", L"υ");
    StringHelpers::replace_all(modifiedText, L"\\sim", L"~");
    StringHelpers::replace_all(modifiedText, L"\\angle", L"∠");
    StringHelpers::replace_all(modifiedText, L"\\phi", L"ϕ");
    StringHelpers::replace_all(modifiedText, L"\\leq", L"≤");
    StringHelpers::replace_all(modifiedText, L"\\ast", L"*");
    StringHelpers::replace_all(modifiedText, L"\\chi", L"χ");
    StringHelpers::replace_all(modifiedText, L"\\infty", L"∞");
    StringHelpers::replace_all(modifiedText, L"\\beta", L"β");
    StringHelpers::replace_all(modifiedText, L"\\psi", L"ψ");
    StringHelpers::replace_all(modifiedText, L"\\clubsuit", L"♣");
    StringHelpers::replace_all(modifiedText, L"\\gamma", L"γ");
    StringHelpers::replace_all(modifiedText, L"\\omega", L"ω");
    StringHelpers::replace_all(modifiedText, L"\\diamondsuit", L"♦");
    StringHelpers::replace_all(modifiedText, L"\\delta", L"δ");
    StringHelpers::replace_all(modifiedText, L"\\Gamma", L"Γ");
    StringHelpers::replace_all(modifiedText, L"\\heartsuit", L"♥");
    StringHelpers::replace_all(modifiedText, L"\\epsilon", L"ϵ");
    StringHelpers::replace_all(modifiedText, L"\\Delta", L"Δ");
    StringHelpers::replace_all(modifiedText, L"\\spadesuit", L"♠");
    StringHelpers::replace_all(modifiedText, L"\\zeta", L"ζ");
    StringHelpers::replace_all(modifiedText, L"\\Theta", L"Θ");
    StringHelpers::replace_all(modifiedText, L"\\leftrightarrow", L"↔");
    StringHelpers::replace_all(modifiedText, L"\\eta", L"η");
    StringHelpers::replace_all(modifiedText, L"\\Lambda", L"Λ");
    StringHelpers::replace_all(modifiedText, L"\\leftarrow", L"←");
    StringHelpers::replace_all(modifiedText, L"\\theta", L"θ");
    StringHelpers::replace_all(modifiedText, L"\\Xi", L"Ξ");
    StringHelpers::replace_all(modifiedText, L"\\Leftarrow", L"⇐");
    StringHelpers::replace_all(modifiedText, L"\\vartheta", L"ϑ");
    StringHelpers::replace_all(modifiedText, L"\\Pi", L"Π");
    StringHelpers::replace_all(modifiedText, L"\\uparrow", L"↑");
    StringHelpers::replace_all(modifiedText, L"\\iota", L"ι");
    StringHelpers::replace_all(modifiedText, L"\\Sigma", L"Σ");
    StringHelpers::replace_all(modifiedText, L"\\rightarrow", L"→");
    StringHelpers::replace_all(modifiedText, L"\\kappa", L"κ");
    StringHelpers::replace_all(modifiedText, L"\\upsilon", L"ϒ");
    StringHelpers::replace_all(modifiedText, L"\\Rightarrow", L"⇒");
    StringHelpers::replace_all(modifiedText, L"\\lambda", L"λ");
    StringHelpers::replace_all(modifiedText, L"\\Phi", L"Φ");
    StringHelpers::replace_all(modifiedText, L"\\downarrow", L"↓");
    StringHelpers::replace_all(modifiedText, L"\\mu", L"µ");
    StringHelpers::replace_all(modifiedText, L"\\Psi", L"Ψ");
    StringHelpers::replace_all(modifiedText, L"\\circ", L"º");
    StringHelpers::replace_all(modifiedText, L"\\nu", L"ν");
    StringHelpers::replace_all(modifiedText, L"\\Omega", L"Ω");
    StringHelpers::replace_all(modifiedText, L"\\pm", L"±");
    StringHelpers::replace_all(modifiedText, L"\\xi", L"ξ");
    StringHelpers::replace_all(modifiedText, L"\\forall", L"∀");
    StringHelpers::replace_all(modifiedText, L"\\geq", L"≥");
    StringHelpers::replace_all(modifiedText, L"\\pi", L"π");
    StringHelpers::replace_all(modifiedText, L"\\exists", L"∃");
    StringHelpers::replace_all(modifiedText, L"\\propto", L"∝");
    StringHelpers::replace_all(modifiedText, L"\\rho", L"ρ");
    StringHelpers::replace_all(modifiedText, L"\\ni", L"∍");
    StringHelpers::replace_all(modifiedText, L"\\partial", L"∂");
    StringHelpers::replace_all(modifiedText, L"\\sigma", L"σ");
    StringHelpers::replace_all(modifiedText, L"\\cong", L"≅");
    StringHelpers::replace_all(modifiedText, L"\\bullet", L"•");
    StringHelpers::replace_all(modifiedText, L"\\varsigma", L"ς");
    StringHelpers::replace_all(modifiedText, L"\\approx", L"≈");
    StringHelpers::replace_all(modifiedText, L"\\div", L"÷");
    StringHelpers::replace_all(modifiedText, L"\\tau", L"τ");
    StringHelpers::replace_all(modifiedText, L"\\Re", L"ℜ");
    StringHelpers::replace_all(modifiedText, L"\\neq", L"≠");
    StringHelpers::replace_all(modifiedText, L"\\equiv", L"≡");
    StringHelpers::replace_all(modifiedText, L"\\oplus", L"⊕");
    StringHelpers::replace_all(modifiedText, L"\\aleph", L"ℵ");
    StringHelpers::replace_all(modifiedText, L"\\Im", L"ℑ");
    StringHelpers::replace_all(modifiedText, L"\\cup", L"∪");
    StringHelpers::replace_all(modifiedText, L"\\wp", L"℘");
    StringHelpers::replace_all(modifiedText, L"\\otimes", L"⊗");
    StringHelpers::replace_all(modifiedText, L"\\subseteq", L"⊆");
    StringHelpers::replace_all(modifiedText, L"\\oslash", L"∅");
    StringHelpers::replace_all(modifiedText, L"\\cap", L"∩");
    StringHelpers::replace_all(modifiedText, L"\\in", L"∈");
    StringHelpers::replace_all(modifiedText, L"\\supseteq", L"⊇");
    StringHelpers::replace_all(modifiedText, L"\\supset", L"⊃");
    StringHelpers::replace_all(modifiedText, L"\\lceil", L"⌈");
    StringHelpers::replace_all(modifiedText, L"\\subset", L"⊂");
    StringHelpers::replace_all(modifiedText, L"\\int", L"∫");
    StringHelpers::replace_all(modifiedText, L"\\cdot", L"·");
    StringHelpers::replace_all(modifiedText, L"\\o", L"ο");
    StringHelpers::replace_all(modifiedText, L"\\rfloor", L"⌋");
    StringHelpers::replace_all(modifiedText, L"\\neg", L"¬");
    StringHelpers::replace_all(modifiedText, L"\\nabla", L"∇");
    StringHelpers::replace_all(modifiedText, L"\\lfloor", L"⌊");
    StringHelpers::replace_all(modifiedText, L"\\times", L"x");
    StringHelpers::replace_all(modifiedText, L"\\ldots", L"…");
    StringHelpers::replace_all(modifiedText, L"\\perp", L"⊥");
    StringHelpers::replace_all(modifiedText, L"\\surd", L"√");
    StringHelpers::replace_all(modifiedText, L"\\prime", L"´");
    StringHelpers::replace_all(modifiedText, L"\\wedge", L"∧");
    StringHelpers::replace_all(modifiedText, L"\\varpi", L"ϖ");
    StringHelpers::replace_all(modifiedText, L"\\0", L"∅");
    StringHelpers::replace_all(modifiedText, L"\\rceil", L"⌉");
    StringHelpers::replace_all(modifiedText, L"\\rangle", L"〉");
    StringHelpers::replace_all(modifiedText, L"\\mid", L"|");
    StringHelpers::replace_all(modifiedText, L"\\vee", L"∨");
    StringHelpers::replace_all(modifiedText, L"\\langle", L"〈");
    StringHelpers::replace_all(modifiedText, L"\\copyright", L"©");
    return modifiedText;
}
//=============================================================================
}
//=============================================================================
