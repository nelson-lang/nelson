//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cwctype>
#include "StringToDoubleComplex.hpp"
#include "StringToDouble.hpp"
#include "StringTrim.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// One signed term of a complex literal: "1.5e3", "-2i", "+j", "Inf".
struct ComplexTerm
{
    bool valid = false;
    bool imaginary = false;
    double value = 0;
};
//=============================================================================
static std::wstring
normalizeComplexText(const std::wstring& str);
static ComplexTerm
parseComplexTerm(std::wstring text, bool allowSecondSign);
static bool
parseProductWithUnit(const std::wstring& text, double& imaginary);
static doublecomplex
combineTerms(const ComplexTerm& first, const ComplexTerm& second, bool& wasConverted);
//=============================================================================
doublecomplex
stringToDoubleComplex(const std::wstring& str, bool& wasConverted)
{
    wasConverted = false;
    doublecomplex res(std::nan(""), 0);
    std::wstring text = normalizeComplexText(str);
    if (text.empty()) {
        return res;
    }
    double imaginary = 0;
    if (text.find(L'*') != std::wstring::npos) {
        // "5*i", "i*5": a real factor times the imaginary unit
        if (parseProductWithUnit(text, imaginary)) {
            wasConverted = true;
            res = doublecomplex(0, imaginary);
        }
        return res;
    }
    // The second term starts at the first operator that is not a leading
    // sign and does not belong to an exponent ("1e+3", "0x1p-3").
    size_t split = std::wstring::npos;
    for (size_t k = 1; k < text.size(); ++k) {
        if ((text[k] == L'+' || text[k] == L'-')
            && std::wcschr(L"eEpP+-", text[k - 1]) == nullptr) {
            split = k;
            break;
        }
    }
    if (split == std::wstring::npos) {
        ComplexTerm term = parseComplexTerm(text, false);
        if (term.valid) {
            wasConverted = true;
            res = term.imaginary ? doublecomplex(0, term.value) : doublecomplex(term.value, 0);
        }
        return res;
    }
    ComplexTerm first = parseComplexTerm(text.substr(0, split), false);
    ComplexTerm second = parseComplexTerm(text.substr(split), true);
    return combineTerms(first, second, wasConverted);
}
//=============================================================================
// Commas are thousands separators, whitespace is accepted around the text
// and next to operators or the imaginary unit ("1 + 2 i"), not between digits.
static std::wstring
normalizeComplexText(const std::wstring& str)
{
    size_t first = 0;
    size_t last = str.size();
    while (first < last && IsNumberLeadingBlank(str[first])) {
        ++first;
    }
    while (
        last > first && (IsNumberLeadingBlank(str[last - 1]) || IsTextWhitespace(str[last - 1]))) {
        --last;
    }
    std::wstring out;
    out.reserve(last - first);
    for (size_t k = first; k < last; ++k) {
        wchar_t c = str[k];
        if (c == L',') {
            continue;
        }
        if (IsTextWhitespace(c)) {
            wchar_t previous = out.empty() ? L' ' : out.back();
            size_t next = k + 1;
            while (next < last && IsTextWhitespace(str[next])) {
                ++next;
            }
            wchar_t following = next < last ? str[next] : L' ';
            // a blank may surround an operator, but never follow a sign that
            // belongs to a number ("- 1", "1 - - 2i")
            bool unarySign = (previous == L'+' || previous == L'-')
                && (out.size() == 1
                    || (out[out.size() - 2] != 0
                        && std::wcschr(L"+-*eEpP", out[out.size() - 2]) != nullptr));
            bool nearOperator = !unarySign
                && ((previous != 0 && std::wcschr(L"+-*", previous) != nullptr)
                    || (following != 0 && std::wcschr(L"+-*ij", following) != nullptr));
            if (!nearOperator) {
                out.push_back(c);
            }
            continue;
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
// text is "[op][sign]number[i|j]"; the leading operator is the sign of the
// term, a second sign is only accepted right after an operator ("+-2i").
static ComplexTerm
parseComplexTerm(std::wstring text, bool allowSecondSign)
{
    ComplexTerm term;
    double sign = 1;
    if (!text.empty() && (text[0] == L'+' || text[0] == L'-')) {
        sign = text[0] == L'-' ? -1 : 1;
        text.erase(0, 1);
        if (allowSecondSign && !text.empty() && (text[0] == L'+' || text[0] == L'-')) {
            sign *= text[0] == L'-' ? -1 : 1;
            text.erase(0, 1);
        }
    }
    if (text.empty() || IsTextWhitespace(text[0])) {
        return term;
    }
    wchar_t lastChar = text.back();
    if (lastChar == L'i' || lastChar == L'j') {
        term.imaginary = true;
        text.pop_back();
        if (text.empty()) {
            term.valid = true;
            term.value = sign;
            return term;
        }
    }
    if (text[0] == L'+' || text[0] == L'-') {
        return term;
    }
    bool converted = false;
    double magnitude = stringToDouble(text, converted);
    if (converted) {
        term.valid = true;
        term.value = sign * magnitude;
    }
    return term;
}
//=============================================================================
static bool
parseProductWithUnit(const std::wstring& text, double& imaginary)
{
    size_t star = text.find(L'*');
    std::wstring left = text.substr(0, star);
    std::wstring right = text.substr(star + 1);
    auto isUnit = [](const std::wstring& s) {
        return s.size() == 1 && std::wcschr(L"ij", s[0]) != nullptr;
    };
    const std::wstring& factor = isUnit(right) ? left : (isUnit(left) ? right : L"");
    if (factor.empty() || factor.find(L'*') != std::wstring::npos) {
        return false;
    }
    bool converted = false;
    double value = stringToDouble(factor, converted);
    if (!converted) {
        return false;
    }
    imaginary = value;
    return true;
}
//=============================================================================
static doublecomplex
combineTerms(const ComplexTerm& first, const ComplexTerm& second, bool& wasConverted)
{
    wasConverted = false;
    if (!first.valid || !second.valid || first.imaginary == second.imaginary) {
        return doublecomplex(std::nan(""), 0);
    }
    wasConverted = true;
    const ComplexTerm& real = first.imaginary ? second : first;
    const ComplexTerm& imag = first.imaginary ? first : second;
    return doublecomplex(real.value, imag.value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
