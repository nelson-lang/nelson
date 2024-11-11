//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <locale>
#include <cmath>
#include <cwctype>
#include "StringHelpers.hpp"
#include "StringToDoubleComplex.hpp"
#include "StringToDouble.hpp"
//=============================================================================
namespace Nelson {
#define ComplexCharI L'i'
#define ComplexCharJ L'j'
#define LessChar L'-'
#define PlusChar L'+'
//=============================================================================
static bool
is_unit_imaginary(const std::wstring& str, double& im)
{
    bool isUnitImag = false;
    im = nan("");
    if (str.empty()) {
        return isUnitImag;
    }
    std::wstring nextString = str;
    if (str[0] == LessChar) {
        im = -1.0;
        nextString.erase(nextString.begin());
    } else {
        im = +1.0;
        if (str[0] == PlusChar) {
            nextString = str;
            nextString.erase(nextString.begin());
        } else {
            nextString = str;
        }
    }
    if (nextString.size() == 1) {
        if ((nextString[0] == ComplexCharI || nextString[0] == ComplexCharJ)) {
            isUnitImag = true;
        }
    }
    return isUnitImag;
}
/* ========================================================================== */
static std::wstring
ToUpper(const std::wstring& A)
{
    std::wstring res = A;
    transform(res.begin(), res.end(), res.begin(), towupper);
    return res;
}
//=============================================================================
static int
ParseNumber(const std::wstring& tx)
{
    if (ToUpper(tx).compare(L"NAN*I") == 0 || ToUpper(tx).compare(L"INF*I") == 0
        || ToUpper(tx).compare(L"INFI") == 0 || ToUpper(tx).compare(L"NANI") == 0) {
        return 3;
    }
    std::locale loc;
    int lookahead = 0;
    int len = 0;
    if ((tx[len] == PlusChar) || (tx[len] == LessChar)) {
        len++;
    }
    lookahead = len;
    while (isdigit(tx[len], loc)) {
        len++;
    }
    lookahead = len;
    if (tx[lookahead] == L'.') {
        lookahead++;
        len = 0;
        while (isdigit(tx[len + lookahead], loc)) {
            len++;
        }
        lookahead += len;
    }
    if ((tx[lookahead] == L'E') || (tx[lookahead] == L'e') || (tx[lookahead] == L'D')
        || (tx[lookahead] == L'd')) {
        lookahead++;
        if ((tx[lookahead] == PlusChar) || (tx[lookahead] == LessChar)) {
            lookahead++;
        }
        len = 0;
        while (isdigit(tx[len + lookahead], loc)) {
            len++;
        }
        lookahead += len;
    }
    return lookahead;
}
//=============================================================================
static void
ParseComplexValue(const std::wstring& tx, double& real, double& imag)
{
    int lnum = ParseNumber(tx);
    int rnum = ParseNumber(tx.substr(lnum));
    std::wstring num1 = tx.substr(0, lnum);
    std::wstring num2 = tx.substr(lnum, rnum);
    if (num1.empty() && num2.empty()) {
        if (StringHelpers::starts_with(ToUpper(tx), L"-INF")) {
            real = -std::numeric_limits<double>::infinity();
        } else if (StringHelpers::starts_with(ToUpper(tx), L"+INF")
            || StringHelpers::starts_with(ToUpper(tx), L"INF")) {
            real = std::numeric_limits<double>::infinity();
        } else {
            real = nan("");
        }
        if (StringHelpers::ends_with(ToUpper(tx), L"-INFI")) {
            imag = -std::numeric_limits<double>::infinity();
        } else if (StringHelpers::ends_with(ToUpper(tx), L"+INFI")) {
            imag = std::numeric_limits<double>::infinity();
        } else if (StringHelpers::ends_with(ToUpper(tx), L"NANI")) {
            imag = nan("");
        } else {
            imag = 0;
        }
        return;
    }
    bool wasConverted;
    if ((num1 == L"+") && (ToUpper(num2) == L"INF")) {
        imag = stringToDouble(num2, wasConverted);
        real = 0;
        return;
    }
    if ((num1 == L"+") && (ToUpper(num2) == L"NAN")) {
        imag = stringToDouble(num2, wasConverted);
        real = nan("");
        return;
    }
    if ((num1 == L"-") && (ToUpper(num2) == L"INF")) {
        imag = -stringToDouble(num2, wasConverted);
        real = 0;
        return;
    }
    if ((num1 == L"-") && (ToUpper(num2) == L"NAN")) {
        imag = stringToDouble(num2, wasConverted);
        real = nan("");
        return;
    }
    if (num1 == L"+") {
        num1 = L"+1";
    }
    if (num2 == L"+") {
        num2 = L"+1";
    }
    if (num1 == L"-") {
        num1 = L"-1";
    }
    if (num2 == L"-") {
        num2 = L"-1";
    }
    if (num2.empty()) {
        imag = stringToDouble(num1, wasConverted);
        if (std::isnan(imag)) {
            real = nan("");
        } else {
            real = 0;
        }
    } else {
        real = stringToDouble(num1, wasConverted);
        imag = stringToDouble(num2, wasConverted);
    }
}
//=============================================================================
doublecomplex
stringToDoubleComplex(const std::wstring& str, bool& wasConverted)
{
    doublecomplex res = doublecomplex(nan(""), 0);
    wasConverted = false;
    if (str.empty()) {
        res = doublecomplex(nan(""), 0);
        wasConverted = false;
    } else {
        std::wstring STR = str;
        StringHelpers::replace_all(STR, L" ", L"");
        StringHelpers::replace_all(STR, L",", L"");
        if (STR.empty()) {
            res = doublecomplex(nan(""), 0);
            wasConverted = false;
        } else {
            /* case .9 replaced by 0.9 */
            if (StringHelpers::starts_with(STR, L".")) {
                STR = L"0" + STR;
            }
            if (((STR[0] == PlusChar) || (STR[0] == LessChar)) && (STR[1] == L'.')) {
                /* case +.9 replaced by +0.9 */
                StringHelpers::replace_all(STR, L"+.", L"+0.");
                /* case -.9 replaced by -0.9 */
                StringHelpers::replace_all(STR, L"-.", L"-0.");
            }
            /* Case: 'i', '+i', '-i', and with 'j' */
            double imag = nan("");
            if (is_unit_imaginary(STR, imag)) {
                wasConverted = true;
                res = doublecomplex(0, imag);
            } else {
                double realPart;
                double imagPart;
                if (StringHelpers::contains(STR, L"i") || StringHelpers::contains(STR, L"j")) {
                    StringHelpers::replace_all(STR, L" ", L"");
                    ParseComplexValue(STR, realPart, imagPart);
                    res = doublecomplex(realPart, imagPart);
                    wasConverted = true;
                } else {
                    realPart = stringToDouble(str, wasConverted);
                    imagPart = 0;
                    if (wasConverted) {
                        res = doublecomplex(realPart, imagPart);
                        wasConverted = true;
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
