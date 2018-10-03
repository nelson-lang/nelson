//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string.hpp>
#include <locale>
#include <math.h>
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
        if (boost::algorithm::starts_with(ToUpper(tx), "-INF")) {
            real = -std::numeric_limits<double>::infinity();
        } else if (boost::algorithm::starts_with(ToUpper(tx), "+INF")
            || boost::algorithm::starts_with(ToUpper(tx), "INF")) {
            real = std::numeric_limits<double>::infinity();
        } else {
            real = nan("");
        }
        if (boost::algorithm::ends_with(ToUpper(tx), "-INFI")) {
            imag = -std::numeric_limits<double>::infinity();
        } else if (boost::algorithm::ends_with(ToUpper(tx), "+INFI")) {
            imag = std::numeric_limits<double>::infinity();
        } else if (boost::algorithm::ends_with(ToUpper(tx), "NANI")) {
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
        boost::replace_all(STR, L" ", L"");
        boost::replace_all(STR, L",", L"");
        if (STR.empty()) {
            res = doublecomplex(nan(""), 0);
            wasConverted = false;
        } else {
            /* case .9 replaced by 0.9 */
            if (boost::algorithm::starts_with(STR, L".")) {
                STR = L"0" + STR;
            }
            if (STR.length() > 0) {
                if (((STR[0] == PlusChar) || (STR[0] == LessChar)) && (STR[1] == L'.')) {
                    /* case +.9 replaced by +0.9 */
                    boost::replace_all(STR, L"+.", L"+0.");
                    /* case -.9 replaced by -0.9 */
                    boost::replace_all(STR, L"-.", L"-0.");
                }
                /* Case: 'i', '+i', '-i', and with 'j' */
                double imag = nan("");
                if (is_unit_imaginary(STR, imag)) {
                    wasConverted = true;
                    res = doublecomplex(0, imag);
                } else {
                    double realPart;
                    double imagPart;
                    if (boost::algorithm::contains(STR, L"i")
                        || boost::algorithm::contains(STR, L"j")) {
                        boost::replace_all(STR, L" ", L"");
                        ParseComplexValue(STR, realPart, imagPart);
                    } else {
                        realPart = stringToDouble(str, wasConverted);
                        imagPart = 0;
                    }
                    res = doublecomplex(realPart, imagPart);
                    wasConverted = true;
                }
            }
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
