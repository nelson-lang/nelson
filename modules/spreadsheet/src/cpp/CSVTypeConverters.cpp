//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CSVTypeConverters.hpp"
#include <fast_float/fast_float.h>
#include <regex>
//=============================================================================
namespace Nelson {
//=============================================================================
void
ConvertStringToDouble(const std::string& pStr, double& Val)
{
    if (ConvertToDouble(pStr, Val)) {
        return;
    }
    Val = std::nan("");
}
//=============================================================================
void
ConvertStringToSingle(const std::string& pStr, single& Val)
{
    double value;
    if (ConvertToDouble(pStr, value)) {
        Val = (single)value;
        return;
    }
    Val = (single)std::nan("");
}
//=============================================================================
void
ConvertStringToDoubleComplex(const std::string& pStr, std::complex<double>& pVal)
{
    if (ConvertToDoubleComplex(pStr, pVal)) {
        return;
    }
    double value;
    if (ConvertToDouble(pStr, value)) {
        pVal.real(value);
        pVal.imag(0.);
        return;
    }
    pVal.real(std::nan(""));
    pVal.imag(0.);
}
//=============================================================================
void
ConvertStringToSingleComplex(const std::string& pStr, std::complex<single>& Val)
{
    std::complex<double> value;
    ConvertStringToDoubleComplex(pStr, value);
    Val = (std::complex<single>)value;
}
//=============================================================================
template <typename T>
void
ConvertStringToInteger(const std::string& pStr, T& Val)
{
    double dval;
    if (ConvertToDouble(pStr, dval)) {
        if (std::isnan(dval)) {
            Val = static_cast<T>(0.);
            return;
        }
        if (std::isinf(dval)) {
            if (dval > 0) {
                Val = std::numeric_limits<T>::max();
            } else {
                Val = std::numeric_limits<T>::min();
            }
            return;
        }
        if (dval < static_cast<double>(std::numeric_limits<T>::min())) {
            Val = std::numeric_limits<T>::min();
            return;
        }
        if (dval > static_cast<double>(std::numeric_limits<T>::max())) {
            Val = std::numeric_limits<T>::max();
            return;
        }
        Val = static_cast<T>(dval);
        return;
    }
    Val = static_cast<T>(0.);
}
//=============================================================================
void
ConvertStringToInt8(const std::string& pStr, int8& Val)
{
    ConvertStringToInteger<int8>(pStr, Val);
}
//=============================================================================
void
ConvertStringToInt16(const std::string& pStr, int16& Val)
{
    ConvertStringToInteger<int16>(pStr, Val);
}
//=============================================================================
void
ConvertStringToInt32(const std::string& pStr, int32& Val)
{
    ConvertStringToInteger<int32>(pStr, Val);
}
//=============================================================================
void
ConvertStringToInt64(const std::string& pStr, int64& Val)
{
    ConvertStringToInteger<int64>(pStr, Val);
}
//=============================================================================
void
ConvertStringToUInt8(const std::string& pStr, uint8& Val)
{
    ConvertStringToInteger<uint8>(pStr, Val);
}
//=============================================================================
void
ConvertStringToUInt16(const std::string& pStr, uint16& Val)
{
    ConvertStringToInteger<uint16>(pStr, Val);
}
//=============================================================================
void
ConvertStringToUInt32(const std::string& pStr, uint32& Val)
{
    ConvertStringToInteger<uint32>(pStr, Val);
}
//=============================================================================
void
ConvertStringToUInt64(const std::string& pStr, uint64& Val)
{
    ConvertStringToInteger<uint64>(pStr, Val);
}
//=============================================================================
struct ComplexPatterns
{
    // Regex for special values (Inf, NaN)
    static inline const std::string special_re = R"((?:[Nn][Aa][Nn]|[Ii][Nn][Ff]))";

    // Full regex patterns combining numbers and special values
    static inline const std::regex full_complex { R"(([+-]?(?:\d*\.?\d+|)" + special_re
            + R"())([+-](?:\d*\.?\d+|)" + special_re + R"())[ij])",
        std::regex::optimize };
    static inline const std::regex real_only {
        R"(([+-]?(?:\d*\.?\d+|)" + special_re + R"())(?![ij]))", std::regex::optimize
    };
    static inline const std::regex imag_only { R"(([+-]?(?:\d*\.?\d+|)" + special_re + R"())[ij])",
        std::regex::optimize };
};
//=============================================================================
bool
ConvertToDouble(const std::string& pStr, double& pVal)
{
    if (pStr.empty()) {
        return false;
    }
    fast_float::parse_options options { fast_float::chars_format::fortran };

    const char* first = pStr.data();
    const char* last = pStr.data() + pStr.size();
    if (pStr[0] == '+') {
        first += 1;
    }

    auto answer = fast_float::from_chars_advanced(first, last, pVal, options);

    if (answer.ec != std::errc() || answer.ptr != last) {
        return false;
    }
    return true;
}
//=============================================================================
bool
ConvertToDoubleComplex(const std::string& str, std::complex<double>& pVal)
{
    char lastChar = '\0';
    if (!str.empty()) {
        lastChar = str.back();
    }
    if ((lastChar != '\0') && lastChar == 'I' || lastChar == 'J' || lastChar == 'i'
        || lastChar == 'j') {
        std::smatch matches;
        if (std::regex_match(str, matches, ComplexPatterns::full_complex)) {
            bool isNegativeReal = false;
            bool isNegativeImag = false;
            std::string realStr = matches[1].str();
            std::string imagStr = matches[2].str();
            if (imagStr.front() == L'+' || imagStr.front() == L'-') {
                if (imagStr.front() == L'-') {
                    isNegativeImag = true;
                }
                imagStr.erase(0, 1);
            }

            double realPart, imagPart;

            bool res = ConvertToDouble(realStr, realPart);
            if (!res) {
                return res;
            }
            res = ConvertToDouble(imagStr, imagPart);
            if (!res) {
                return res;
            }
            if (isNegativeReal) {
                realPart = -realPart;
            }
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { realPart, imagPart };
            return true;
        } else if (std::regex_match(str, matches, ComplexPatterns::imag_only)) {
            bool isNegativeImag = false;
            std::string imagStr = matches[1].str();
            if (imagStr.front() == L'+' || imagStr.front() == L'-') {
                if (imagStr.front() == L'-') {
                    isNegativeImag = true;
                }
                imagStr.erase(0, 1);
            }

            double imagPart;
            bool res = ConvertToDouble(imagStr, imagPart);
            if (!res) {
                return false;
            }
            if (isNegativeImag) {
                imagPart = -imagPart;
            }
            pVal = { 0., imagPart };
            return true;
        } else {
            return false;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
