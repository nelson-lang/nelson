//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <algorithm>
#include <cstdlib>
#include <cmath>
#include <fast_float/fast_float.h>
#include "TextToNumber.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int64
textToInt64Converter(const std::string& str)
{
    int64 result = static_cast<int64>(0);
    size_t idx;
    size_t startPos = (str[0] == '-') ? 1 : 0;
    unsigned long long int v = std::stoull(str.substr(startPos), &idx, 10);

    try {
        if (startPos == 1) {
            if (v > static_cast<unsigned long long int>(std::numeric_limits<int64>::max()) + 1ULL) {
                result = std::numeric_limits<int64>::min();
            } else {
                result = -static_cast<int64>(v);
            }
        } else {
            if (v > (unsigned long long int)(std::numeric_limits<int64>::max())) {
                result = std::numeric_limits<int64>::max();
            } else {
                result = static_cast<int64>(v);
            }
        }
    } catch (const std::out_of_range&) {
        if (startPos == 1) {
            result = std::numeric_limits<int64>::min();
        } else {
            result = std::numeric_limits<int64>::max();
        }
    } catch (const std::invalid_argument&) {
        throw;
    }
    return result;
}
//=============================================================================
template <class T>
static T
saturate(int64 valuei64)
{
    T res;
    if (valuei64 >= static_cast<int64>(std::numeric_limits<T>::max())) {
        res = std::numeric_limits<T>::max();
    } else if (valuei64 <= static_cast<int64>(std::numeric_limits<T>::min())) {
        res = std::numeric_limits<T>::min();
    } else {
        res = static_cast<T>(valuei64);
    }
    return res;
}
//=============================================================================
double
textToDouble(const std::string& str)
{
    double value;
    fast_float::parse_options options { fast_float::chars_format::fortran };
    auto answer
        = fast_float::from_chars_advanced(str.data(), str.data() + str.size(), value, options);
    return std::abs(value);
}
//=============================================================================
single
textToSingle(const std::string& str)
{
    return static_cast<single>(textToDouble(str));
}
//=============================================================================
uint8
textToUint8(const std::string& str)
{
    return saturate<uint8>(textToInt64Converter(str));
}
//=============================================================================
int8
textToInt8(const std::string& str)
{
    return saturate<int8>(textToInt64Converter(str));
}
//=============================================================================
uint16
textToUint16(const std::string& str)
{
    return saturate<uint16>(textToInt64Converter(str));
}
//=============================================================================
int16
textToInt16(const std::string& str)
{

    return saturate<int16>(textToInt64Converter(str));
}
//=============================================================================
uint32
textToUint32(const std::string& str)
{
    return saturate<uint32>(textToInt64Converter(str));
}
//=============================================================================
int32
textToInt32(const std::string& str)
{
    return saturate<int32>(textToInt64Converter(str));
}
//=============================================================================
int64
textToInt64(const std::string& str)
{
    return textToInt64Converter(str);
}
//=============================================================================
uint64
textToUint64(const std::string& str)
{
    size_t idx;
    unsigned long long int v = 0;
    try {
        v = std::stoull(str, &idx, 10);
    } catch (const std::out_of_range&) {
        size_t startPos = (str[0] == '-') ? 1 : 0;
        if (startPos == 1) {
            return std::numeric_limits<uint64>::min();
        } else {
            return std::numeric_limits<uint64>::max();
        }
    } catch (const std::invalid_argument&) {
        throw;
    }
    return static_cast<uint64>(v);
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
