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
    int64 res = static_cast<int64>(0);
    char* endptr = nullptr;
    if (str[0] == '-') {
        std::string withNeg = str.substr(1);
        unsigned long long int v = strtoull(withNeg.c_str(), &endptr, 10);
        if (v > -std::numeric_limits<int64>::min()) {
            res = std::numeric_limits<int64>::min();
        } else {
            res = -static_cast<int64>(v);
        }
    } else {
        unsigned long long int v = strtoull(str.c_str(), &endptr, 10);
        if (v > std::numeric_limits<int64>::max()) {
            res = std::numeric_limits<int64>::max();
        } else {
            res = static_cast<int64>(v);
        }
    }
    return res;
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
    auto answer = fast_float::from_chars(str.data(), str.data() + str.size(), value);
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
    char* endptr = nullptr;
    unsigned long long int v = strtoull(str.c_str(), &endptr, 10);
    return static_cast<uint64>(v);
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
