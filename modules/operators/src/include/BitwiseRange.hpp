//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cmath>
#include <string>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Maps an ASSUMEDTYPE name to its integer class, NLS_UNKNOWN otherwise.
inline NelsonType
bitwiseAssumedClass(const std::wstring& name)
{
    if (name == L"int8") {
        return NLS_INT8;
    }
    if (name == L"uint8") {
        return NLS_UINT8;
    }
    if (name == L"int16") {
        return NLS_INT16;
    }
    if (name == L"uint16") {
        return NLS_UINT16;
    }
    if (name == L"int32") {
        return NLS_INT32;
    }
    if (name == L"uint32") {
        return NLS_UINT32;
    }
    if (name == L"int64") {
        return NLS_INT64;
    }
    if (name == L"uint64") {
        return NLS_UINT64;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
// Range of an integer class as [low, high) in double.
inline void
bitwiseClassRange(NelsonType cls, double& low, double& high)
{
    switch (cls) {
    case NLS_INT8:
        low = -128.0;
        high = 128.0;
        break;
    case NLS_UINT8:
        low = 0.0;
        high = 256.0;
        break;
    case NLS_INT16:
        low = -32768.0;
        high = 32768.0;
        break;
    case NLS_UINT16:
        low = 0.0;
        high = 65536.0;
        break;
    case NLS_INT32:
        low = -2147483648.0;
        high = 2147483648.0;
        break;
    case NLS_UINT32:
        low = 0.0;
        high = 4294967296.0;
        break;
    case NLS_INT64:
        low = -9223372036854775808.0;
        high = 9223372036854775808.0;
        break;
    case NLS_UINT64:
    default:
        low = 0.0;
        high = 18446744073709551616.0;
        break;
    }
}
//=============================================================================
inline bool
isDoubleInBitRange(double v, NelsonType cls)
{
    double low, high;
    bitwiseClassRange(cls, low, high);
    return v >= low && v < high && v == std::floor(v);
}
//=============================================================================
// Raises an error unless every value of a real double array is whole and fits cls.
inline void
checkDoubleInBitRange(const ArrayOf& X, NelsonType cls, bool withAssumedType)
{
    double low, high;
    bitwiseClassRange(cls, low, high);
    const double* p = static_cast<const double*>(X.dataPointer());
    ompIndexType len = (ompIndexType)X.elementCount();
    int bad = 0;
#if WITH_OPENMP
#pragma omp parallel for reduction(| : bad) if (NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled() && (uint64_t)len > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold())
#endif
    for (ompIndexType k = 0; k < len; ++k) {
        double v = p[k];
        bad |= !((v >= low) && (v < high) && (v == std::floor(v)));
    }
    if (!bad) {
        return;
    }
    if (withAssumedType) {
        Error(L"Nelson:operators:invalidBitInput:doubleInputsIntegerValuesRangeASSUMEDTYPE",
            L"Double inputs must have integer values in the range of ASSUMEDTYPE.");
    }
    Error(L"Nelson:operators:bitDoubleOutOfRange",
        L"Double inputs must be nonnegative whole numbers less than 2^64.");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
