//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "TruncateFunctions.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum TRUNCATE_LEVEL
{
    ROUND,
    CEIL,
    FIX,
    FLOOR
};
//=============================================================================
template <class T>
static void
oTrunc(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::trunc((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oRound(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::round((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oFloor(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::floor((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
template <class T>
static void
oCeil(indexType len, const T* pIn, T* pOut)
{
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType i = 0; i < (ompIndexType)(len); i++) {
        if (std::isfinite((T)pIn[i])) {
            pOut[i] = std::ceil((T)pIn[i]);
        } else {
            pOut[i] = (T)pIn[i];
        }
    }
}
//=============================================================================
static ArrayOf
FixSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsTrunc((MKL_INT)(len * 2), dp, ptr);
#else
    oTrunc<single>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdTrunc((MKL_INT)(len * 2), dp, ptr);
#else
    oTrunc<double>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsTrunc((MKL_INT)len, dp, ptr);
#else
    oTrunc<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FixDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdTrunc((MKL_INT)len, dp, ptr);
#else
    oTrunc<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdRound((MKL_INT)(len * 2), dp, ptr);
#else
    oRound<double>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsRound((MKL_INT)(len * 2), dp, ptr);
#else
    oRound<single>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdRound((MKL_INT)len, dp, ptr);
#else
    oRound<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
RoundSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsRound((MKL_INT)len, dp, ptr);
#else
    oRound<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsFloor((MKL_INT)(len * 2), dp, ptr);
#else
    oFloor<single>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdFloor((MKL_INT)(len * 2), dp, ptr);
#else
    oFloor<double>(len * 2, dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsFloor((MKL_INT)len, dp, ptr);
#else
    oFloor<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
FloorDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdFloor((MKL_INT)len, dp, ptr);
#else
    oFloor<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilSingleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsCeil((MKL_INT)(len * 2), dp, ptr);
#else
    oCeil<single>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_SCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilDoubleComplex(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdCeil((MKL_INT)(len * 2), dp, ptr);
#else
    oCeil<double>((len * 2), dp, ptr);
#endif
    return ArrayOf(NLS_DCOMPLEX, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilSingle(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, len, stringVector(), false);
    const single* dp = (const single*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vsCeil((MKL_INT)len, dp, ptr);
#else
    oCeil<single>(len, dp, ptr);
#endif
    return ArrayOf(NLS_SINGLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static ArrayOf
CeilDouble(const ArrayOf& arrayIn)
{
    indexType len = arrayIn.getElementCount();
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
    const double* dp = (const double*)arrayIn.getDataPointer();
#if defined(_NLS_WITH_VML)
    vdCeil((MKL_INT)len, dp, ptr);
#else
    oCeil<double>(len, dp, ptr);
#endif
    return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), ptr);
}
//=============================================================================
static std::wstring
getNotSupportedTypeMessage(const ArrayOf& arrayIn, TRUNCATE_LEVEL level)
{
    std::wstring name;
    ClassName(arrayIn, name);
    std::wstring functionName;
    switch (level) {
    case TRUNCATE_LEVEL::CEIL:
        functionName = L"ceil";
        break;
    case TRUNCATE_LEVEL::ROUND:
        functionName = L"round";
        break;
    case TRUNCATE_LEVEL::FIX:
        functionName = L"fix";
        break;
    case TRUNCATE_LEVEL::FLOOR:
        functionName = L"floor";
        break;
    }
    return _W("Undefined function '") + name + L"_" + functionName + L"'";
}
//=============================================================================
static ArrayOf
Truncate(const ArrayOf& arrayIn, TRUNCATE_LEVEL level)
{
    if (arrayIn.isSparse()) {
        Error(getNotSupportedTypeMessage(arrayIn, level));
    }
    switch (arrayIn.getDataClass()) {
    case NLS_SCOMPLEX: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixSingleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorSingleComplex(arrayIn);
        } break;
        }
    } break;
    case NLS_DCOMPLEX: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixDoubleComplex(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorDoubleComplex(arrayIn);
        } break;
        }
    } break;
    case NLS_LOGICAL: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (logical*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = (dp[i] == 0 ? 0 : 1);
        }
        return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_SINGLE: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixSingle(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorSingle(arrayIn);
        } break;
        }
    } break;
    case NLS_DOUBLE: {
        switch (level) {
        case TRUNCATE_LEVEL::CEIL: {
            return CeilDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::ROUND: {
            return RoundDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FIX: {
            return FixDouble(arrayIn);
        } break;
        case TRUNCATE_LEVEL::FLOOR: {
            return FloorDouble(arrayIn);
        } break;
        }
    }
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        return arrayIn;
    } break;
    case NLS_CHAR: {
        size_t len = arrayIn.getElementCount();
        void* ptr = ArrayOf::allocateArrayOf(NLS_DOUBLE, len, stringVector(), false);
        auto* rp = static_cast<double*>(ptr);
        auto* dp = (charType*)arrayIn.getDataPointer();
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
            rp[i] = static_cast<double>(dp[i]);
        }
        return ArrayOf(NLS_DOUBLE, arrayIn.getDimensions(), rp);
    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        Error(getNotSupportedTypeMessage(arrayIn, level));
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
Round(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::ROUND);
}
//=============================================================================
ArrayOf
Ceil(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::CEIL);
}
//=============================================================================
ArrayOf
Floor(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FLOOR);
}
//=============================================================================
ArrayOf
Fix(const ArrayOf& arrayIn)
{
    return Truncate(arrayIn, TRUNCATE_LEVEL::FIX);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
