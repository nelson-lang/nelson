//=============================================================================
// Copyright (c) 2019-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4018)
#endif
//=============================================================================
#include <cmath>
#include <limits>
#include <type_traits>
#include <typeinfo>
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
// Pure element-wise conversion / saturation kernels backing ArrayOf::promoteType.
// Extracted from ArrayOf_PromoteType.cpp so that the (large) promoteType dispatch
// body is not interleaved with the per-type casting helpers. These operate only on
// raw typed buffers and ArrayOf::allocateArrayOf; they touch no ArrayOf private state.
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename TIN, typename TOUT>
inline TOUT
numeric_cast(TIN value)
{
    if constexpr (std::is_floating_point<TIN>::value && std::is_integral<TOUT>::value) {
        // Saturate with exact power-of-two bounds: TOUT max + 1 (== 2^digits) and
        // -(2^digits) are exactly representable as floating point, while TOUT max
        // itself may not be (e.g. int64 max rounds up to 2^63 as a double, which
        // made "value > max" miss the overflow and wrap around).
        const long double rounded = std::round(static_cast<long double>(value));
        if (std::isnan(rounded)) {
            return static_cast<TOUT>(0);
        }
        const long double upperBound = std::ldexp(1.0L, std::numeric_limits<TOUT>::digits);
        if (rounded >= upperBound) {
            return std::numeric_limits<TOUT>::max();
        }
        if constexpr (std::numeric_limits<TOUT>::is_signed) {
            if (rounded < -upperBound) {
                return std::numeric_limits<TOUT>::min();
            }
        } else {
            if (rounded <= 0.0L) {
                return static_cast<TOUT>(0);
            }
        }
        return static_cast<TOUT>(rounded);
    } else {
        const bool positive_overflow_possible
            = std::numeric_limits<TOUT>::max() < std::numeric_limits<TIN>::max();
        const bool negative_overflow_possible = std::numeric_limits<TIN>::is_signed
            || (std::numeric_limits<TOUT>::lowest() > std::numeric_limits<TIN>::lowest());

        // unsigned <-- unsigned
        if ((!std::numeric_limits<TOUT>::is_signed) && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        }
        // unsigned <-- signed
        else if ((!std::numeric_limits<TOUT>::is_signed) && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
            if (negative_overflow_possible && (value < 0)) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        // signed <-- unsigned
        else if (std::numeric_limits<TOUT>::is_signed && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        } else if (std::numeric_limits<TOUT>::is_signed && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
            if (negative_overflow_possible && (value < std::numeric_limits<TOUT>::lowest())) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        return static_cast<TOUT>(value);
    }
}
//=============================================================================
template <class TIN, class TOUT>
void
saturate(NelsonType classIn, NelsonType classOut, const void* pIn, void* pOut, indexType count)
{
    const TIN* sp = static_cast<const TIN*>(pIn);
    TOUT* qp = static_cast<TOUT*>(pOut);
    if (classIn != classOut) {
        bool checkNaN = false;
        if (typeid(TOUT) != typeid(single) || typeid(TOUT) != typeid(double)) {
            if (typeid(TIN) == typeid(single) || typeid(TIN) == typeid(double)) {
                checkNaN = true;
            }
        }
        if (checkNaN) {
            OMP_PARALLEL_FOR_LOOP(count)
            for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
                if (std::isnan((double)sp[i])) {
                    qp[i] = (TOUT)0;
                } else {
                    qp[i] = (TOUT)numeric_cast<TIN, TOUT>(sp[i]);
                }
            }
        } else {
            OMP_PARALLEL_FOR_LOOP(count)
            for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
                qp[i] = numeric_cast<TIN, TOUT>(sp[i]);
            }
        }
    }
}
//=============================================================================
/**
 * Promote our data to a new type, converting the data as we go.
 * Reference source or destination types cannot be converted. Rules for the other types:
 *
 * Source type
 *  - string
 *    - logical dest = (source == 0) ? 0 : 1
 *    - real dest = (double) source
 *    - complex dest = (double) source
 *  - logical
 *    - string dest = (char) source
 *    - real   dest = (double) source
 *    - complex dest = (double) source
 *  - real
 *    - string dest = (char) source
 *    - logical dest = (source == 0) ? 0 : 1
 *    - complex dest = (double) source
 *  - complex
 *    - string dest = (char) real(source)
 *    - logical dest = (real(source) == 0 && imag(source) == 0) ? 0:1
 *    - real dest = real(source)
 */
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsReal(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dstPtr[i] = (TOUT)ptr[i];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN>
charType*
promoteAsChar(NelsonType dstClass, const TIN* ptr, indexType count)
{
    charType* dstPtr = (charType*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        long double value = static_cast<long double>(ptr[i]);
        if (!std::isfinite(value)) {
            dstPtr[i] = (value > 0) ? static_cast<charType>(65535) : static_cast<charType>(0);
        } else if (value < 0) {
            dstPtr[i] = static_cast<charType>(0);
        } else if (value > 65535) {
            dstPtr[i] = static_cast<charType>(65535);
        } else {
            dstPtr[i] = static_cast<charType>(value);
        }
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsLogical(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
        dstPtr[i] = (ptr[i] == 0) ? 0 : 1;
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsLogical(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < count; ++i) {
        dstPtr[i] = (ptr[(i * 2)] || ptr[(i * 2) + 1]) ? 1 : 0;
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsReal(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i = i + 1) {
        dstPtr[i] = (TOUT)ptr[i * 2];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsComplex(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count * 2; i = i + 2) {
        dstPtr[i] = (TOUT)ptr[i];
        dstPtr[i + 1] = (TOUT)ptr[i + 1];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteAsComplex(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count, stringVector(), true);
    OMP_PARALLEL_FOR_LOOP(count)
    for (ompIndexType i = 0; i < (ompIndexType)count; i = i + 1) {
        dstPtr[i << 1] = (TOUT)ptr[i];
    }
    return dstPtr;
}
//=============================================================================
template <class TIN, class TOUT>
TOUT*
promoteComplexAsInteger(NelsonType dstClass, const TIN* ptr, indexType count)
{
    TOUT* dstPtr = (TOUT*)ArrayOf::allocateArrayOf(dstClass, count);
    bool checkNaN = false;
    if (typeid(TOUT) != typeid(single) || typeid(TOUT) != typeid(double)) {
        if (typeid(TIN) == typeid(single) || typeid(TIN) == typeid(double)) {
            checkNaN = true;
        }
    }
    if (checkNaN) {
        OMP_PARALLEL_FOR_LOOP(count)
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            if (std::isnan((double)ptr[i * 2])) {
                dstPtr[i] = (TOUT)0;
            } else {
                dstPtr[i] = (TOUT)numeric_cast<TIN, TOUT>(ptr[i * 2]);
            }
        }
    } else {
        for (ompIndexType i = 0; i < (ompIndexType)count; i++) {
            dstPtr[i] = numeric_cast<TIN, TOUT>(ptr[i * 2]);
        }
    }
    return dstPtr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
