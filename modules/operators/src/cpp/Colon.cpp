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
#include <type_traits>
#include "Colon.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The step is signed whatever the integer class: uint8(1):-1:uint8(0) counts
// down. Differences and products are computed in the unsigned companion type
// (exact, no overflow); 64-bit classes keep every bit.
template <class T>
static ArrayOf
integer_colon(NelsonType destinationClass, T low, T high, int64 step)
{
    if (step == 0 || (low < high && step < 0) || (low > high && step > 0)) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(destinationClass);
        return res;
    }
    using UT = typename std::make_unsigned<T>::type;
    uint64 absStep
        = (step < 0) ? (static_cast<uint64>(-(step + 1)) + 1) : static_cast<uint64>(step);
    uint64 range = (low <= high)
        ? static_cast<uint64>(static_cast<UT>(static_cast<UT>(high) - static_cast<UT>(low)))
        : static_cast<uint64>(static_cast<UT>(static_cast<UT>(low) - static_cast<UT>(high)));
    auto n = static_cast<indexType>(range / absStep) + 1;
    T* pV = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
        auto offset = static_cast<UT>(static_cast<uint64>(k) * absStep);
        pV[k] = (step < 0) ? static_cast<T>(static_cast<UT>(static_cast<UT>(low) - offset))
                           : static_cast<T>(static_cast<UT>(static_cast<UT>(low) + offset));
    }
    return ArrayOf(destinationClass, Dimensions(1, n), pV);
}
//=============================================================================
static ArrayOf
char_colon(charType low, charType high, int64 step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }

    if ((low < high && step < 0) || (low > high && step > 0)) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }

    int64 range = static_cast<int64>(high) - static_cast<int64>(low);
    int64 elementCount = (range / step) + 1;

    if (elementCount <= 0) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }

    auto n = static_cast<indexType>(elementCount);

    charType* pV = nullptr;
    try {
        pV = static_cast<charType*>(ArrayOf::allocateArrayOf(NLS_CHAR, n, stringVector(), false));
        if (!pV) {
            ArrayOf res = ArrayOf::empty(1, 0);
            res.promoteType(NLS_CHAR);
            return res;
        }
    } catch (const std::exception&) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }

#if defined(WITH_OPENMP)
#pragma omp parallel for schedule(static) if (n > OMP_DEFAULT_THRESHOLD)
#endif
    for (ompIndexType k = 0; k < static_cast<ompIndexType>(n); k++) {
        int64 offset = static_cast<int64>(k) * step;
        int64 value = static_cast<int64>(low) + offset;

        if (value >= std::numeric_limits<charType>::min()
            && value <= std::numeric_limits<charType>::max()) {
            pV[k] = static_cast<charType>(value);
        } else {
            pV[k] = 0;
        }
    }

    try {
        return ArrayOf(NLS_CHAR, Dimensions(1, n), pV);
    } catch (const std::exception&) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }
}
//=============================================================================
template <class T>
static ArrayOf
real_colon(NelsonType destinationClass, T low, T high, T step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::empty(1, 0);
        res.promoteType(destinationClass);
        return res;
    }
    if (std::isnan(low) || std::isnan(high) || std::isnan(step)) {
        Dimensions Cdim(1, 1);
        if (destinationClass == NLS_SINGLE) {
            return ArrayOf::singleScalar(nanf(""));
        }
        return ArrayOf::doubleScalar(nan(""));
    }
    if (!std::isfinite(low) || !std::isfinite(high) || !std::isfinite(step)) {
        if (!std::isfinite(low) && !std::isfinite(step)) {
            if ((low > 0 && step < 0) || (low < 0 && step > 0)) {
                if (destinationClass == NLS_SINGLE) {
                    return ArrayOf::singleScalar(nanf(""));
                }
                return ArrayOf::doubleScalar(nan(""));
            }
        }
        if (!std::isfinite(low)) {
            if (low > 0) {
                Dimensions dims(1, 0);
                return ArrayOf::empty(dims);
            }
        }
        if (!std::isfinite(step)) {
            if (step < 0) {
                Dimensions dims(1, 0);
                return ArrayOf::empty(dims);
            }
            if (destinationClass == NLS_SINGLE) {
                return ArrayOf::singleScalar((single)low);
            }
            return ArrayOf::doubleScalar((double)low);
        }
        if (!std::isfinite(high)) {
            if (high < 0) {
                Dimensions dims(1, 0);
                return ArrayOf::empty(dims);
            }
        }
        Error(L"Nelson:operators:invalidRange", L"Invalid range.");
    }
    if (low < high) {
        if (step < 0) {
            ArrayOf res = ArrayOf::empty(1, 0);
            res.promoteType(destinationClass);
            return res;
        }
    }
    if (low > high) {
        if (step > 0) {
            ArrayOf res = ArrayOf::empty(1, 0);
            res.promoteType(destinationClass);
            return res;
        }
    }
    T dn = (T)((((high - low) / step) + 1));
    indexType n;
    T truncatedStep = T(int(step));
    if (truncatedStep == step) {
        n = (indexType)std::floor(dn);
    } else {
        T nMax = (T)(std::nextafter(high - low, high - low + step) / std::nextafter(step, 0));
        T nSize = std::floor(nMax);
        nSize++;
        n = (indexType)nSize;
    }
    T* pV = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
    ArrayOf V = ArrayOf(destinationClass, Dimensions(1, n), pV);
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
        pV[k] = low + (k * step);
    }
    return V;
}
//=============================================================================
static void
checkColonOperandIsScalarOrEmpty(const ArrayOf& operand)
{
    if (!operand.isEmpty() && !operand.isScalar()) {
        Error(L"Nelson:operators:invalidColonOperand", L"Colon operands must be real scalars.");
    }
}
//=============================================================================
ArrayOf
Colon(const ArrayOf& J, const ArrayOf& K, bool& needOverload)
{
    ArrayOf I = ArrayOf::doubleScalar(1);
    if (!J.isCharacterArray()) {
        I.promoteType(J.dataClass());
    }
    return Colon(J, I, K, needOverload);
}
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
Colon(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K, bool& needOverload)
{
    needOverload = false;
    checkColonOperandIsScalarOrEmpty(J);
    checkColonOperandIsScalarOrEmpty(I);
    checkColonOperandIsScalarOrEmpty(K);
    switch (J.dataClass()) {
    case NLS_UINT8: {
        int64 step;
        uint8 low;
        uint8 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            low = static_cast<uint8>(1);
            high = static_cast<uint8>(0);
            step = 1;
        } else {
            step = I.asInt64Scalar(true);
            low = J.asUint8Scalar(true);
            high = K.asUint8Scalar(true);
        }
        return integer_colon<uint8>(NLS_UINT8, low, high, step);
    } break;
    case NLS_INT8: {
        int64 step;
        int8 low;
        int8 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            low = static_cast<int8>(1);
            high = static_cast<int8>(0);
            step = 1;
        } else {
            step = I.asInt64Scalar(true);
            low = J.asInt8Scalar(true);
            high = K.asInt8Scalar(true);
        }
        return integer_colon<int8>(NLS_INT8, low, high, step);
    } break;
    case NLS_UINT16: {
        int64 step;
        uint16 low;
        uint16 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<uint16>(1);
            high = static_cast<uint16>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asUint16Scalar(true);
            high = K.asUint16Scalar(true);
        }
        return integer_colon<uint16>(NLS_UINT16, low, high, step);
    } break;
    case NLS_INT16: {
        int64 step;
        int16 low;
        int16 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<int16>(1);
            high = static_cast<int16>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asInt16Scalar(true);
            high = K.asInt16Scalar(true);
        }
        return integer_colon<int16>(NLS_INT16, low, high, step);
    } break;
    case NLS_UINT32: {
        int64 step;
        uint32 low;
        uint32 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<uint32>(1);
            high = static_cast<uint32>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asUint32Scalar(true);
            high = K.asUint32Scalar(true);
        }
        return integer_colon<uint32>(NLS_UINT32, low, high, step);
    } break;
    case NLS_INT32: {
        int64 step;
        int32 low;
        int32 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<int32>(1);
            high = static_cast<int32>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asInt32Scalar(true);
            high = K.asInt32Scalar(true);
        }
        return integer_colon<int32>(NLS_INT32, low, high, step);
    } break;
    case NLS_UINT64: {
        int64 step;
        uint64 low;
        uint64 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<uint64>(1);
            high = static_cast<uint64>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asUint64Scalar(true);
            high = K.asUint64Scalar(true);
        }
        return integer_colon<uint64>(NLS_UINT64, low, high, step);
    } break;
    case NLS_INT64: {
        int64 step;
        int64 low;
        int64 high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = 1;
            low = static_cast<int64>(1);
            high = static_cast<int64>(0);
        } else {
            step = I.asInt64Scalar(true);
            low = J.asInt64Scalar(true, true);
            high = K.asInt64Scalar(true, true);
        }
        return integer_colon<int64>(NLS_INT64, low, high, step);
    } break;
    case NLS_SINGLE: {
        single step;
        single low;
        single high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = static_cast<single>(1);
            low = static_cast<single>(1);
            high = static_cast<single>(0);
        } else {
            step = I.asSingleScalar(true);
            low = J.asSingleScalar(true);
            high = K.asSingleScalar(true);
        }
        return real_colon<single>(NLS_SINGLE, low, high, step);
    } break;
    case NLS_DOUBLE: {
        double step;
        double low;
        double high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = static_cast<double>(1);
            low = static_cast<double>(1);
            high = static_cast<double>(0);
        } else {
            step = I.asDoubleScalar(true);
            low = J.asDoubleScalar(true);
            high = K.asDoubleScalar(true);
        }
        return real_colon<double>(NLS_DOUBLE, low, high, step);
    } break;
    case NLS_SCOMPLEX: {
        single step;
        single low;
        single high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = static_cast<single>(1);
            low = static_cast<single>(1);
            high = static_cast<single>(0);
        } else {
            ArrayOf JJ(J);
            ArrayOf KK(K);
            ArrayOf II(I);
            II.promoteType(NLS_SINGLE);
            JJ.promoteType(NLS_SINGLE);
            KK.promoteType(NLS_SINGLE);
            step = II.asSingleScalar(true);
            low = JJ.asSingleScalar(true);
            high = KK.asSingleScalar(true);
        }
        return real_colon<single>(NLS_SINGLE, low, high, step);
    } break;
    case NLS_DCOMPLEX: {
        double step;
        double low;
        double high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = static_cast<double>(1);
            low = static_cast<double>(1);
            high = static_cast<double>(0);
        } else {
            ArrayOf II(I);
            ArrayOf JJ(J);
            ArrayOf KK(K);
            II.promoteType(NLS_DOUBLE);
            JJ.promoteType(NLS_DOUBLE);
            KK.promoteType(NLS_DOUBLE);
            step = II.asDoubleScalar(true);
            low = JJ.asDoubleScalar(true);
            high = KK.asDoubleScalar(true);
        }
        return real_colon<double>(NLS_DOUBLE, low, high, step);
    } break;
    case NLS_CHAR: {
        int64 step;
        charType low;
        charType high;
        if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
            step = static_cast<charType>(1);
            low = static_cast<charType>(1);
            high = static_cast<charType>(0);
        } else {
            step = I.asInt64Scalar(true);
            std::wstring content = J.asWideString();
            low = content[0];
            content = K.asWideString();
            high = content[0];
        }
        return char_colon(low, high, step);
    } break;
    default: {
        needOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
