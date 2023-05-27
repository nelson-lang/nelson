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
#include <Eigen/Sparse>
#include "Colon.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "Warning.hpp"
#include "nlsBuildConfig.h"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSignedInteger(NelsonType destinationClass)
{
    return (destinationClass == NLS_INT8 || destinationClass == NLS_INT16
        || destinationClass == NLS_INT32 || destinationClass == NLS_INT64);
}
//=============================================================================
template <class T>
static ArrayOf
integer_colon(NelsonType destinationClass, T low, T high, T step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(destinationClass);
        return res;
    }
    if (low < high) {
        if (isSignedInteger(destinationClass)) {
            if (step < 0) {
                ArrayOf res = ArrayOf::emptyConstructor(1, 0);
                res.promoteType(destinationClass);
                return res;
            }
        }
    }
    if (low > high) {
        if (step > 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(destinationClass);
            return res;
        }
    }
    double dn = (double)((((high - low) / step) + 1));
    indexType n = (indexType)std::trunc(dn);
    T* pV = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
        pV[k] = (T)(low + (k * step));
    }
    return ArrayOf(destinationClass, Dimensions(1, n), pV);
}
//=============================================================================
static ArrayOf
char_colon(charType low, charType high, charType step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }
    if ((low < high) && (step < 0)) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }
    if ((low > high) && (step > 0)) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(NLS_CHAR);
        return res;
    }
    auto dn = static_cast<double>((((high - low) / step) + 1));
    auto n = static_cast<indexType>(std::trunc(dn));
    charType* pV = (charType*)ArrayOf::allocateArrayOf(NLS_CHAR, n, stringVector(), false);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
        pV[k] = (charType)(low + (k * step));
    }
    return ArrayOf(NLS_CHAR, Dimensions(1, n), pV);
}
//=============================================================================
template <class T>
static ArrayOf
real_colon(NelsonType destinationClass, T low, T high, T step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(destinationClass);
        return res;
    }
    if (std::isnan(low) || std::isnan(high) || std::isnan(step)) {
        Dimensions Cdim(1, 1);
        if (destinationClass == NLS_SINGLE) {
            return ArrayOf::singleConstructor(nanf(""));
        }
        return ArrayOf::doubleConstructor(nan(""));
    }
    if (!std::isfinite(low) || !std::isfinite(high) || !std::isfinite(step)) {
        if (!std::isfinite(low) && !std::isfinite(step)) {
            if ((low > 0 && step < 0) || (low < 0 && step > 0)) {
                if (destinationClass == NLS_SINGLE) {
                    return ArrayOf::singleConstructor(nanf(""));
                }
                return ArrayOf::doubleConstructor(nan(""));
            }
        }
        if (!std::isfinite(low)) {
            if (low > 0) {
                Dimensions dims(1, 0);
                return ArrayOf::emptyConstructor(dims);
            }
        }
        if (!std::isfinite(step)) {
            if (step < 0) {
                Dimensions dims(1, 0);
                return ArrayOf::emptyConstructor(dims);
            }
            if (destinationClass == NLS_SINGLE) {
                return ArrayOf::singleConstructor((single)low);
            }
            return ArrayOf::doubleConstructor((double)low);
        }
        if (!std::isfinite(high)) {
            if (high < 0) {
                Dimensions dims(1, 0);
                return ArrayOf::emptyConstructor(dims);
            }
        }
        Error(_W("Invalid range."));
    }
    if (low < high) {
        if (step < 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(destinationClass);
            return res;
        }
    }
    if (low > high) {
        if (step > 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
        pV[k] = low + (k * step);
    }
    return V;
}
//=============================================================================
template <class T>
ArrayOf
ColonInteger(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K, NelsonType destinationType)
{
    bool warningArrayAsScalar = false;
    T step;
    T low;
    T high;
    if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
        low = static_cast<T>(1);
        high = static_cast<T>(0);
        step = static_cast<T>(1);
    } else {
        if (!I.isScalar() || !J.isScalar() || !K.isScalar()) {
            warningArrayAsScalar = true;
        }
        ArrayOf _I(I);
        _I.promoteType(destinationType);
        ArrayOf _J(J);
        _J.promoteType(destinationType);
        ArrayOf _K(K);
        _K.promoteType(destinationType);

        T* pStep = (T*)_I.getDataPointer();
        step = pStep[0];
        T* pLow = (T*)_J.getDataPointer();
        low = pLow[0];
        T* pHigh = (T*)_K.getDataPointer();
        high = pHigh[0];
    }
    if (warningArrayAsScalar) {
        Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
    }
    return integer_colon<T>(destinationType, low, high, step);
}
//=============================================================================
template <class T>
T
getSparseScalar(const ArrayOf& A)
{
    T value;
    if (A.isComplex()) {
        Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)A.getSparseDataPointer();

        const std::complex<T>* values = spMat->valuePtr();
        value = values[0].real();
    } else {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<T, 0, signedIndexType>*)A.getSparseDataPointer();

        const T* values = spMat->valuePtr();
        value = values[0];
    }
    return value;
}
//=============================================================================
template <class T>
ArrayOf
ColonReal(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K, NelsonType destinationType)
{
    bool warningArrayAsScalar = false;
    T step = (T)0;
    T low = (T)0;
    T high = (T)0;
    if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
        low = static_cast<T>(1);
        high = static_cast<T>(0);
        step = static_cast<T>(1);
    } else {
        if (!I.isScalar() || !J.isScalar() || !K.isScalar()) {
            warningArrayAsScalar = true;
        }
        ArrayOf _I(I);
        _I.promoteType(destinationType);
        if (_I.isSparse()) {
            step = getSparseScalar<T>(_I);
        } else {
            T* pStep = (T*)_I.getDataPointer();
            step = pStep[0];
        }
        ArrayOf _J(J);
        _J.promoteType(destinationType);
        if (_J.isSparse()) {
            low = getSparseScalar<T>(_J);
        } else {
            T* pLow = (T*)_J.getDataPointer();
            low = pLow[0];
        }
        ArrayOf _K(K);
        _K.promoteType(destinationType);
        if (_K.isSparse()) {
            high = getSparseScalar<T>(_K);
        } else {
            T* pHigh = (T*)_K.getDataPointer();
            high = pHigh[0];
        }
    }
    if (warningArrayAsScalar) {
        Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
    }
    return real_colon<T>(destinationType, low, high, step);
}
//=============================================================================
template <class T>
ArrayOf
ColonChar(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K)
{
    bool warningArrayAsScalar = false;
    charType step;
    charType low;
    charType high;
    if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
        step = static_cast<charType>(1);
        low = static_cast<charType>(1);
        high = static_cast<charType>(0);
    } else {
        std::wstring content = I.getContentAsWideString();
        step = content[0];
        content = J.getContentAsWideString();
        low = content[0];
        content = K.getContentAsWideString();
        high = content[0];
        if (!I.isScalar() || !J.isScalar() || !K.isScalar()) {
            warningArrayAsScalar = true;
        }
    }
    if (warningArrayAsScalar) {
        Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
    }
    return char_colon(low, high, step);
}
//=============================================================================
ArrayOf
Colon(const ArrayOf& J, const ArrayOf& K)
{
    ArrayOf I = ArrayOf::doubleConstructor(1);
    return Colon(J, I, K);
}
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
Colon(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K)
{
    switch (J.getDataClass()) {
    case NLS_UINT8: {
        return ColonInteger<uint8>(J, I, K, NLS_UINT8);
    } break;
    case NLS_INT8: {
        return ColonInteger<int8>(J, I, K, NLS_INT8);
    } break;
    case NLS_UINT16: {
        return ColonInteger<uint16>(J, I, K, NLS_UINT16);
    } break;
    case NLS_INT16: {
        return ColonInteger<int16>(J, I, K, NLS_INT16);
    } break;
    case NLS_UINT32: {
        return ColonInteger<uint32>(J, I, K, NLS_UINT32);
    } break;
    case NLS_INT32: {
        return ColonInteger<int32>(J, I, K, NLS_INT32);
    } break;
    case NLS_UINT64: {
        return ColonInteger<uint64>(J, I, K, NLS_UINT64);
    } break;
    case NLS_INT64: {
        return ColonInteger<int64>(J, I, K, NLS_INT64);
    } break;
    case NLS_SCOMPLEX:
    case NLS_SINGLE: {
        return ColonReal<single>(J, I, K, NLS_SINGLE);
    } break;
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        return ColonReal<double>(J, I, K, NLS_DOUBLE);
    } break;
    case NLS_CHAR: {
        return ColonChar<charType>(J, I, K);
    } break;
    default: {
        std::string overloadName = ClassName(J) + "_colon";
        Error(_("function") + " " + overloadName + " " + _("undefined."));
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
