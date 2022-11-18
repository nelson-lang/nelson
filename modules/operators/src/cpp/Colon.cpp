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
#include "Colon.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "Warning.hpp"
#include "nlsBuildConfig.h"
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
ArrayOf
Colon(const ArrayOf& J, const ArrayOf& K, bool& needToOverload)
{
    ArrayOf I = ArrayOf::doubleConstructor(1);
    return Colon(J, I, K, needToOverload);
}
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
Colon(const ArrayOf& J, const ArrayOf& I, const ArrayOf& K, bool& needToOverload)
{
    ArrayOf _I = I;
    ArrayOf _J = J;
    ArrayOf _K = K;
    needToOverload = false;
    if (_J.isSparse() || _I.isSparse() || _K.isSparse()) {
        needToOverload = true;
    } else {
        if (_J.isDoubleType() && _K.isDoubleType()) {
            try {
                _J.promoteType(_I.getDataClass());
            } catch (const Exception&) {
                needToOverload = true;
                return {};
            }
            try {
                _K.promoteType(_I.getDataClass());
            } catch (const Exception&) {
                needToOverload = true;
                return {};
            }
        } else if ((_J.getDataClass() != _K.getDataClass())) {
            if (_J.isDoubleType() || _K.isDoubleType()) {
                if (_J.isDoubleType()) {
                    if (_K.isIntegerType()) {
                        double d = _J.getContentAsDoubleScalar(true);
                        if (int64(d) != d) {
                            Error(_W("Colon input arguments must have same type."));
                        }
                        if (d < 0 && _K.isUnsignedIntegerType()) {
                            try {
                                _K.promoteType(static_cast<NelsonType>(_K.getDataClass() + 1));
                            } catch (const Exception&) {
                                needToOverload = true;
                                return {};
                            }
                        }
                    }
                    try {
                        _J.promoteType(_K.getDataClass());
                    } catch (const Exception&) {
                        needToOverload = true;
                        return {};
                    }
                } else {
                    if (_J.isIntegerType()) {
                        double d = _K.getContentAsDoubleScalar(true);
                        if (int64(d) != d) {
                            Error(_W("Colon input arguments must have same type."));
                        }
                        if (d < 0 && _K.isUnsignedIntegerType()) {
                            try {
                                _K.promoteType(static_cast<NelsonType>(_K.getDataClass() + 1));
                            } catch (const Exception&) {
                                needToOverload = true;
                                return {};
                            }
                        }
                    }
                    try {
                        _K.promoteType(_J.getDataClass());
                    } catch (const Exception&) {
                        needToOverload = true;
                        return {};
                    }
                }
            } else {
                Error(_W("Colon input arguments must have same type."));
            }
        }
        if (_I.getDataClass() != _J.getDataClass()) {
            if (_I.isDoubleType()) {
                try {
                    _I.promoteType(_J.getDataClass());
                } catch (const Exception&) {
                    needToOverload = true;
                    return {};
                }
            } else {
                Error(_W("Colon input arguments must have same type."));
            }
        }
        bool warningArrayAsScalar = false;
        switch (_J.getDataClass()) {
        case NLS_UINT8: {
            uint8 step;
            uint8 low;
            uint8 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                low = static_cast<uint8>(1);
                high = static_cast<uint8>(0);
                step = static_cast<uint8>(1);
            } else {
                step = _I.getContentAsUnsignedInteger8Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsUnsignedInteger8Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsUnsignedInteger8Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<uint8>(NLS_UINT8, low, high, step);
        } break;
        case NLS_INT8: {
            int8 step;
            int8 low;
            int8 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                low = static_cast<int8>(1);
                high = static_cast<int8>(0);
                step = static_cast<int8>(1);
            } else {
                step = _I.getContentAsInteger8Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsInteger8Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsInteger8Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<int8>(NLS_INT8, low, high, step);
        } break;
        case NLS_UINT16: {
            uint16 step;
            uint16 low;
            uint16 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<uint16>(1);
                low = static_cast<uint16>(1);
                high = static_cast<uint16>(0);
            } else {
                step = _I.getContentAsUnsignedInteger16Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsUnsignedInteger16Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsUnsignedInteger16Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<uint16>(NLS_UINT16, low, high, step);
        } break;
        case NLS_INT16: {
            int16 step;
            int16 low;
            int16 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<int16>(1);
                low = static_cast<int16>(1);
                high = static_cast<int16>(0);
            } else {
                step = _I.getContentAsInteger16Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsInteger16Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsInteger16Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<int16>(NLS_INT16, low, high, step);
        } break;
        case NLS_UINT32: {
            uint32 step;
            uint32 low;
            uint32 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<uint32>(1);
                low = static_cast<uint32>(1);
                high = static_cast<uint32>(0);
            } else {
                step = _I.getContentAsUnsignedInteger32Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsUnsignedInteger32Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsUnsignedInteger32Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<uint32>(NLS_UINT32, low, high, step);
        } break;
        case NLS_INT32: {
            auto step = static_cast<int32>(1);
            int32 low;
            int32 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<int32>(1);
                low = static_cast<int32>(1);
                high = static_cast<int32>(0);
            } else {
                step = _I.getContentAsInteger32Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsInteger32Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsInteger32Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<int32>(NLS_INT32, low, high, step);
        } break;
        case NLS_UINT64: {
            auto step = static_cast<uint64>(1);
            uint64 low;
            uint64 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<uint64>(1);
                low = static_cast<uint64>(1);
                high = static_cast<uint64>(0);
            } else {
                step = _I.getContentAsUnsignedInteger64Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsUnsignedInteger64Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsUnsignedInteger64Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<uint64>(NLS_UINT64, low, high, step);
        } break;
        case NLS_INT64: {
            int64 step;
            int64 low;
            int64 high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<int64>(1);
                low = static_cast<int64>(1);
                high = static_cast<int64>(0);
            } else {
                step = _I.getContentAsInteger32Scalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsInteger32Scalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsInteger32Scalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return integer_colon<int64>(NLS_INT64, low, high, step);
        } break;
        case NLS_SINGLE: {
            single step;
            single low;
            single high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<single>(1);
                low = static_cast<single>(1);
                high = static_cast<single>(0);
            } else {
                step = _I.getContentAsSingleScalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsSingleScalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsSingleScalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return real_colon<single>(NLS_SINGLE, low, high, step);
        } break;
        case NLS_DOUBLE: {
            double step;
            double low;
            double high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<double>(1);
                low = static_cast<double>(1);
                high = static_cast<double>(0);
            } else {
                step = _I.getContentAsDoubleScalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = _J.getContentAsDoubleScalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = _K.getContentAsDoubleScalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return real_colon<double>(NLS_DOUBLE, low, high, step);
        } break;
        case NLS_SCOMPLEX: {
            single step;
            single low;
            single high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<single>(1);
                low = static_cast<single>(1);
                high = static_cast<single>(0);
            } else {
                ArrayOf JJ(_J);
                ArrayOf KK(_K);
                ArrayOf II(_I);
                II.promoteType(NLS_SINGLE);
                JJ.promoteType(NLS_SINGLE);
                KK.promoteType(NLS_SINGLE);
                step = II.getContentAsSingleScalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = JJ.getContentAsSingleScalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = KK.getContentAsSingleScalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return real_colon<single>(NLS_SINGLE, low, high, step);
        } break;
        case NLS_DCOMPLEX: {
            double step;
            double low;
            double high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<double>(1);
                low = static_cast<double>(1);
                high = static_cast<double>(0);
            } else {
                ArrayOf II(_I);
                ArrayOf JJ(_J);
                ArrayOf KK(_K);
                II.promoteType(NLS_DOUBLE);
                JJ.promoteType(NLS_DOUBLE);
                KK.promoteType(NLS_DOUBLE);
                step = II.getContentAsDoubleScalar(true);
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = JJ.getContentAsDoubleScalar(true);
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = KK.getContentAsDoubleScalar(true);
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return real_colon<double>(NLS_DOUBLE, low, high, step);
        } break;
        case NLS_CHAR: {
            charType step;
            charType low;
            charType high;
            if (_J.isEmpty() || _K.isEmpty() || _I.isEmpty()) {
                step = static_cast<charType>(1);
                low = static_cast<charType>(1);
                high = static_cast<charType>(0);
            } else {
                std::wstring content = _I.getContentAsWideString();
                step = content[0];
                if (!_I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                content = _J.getContentAsWideString();
                low = content[0];
                if (!_J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                content = _K.getContentAsWideString();
                high = content[0];
                if (!_K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(WARNING_COLON_ARRAY_AS_SCALAR, _W("Array used as scalar."));
            }
            return char_colon(low, high, step);
        } break;
        default: {
            needToOverload = true;
        } break;
        }
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
