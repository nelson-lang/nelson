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
#include <Eigen/Dense>
#include <cmath>
#include "Colon.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
integer_colon(Class destinationClass, T low, T high, T step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(destinationClass);
        return res;
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
    double dn = (double)((((high - low) / step) + 1));
    indexType n = (indexType)std::trunc(dn);
    T* pV = (T*)ArrayOf::allocateArrayOf(destinationClass, n, stringVector(), false);
	for (indexType k = 0; k < n; k++) {
        pV[k] = (k == 0) ? low : pV[k - 1] + step;
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
    if (low < high) {
        if (step < 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(NLS_CHAR);
            return res;
        }
    }
    if (low > high) {
        if (step > 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(NLS_CHAR);
            return res;
        }
    }
    double dn = (double)((((high - low) / step) + 1));
    indexType n = (indexType)std::trunc(dn);
    charType* pV = (charType*)ArrayOf::allocateArrayOf(NLS_CHAR, n, stringVector(), false);
    for (indexType k = 0; k < n; k++) {
        pV[k] = (k == 0) ? low : pV[k - 1] + step;
    }
    return ArrayOf(NLS_CHAR, Dimensions(1, n), pV);
}
//=============================================================================
static ArrayOf
single_colon(single low, single high, single step)
{
    if (step == 0) {
        ArrayOf res = ArrayOf::emptyConstructor(1, 0);
        res.promoteType(NLS_SINGLE);
        return res;
    }
    if (std::isnan(low) || std::isnan(high) || std::isnan(step)) {
        Dimensions Cdim(1, 1);
        return ArrayOf::singleConstructor(nanf(""));
    }
    if (!std::isfinite(low) || !std::isfinite(high) || !std::isfinite(step)) {
        Error(_W("Invalid range."));
    }
    if (low < high) {
        if (step < 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(NLS_SINGLE);
            return res;
        }
    }
    if (low > high) {
        if (step > 0) {
            ArrayOf res = ArrayOf::emptyConstructor(1, 0);
            res.promoteType(NLS_SINGLE);
            return res;
        }
    }
    single dn = (single)((((high - low) / step) + 1));
    indexType n = (indexType)std::trunc(dn);
    single* pV = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, n, stringVector(), false);
    ArrayOf V = ArrayOf(NLS_SINGLE, Dimensions(1, n), pV);
    if (dn == (single(n))) {
        Eigen::Map<Eigen::VectorXf> Range(pV, n);
        Range = Eigen::VectorXf::LinSpaced(n, low, high);
    } else {
        // We must use another algo. in this case
        // 1:2:10
        int i = 0;
        single v = low;
        while ((low < high && v <= high) || (low > high && v >= high)) {
            pV[i] = v;
            v = v + step;
            i++;
        }
    }
    return V;
}
//=============================================================================
static ArrayOf
double_colon(double low, double high, double step)
{
    if (step == 0) {
        return ArrayOf::emptyConstructor(1, 0);
    }
    if (std::isnan(low) || std::isnan(high) || std::isnan(step)) {
        Dimensions Cdim(1, 1);
        return ArrayOf::doubleConstructor(nan(""));
    }
    if (!std::isfinite(low) || !std::isfinite(high) || !std::isfinite(step)) {
        Error(_W("Invalid range."));
    }
    if (low < high) {
        if (step < 0) {
            return ArrayOf::emptyConstructor(1, 0);
        }
    }
    if (low > high) {
        if (step > 0) {
            return ArrayOf::emptyConstructor(1, 0);
        }
    }
    double dn = (double)((((high - low) / step) + 1));
    indexType n = (indexType)std::trunc(dn);
    double* pV = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, n, stringVector(), false);
    ArrayOf V = ArrayOf(NLS_DOUBLE, Dimensions(1, n), pV);
    if (dn == (double(n))) {
        Eigen::Map<Eigen::VectorXd> Range(pV, n);
        Range = Eigen::VectorXd::LinSpaced(n, low, high);
    } else {
        // We must use another algo. in this case
        // 1:2:10
        indexType i = 0;
        double v = low;
        while ((low < high && v <= high) || (low > high && v >= high)) {
            pV[i] = v;
            v = v + step;
            i++;
        }
    }
    return V;
}
//=============================================================================
ArrayOf
Colon(ArrayOf& J, ArrayOf& K, bool& needToOverload)
{
    ArrayOf I = ArrayOf::doubleConstructor(1);
    return Colon(J, I, K, needToOverload);
}
//=============================================================================
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf
Colon(ArrayOf& J, ArrayOf& I, ArrayOf& K, bool& needToOverload)
{
    needToOverload = false;
    if (J.isSparse() || I.isSparse() || K.isSparse()) {
        needToOverload = true;
    } else {
        if (J.isDoubleType() && K.isDoubleType()) {
            try {
                J.promoteType(I.getDataClass());
            } catch (const Exception&) {
                needToOverload = true;
                return ArrayOf();
            }
            try {
                K.promoteType(I.getDataClass());
            } catch (const Exception&) {
                needToOverload = true;
                return ArrayOf();
            }
        } else if ((J.getDataClass() != K.getDataClass())) {
            if (J.isDoubleType() || K.isDoubleType()) {
                if (J.isDoubleType()) {
                    if (K.isIntegerType()) {
                        double d = J.getContentAsDoubleScalar(true);
                        if (int64(d) != d) {
                            Error(_W("Colon input arguments must have same type."));
                        }
                        if (d < 0 && K.isUnsignedIntegerType()) {
                            try {
                                K.promoteType((Class)(K.getDataClass() + 1));
                            } catch (const Exception&) {
                                needToOverload = true;
                                return ArrayOf();
                            }
                        }
                    }
                    try {
                        J.promoteType(K.getDataClass());
                    } catch (const Exception&) {
                        needToOverload = true;
                        return ArrayOf();
                    }
                } else {
                    if (J.isIntegerType()) {
                        double d = K.getContentAsDoubleScalar(true);
                        if (int64(d) != d) {
                            Error(_W("Colon input arguments must have same type."));
                        }
                        if (d < 0 && K.isUnsignedIntegerType()) {
                            try {
                                K.promoteType((Class)(K.getDataClass() + 1));
                            } catch (const Exception&) {
                                needToOverload = true;
                                return ArrayOf();
                            }
                        }
                    }
                    try {
                        K.promoteType(J.getDataClass());
                    } catch (const Exception&) {
                        needToOverload = true;
                        return ArrayOf();
                    }
                }
            } else {
                Error(_W("Colon input arguments must have same type."));
            }
        }
        if (I.getDataClass() != J.getDataClass()) {
            if (I.isDoubleType()) {
                try {
                    I.promoteType(J.getDataClass());
                } catch (const Exception&) {
                    needToOverload = true;
                    return ArrayOf();
                }
            } else {
                Error(_W("Colon input arguments must have same type."));
            }
        }
        bool warningArrayAsScalar = false;
        switch (J.getDataClass()) {
        case NLS_UINT8: {
            uint8 step;
            uint8 low;
            uint8 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                low = (uint8)1;
                high = (uint8)0;
                uint8 step = (uint8)1;
            } else {
                step = I.getContentAsUnsignedInteger8Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsUnsignedInteger8Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsUnsignedInteger8Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<uint8>(NLS_UINT8, low, high, step);
        } break;
        case NLS_INT8: {
            int8 step;
            int8 low;
            int8 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                low = (int8)1;
                high = (int8)0;
                step = (int8)1;
            } else {
                step = I.getContentAsInteger8Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsInteger8Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsInteger8Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<int8>(NLS_INT8, low, high, step);
        } break;
        case NLS_UINT16: {
            uint16 step;
            uint16 low;
            uint16 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (uint16)1;
                low = (uint16)1;
                high = (uint16)0;
            } else {
                step = I.getContentAsUnsignedInteger16Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsUnsignedInteger16Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsUnsignedInteger16Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<uint16>(NLS_UINT16, low, high, step);
        } break;
        case NLS_INT16: {
            int16 step;
            int16 low;
            int16 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (int16)1;
                low = (int16)1;
                high = (int16)0;
            } else {
                step = I.getContentAsInteger16Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsInteger16Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsInteger16Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<int16>(NLS_INT16, low, high, step);
        } break;
        case NLS_UINT32: {
            uint32 step;
            uint32 low;
            uint32 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (uint32)1;
                low = (uint32)1;
                high = (uint32)0;
            } else {
                step = I.getContentAsUnsignedInteger32Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsUnsignedInteger32Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsUnsignedInteger32Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<uint32>(NLS_UINT32, low, high, step);
        } break;
        case NLS_INT32: {
            int32 step = (int32)1;
            int32 low;
            int32 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (int32)1;
                low = (int32)1;
                high = (int32)0;
            } else {
                step = I.getContentAsInteger32Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsInteger32Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsInteger32Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<int32>(NLS_INT32, low, high, step);
        } break;
        case NLS_UINT64: {
            uint64 step = (uint64)1;
            uint64 low;
            uint64 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (uint64)1;
                low = (uint64)1;
                high = (uint64)0;
            } else {
                step = I.getContentAsUnsignedInt64Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsUnsignedInt64Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsUnsignedInt64Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<uint64>(NLS_UINT64, low, high, step);
        } break;
        case NLS_INT64: {
            int64 step;
            int64 low;
            int64 high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (int64)1;
                low = (int64)1;
                high = (int64)0;
            } else {
                step = I.getContentAsInteger32Scalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsInteger32Scalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsInteger32Scalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return integer_colon<int64>(NLS_INT64, low, high, step);
        } break;
        case NLS_SINGLE: {
            single step;
            single low;
            single high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (single)1;
                low = (single)1;
                high = (single)0;
            } else {
                step = I.getContentAsSingleScalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsSingleScalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsSingleScalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return single_colon(low, high, step);
        } break;
        case NLS_DOUBLE: {
            double step;
            double low;
            double high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (double)1;
                low = (double)1;
                high = (double)0;
            } else {
                step = I.getContentAsDoubleScalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = J.getContentAsDoubleScalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = K.getContentAsDoubleScalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return double_colon(low, high, step);
        } break;
        case NLS_SCOMPLEX: {
            single step;
            single low;
            single high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (single)1;
                low = (single)1;
                high = (single)0;
            } else {
                ArrayOf JJ(J);
                ArrayOf KK(K);
                ArrayOf II(I);
                II.promoteType(NLS_SINGLE);
                JJ.promoteType(NLS_SINGLE);
                KK.promoteType(NLS_SINGLE);
                step = II.getContentAsSingleScalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = JJ.getContentAsSingleScalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = KK.getContentAsSingleScalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return single_colon(low, high, step);
        } break;
        case NLS_DCOMPLEX: {
            double step;
            double low;
            double high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (double)1;
                low = (double)1;
                high = (double)0;
            } else {
                ArrayOf II(I);
                ArrayOf JJ(J);
                ArrayOf KK(K);
                II.promoteType(NLS_DOUBLE);
                JJ.promoteType(NLS_DOUBLE);
                KK.promoteType(NLS_DOUBLE);
                step = II.getContentAsDoubleScalar(true);
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                low = JJ.getContentAsDoubleScalar(true);
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                high = KK.getContentAsDoubleScalar(true);
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return double_colon(low, high, step);
        } break;
        case NLS_CHAR: {
            charType step;
            charType low;
            charType high;
            if (J.isEmpty() || K.isEmpty() || I.isEmpty()) {
                step = (charType)1;
                low = (charType)1;
                high = (charType)0;
            } else {
                std::wstring content = I.getContentAsWideString();
                step = content[0];
                if (!I.isScalar()) {
                    warningArrayAsScalar = true;
                }
                content = J.getContentAsWideString();
                low = content[0];
                if (!J.isScalar()) {
                    warningArrayAsScalar = true;
                }
                content = K.getContentAsWideString();
                high = content[0];
                if (!K.isScalar()) {
                    warningArrayAsScalar = true;
                }
            }
            if (warningArrayAsScalar) {
                Warning(L"Nelson:colon:array-as-scalar", _W("Array used as scalar."));
            }
            return char_colon(low, high, step);
        } break;
        default: {
            needToOverload = true;
        } break;
        }
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
