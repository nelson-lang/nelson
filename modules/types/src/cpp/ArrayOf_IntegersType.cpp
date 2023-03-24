//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ArrayOf::uint8Constructor(uint8 aval)
{
    Dimensions dim;
    dim.makeScalar();
    uint8* data = static_cast<uint8*>(allocateArrayOf(NLS_UINT8, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_UINT8, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int8Constructor(int8 aval)
{
    Dimensions dim;
    dim.makeScalar();
    int8* data = static_cast<int8*>(allocateArrayOf(NLS_INT8, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_INT8, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::uint16Constructor(uint16 aval)
{
    Dimensions dim;
    dim.makeScalar();
    uint16* data = static_cast<uint16*>(allocateArrayOf(NLS_UINT16, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_UINT16, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int16Constructor(int16 aval)
{
    Dimensions dim;
    dim.makeScalar();
    int16* data = static_cast<int16*>(allocateArrayOf(NLS_INT16, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_INT16, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::uint32Constructor(uint32 aval)
{
    Dimensions dim;
    dim.makeScalar();
    uint32* data = static_cast<uint32*>(allocateArrayOf(NLS_UINT32, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_UINT32, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int32Constructor(int32 aval)
{
    Dimensions dim;
    dim.makeScalar();
    int32* data = static_cast<int32*>(allocateArrayOf(NLS_INT32, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_INT32, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::uint64Constructor(uint64 aval)
{
    Dimensions dim;
    dim.makeScalar();
    uint64* data = static_cast<uint64*>(allocateArrayOf(NLS_UINT64, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_UINT64, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int64Constructor(int64 aval)
{
    Dimensions dim;
    dim.makeScalar();
    int64* data = static_cast<int64*>(allocateArrayOf(NLS_INT64, 1, stringVector(), false));
    *data = aval;
    return ArrayOf(NLS_INT64, dim, data);
}
//=============================================================================
template <class T>
ArrayOf
integerVectorConstructor(NelsonType nlsType, indexType len)
{
    Dimensions dim;
    dim.makeScalar();
    dim[1] = len;
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(nlsType, len, stringVector(), true));
    return ArrayOf(nlsType, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::uint8VectorConstructor(indexType len)
{
    return integerVectorConstructor<uint8>(NLS_UINT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::int8VectorConstructor(indexType len)
{
    return integerVectorConstructor<int8>(NLS_INT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint16VectorConstructor(indexType len)
{
    return integerVectorConstructor<uint16>(NLS_UINT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::int16VectorConstructor(indexType len)
{
    return integerVectorConstructor<int16>(NLS_INT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint32VectorConstructor(indexType len)
{
    return integerVectorConstructor<uint32>(NLS_UINT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::int32VectorConstructor(indexType len)
{
    return integerVectorConstructor<int32>(NLS_INT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint64VectorConstructor(indexType len)
{
    return integerVectorConstructor<uint64>(NLS_UINT64, len);
}
//=============================================================================
ArrayOf
ArrayOf::int64VectorConstructor(indexType len)
{
    return integerVectorConstructor<int64>(NLS_INT64, len);
}
//=============================================================================
ArrayOf
ArrayOf::int32Matrix2dConstructor(indexType m, indexType n)
{
    Dimensions dim(m, n);
    int32* data = static_cast<int32*>(
        allocateArrayOf(NLS_INT32, dim.getElementCount(), stringVector(), true));
    return ArrayOf(NLS_INT32, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::integerRangeConstructor(indexType minval, indexType stepsize, indexType maxval, bool vert)
{
    Dimensions Cdim;
    NelsonType classC;
#ifdef NLS_INDEX_TYPE_64
    classC = NLS_INT64;
#else
    classC = NLS_INT32;
#endif
    if (stepsize == 0) {
        Cdim[0] = 1;
        Cdim[1] = 0;
        return ArrayOf(classC, Cdim, nullptr, false);
    }
    if (minval < maxval) {
        if (stepsize < 0) {
            Cdim[0] = 1;
            Cdim[1] = 0;
            return ArrayOf(classC, Cdim, nullptr, false);
        }
    }
    if (minval > maxval) {
        Cdim[0] = 0;
        Cdim[1] = 1;
        return ArrayOf(classC, Cdim, nullptr, false);
    }
    auto dn = static_cast<double>((((maxval - minval) / stepsize) + 1));
#ifdef NLS_INDEX_TYPE_64
    auto n = static_cast<int64>(floor(static_cast<double>(dn)));
#else
    auto n = static_cast<int32>(floor((dn)));
#endif
    if (vert) {
        Cdim[0] = n;
        Cdim[1] = 1;
    } else {
        Cdim[0] = 1;
        Cdim[1] = n;
    }
#ifdef NLS_INDEX_TYPE_64
    int64* rp = static_cast<int64*>(allocateArrayOf(NLS_INT64, n, stringVector(), false));
#else
    int32* rp = static_cast<int32*>(allocateArrayOf(NLS_INT32, n, stringVector(), false));
#endif
    if (dn == (double(n))) {
#ifdef NLS_INDEX_TYPE_64
        Eigen::Map<Eigen::Matrix<int64, Eigen::Dynamic, 1>> Range(rp, n);
        Range = Eigen::Matrix<int64, Eigen::Dynamic, 1>::LinSpaced(n, minval, maxval);
#else
        Eigen::Map<Eigen::VectorXi> Range(rp, n);
        Range = Eigen::VectorXi::LinSpaced(n, minval, maxval);
#endif
    } else {
        // We must use another algo. in this case
        // 1:2:10
        indexType i = 0;
        indexType v = minval;
        while ((minval < maxval && v <= maxval) || (minval > maxval && v >= maxval)) {
            rp[i] = v;
            v = v + stepsize;
            i++;
        }
    }
    return ArrayOf(classC, Cdim, rp);
}
//=============================================================================
template <class T>
T
getContentAsScalar(
    const ArrayOf& M, NelsonType destinationClass, bool arrayAsScalar, bool checkIsIntegerValue)
{
    T value;
    if (M.getDataClass() != destinationClass && !M.isSparse()) {
        if (M.isEmpty() || M.isComplex() || M.isReferenceType() || M.isCharacterArray()
            || (!arrayAsScalar && !M.isScalar())) {
            Error(ERROR_SCALAR_EXPECTED);
        }
        ArrayOf P(M);
        P.promoteType(destinationClass);
        T* ptr = (T*)P.getDataPointer();
        value = ptr[0];
        if (checkIsIntegerValue) {
            double f = M.getContentAsDoubleScalar(arrayAsScalar, false);
            if (std::abs(f - value) >= std::numeric_limits<double>::epsilon()) {
                Error(_W("A real integer value scalar expected."));
            }
        }
    } else {
        T* ptr = (T*)M.getDataPointer();
        value = ptr[0];
    }
    return value;
}
//=============================================================================
uint8
ArrayOf::getContentAsUnsignedInteger8Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<uint8>(*this, NLS_UINT8, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int8
ArrayOf::getContentAsInteger8Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<int8>(*this, NLS_INT8, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int16
ArrayOf::getContentAsInteger16Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<int16>(*this, NLS_INT16, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint16
ArrayOf::getContentAsUnsignedInteger16Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<uint16>(*this, NLS_UINT16, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int32
ArrayOf::getContentAsInteger32Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<int32>(*this, NLS_INT32, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint32
ArrayOf::getContentAsUnsignedInteger32Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<uint32>(*this, NLS_UINT32, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int64
ArrayOf::getContentAsInteger64Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<int64>(*this, NLS_INT64, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint64
ArrayOf::getContentAsUnsignedInteger64Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return getContentAsScalar<uint64>(*this, NLS_UINT64, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
bool
ArrayOf::isIntegerType() const
{
    if (dp) {
        return ((dp->dataClass == NLS_UINT8) || (dp->dataClass == NLS_UINT16)
            || (dp->dataClass == NLS_UINT32) || (dp->dataClass == NLS_UINT64)
            || (dp->dataClass == NLS_INT8) || (dp->dataClass == NLS_INT16)
            || (dp->dataClass == NLS_INT32) || (dp->dataClass == NLS_INT64));
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isIntegerValue() const
{
    if (isIntegerType()) {
        return true;
    }
    if (dp->dataClass == NLS_DOUBLE) {
        return IsIntegerFormOrNotFinite((double*)dp->getData(), getElementCount());
    }
    if (dp->dataClass == NLS_SINGLE) {
        return IsIntegerFormOrNotFinite((single*)dp->getData(), getElementCount());
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isNdArrayIntegerType() const
{
    return (isIntegerType() && !isSparse() && !is2D());
}
//=============================================================================
bool
ArrayOf::isUnsignedIntegerType() const
{
    return getDataClass() == NLS_UINT8 || getDataClass() == NLS_UINT16
        || getDataClass() == NLS_UINT32 || getDataClass() == NLS_UINT64;
}
//=============================================================================
bool
ArrayOf::isSignedIntegerType() const
{
    return getDataClass() == NLS_INT8 || getDataClass() == NLS_INT16 || getDataClass() == NLS_INT32
        || getDataClass() == NLS_INT64;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
