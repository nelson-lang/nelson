//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
template <typename T>
ArrayOf
scalarConstructor(NelsonType type, T value)
{
    Dimensions dim;
    dim.makeScalar();
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(type, 1, stringVector(), false));
    *data = value;
    return ArrayOf(type, dim, data);
}
//=============================================================================
template <typename T>
ArrayOf
rowVectorConstructor(NelsonType type, indexType len)
{
    Dimensions dim(1, len);
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(type, len, stringVector(), true));
    return ArrayOf(type, dim, data);
}
//=============================================================================
template <typename T>
ArrayOf
rowVectorConstructor(NelsonType type, const std::vector<T>& values)
{
    ArrayOf vector = rowVectorConstructor<T>(type, values.size());
    std::memcpy((T*)vector.getDataPointer(), values.data(), values.size() * sizeof(T));
    return vector;
}
//=============================================================================
// Scalar Constructors
//=============================================================================
ArrayOf
ArrayOf::uint8Constructor(uint8 v)
{
    return scalarConstructor<uint8>(NLS_UINT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::int8Constructor(int8 v)
{
    return scalarConstructor<int8>(NLS_INT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint16Constructor(uint16 v)
{
    return scalarConstructor<uint16>(NLS_UINT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::int16Constructor(int16 v)
{
    return scalarConstructor<int16>(NLS_INT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint32Constructor(uint32 v)
{
    return scalarConstructor<uint32>(NLS_UINT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::int32Constructor(int32 v)
{
    return scalarConstructor<int32>(NLS_INT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint64Constructor(uint64 v)
{
    return scalarConstructor<uint64>(NLS_UINT64, v);
}
//=============================================================================
ArrayOf
ArrayOf::int64Constructor(int64 v)
{
    return scalarConstructor<int64>(NLS_INT64, v);
}
//=============================================================================
// Vector Constructors (length)
ArrayOf
ArrayOf::uint8RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<uint8>(NLS_UINT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::int8RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<int8>(NLS_INT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint16RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<uint16>(NLS_UINT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::int16RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<int16>(NLS_INT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint32RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<uint32>(NLS_UINT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::int32RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<int32>(NLS_INT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint64RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<uint64>(NLS_UINT64, len);
}
//=============================================================================
ArrayOf
ArrayOf::int64RowVectorConstructor(indexType len)
{
    return rowVectorConstructor<int64>(NLS_INT64, len);
}
//=============================================================================
// Vector Constructors (from std::vector)
ArrayOf
ArrayOf::uint8RowVectorConstructor(const std::vector<uint8>& v)
{
    return rowVectorConstructor<uint8>(NLS_UINT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::int8RowVectorConstructor(const std::vector<int8>& v)
{
    return rowVectorConstructor<int8>(NLS_INT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint16RowVectorConstructor(const std::vector<uint16>& v)
{
    return rowVectorConstructor<uint16>(NLS_UINT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::int16RowVectorConstructor(const std::vector<int16>& v)
{
    return rowVectorConstructor<int16>(NLS_INT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint32RowVectorConstructor(const std::vector<uint32>& v)
{
    return rowVectorConstructor<uint32>(NLS_UINT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::int32RowVectorConstructor(const std::vector<int32>& v)
{
    return rowVectorConstructor<int32>(NLS_INT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint64RowVectorConstructor(const std::vector<uint64>& v)
{
    return rowVectorConstructor<uint64>(NLS_UINT64, v);
}
//=============================================================================
ArrayOf
ArrayOf::int64RowVectorConstructor(const std::vector<int64>& v)
{
    return rowVectorConstructor<int64>(NLS_INT64, v);
}
//=============================================================================
bool
ArrayOf::isIntegerType() const
{
    if (dp) {
        return IS_INTEGER_TYPE(dp->dataClass);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isIntegerValue() const
{
    if (!dp) {
        return false;
    }
    if (IS_INTEGER_TYPE(dp->dataClass)) {
        return true;
    }
    if (dp->dataClass == NLS_DOUBLE) {
        return IsIntegerFormOrNotFinite((double*)getDataPointer(), getElementCount());
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
    return IS_UNSIGNED_INTEGER_TYPE(getDataClass());
}
//=============================================================================
bool
ArrayOf::isSignedIntegerType() const
{
    return IS_SIGNED_INTEGER_TYPE(getDataClass());
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
ArrayOf
matrix2dConstructor(NelsonType type, indexType m, indexType n)
{
    Dimensions dim(m, n);
    T* data = static_cast<T*>(
        ArrayOf::allocateArrayOf(type, dim.getElementCount(), stringVector(), true));
    return ArrayOf(type, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int8Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<int8>(NLS_INT8, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint8Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<uint8>(NLS_UINT8, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int16Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<int16>(NLS_INT16, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint16Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<uint16>(NLS_UINT16, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int32Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<int32>(NLS_INT32, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint32Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<uint32>(NLS_UINT32, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int64Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<int64>(NLS_INT64, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint64Matrix2dConstructor(indexType m, indexType n)
{
    return matrix2dConstructor<uint64>(NLS_UINT64, m, n);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
