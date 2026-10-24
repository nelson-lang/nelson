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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IEEEFP.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
ArrayOf
integerScalarStorage(NelsonType type, T value)
{
    Dimensions dim;
    dim.makeScalar();
    return ArrayOf(type, dim, static_cast<const void*>(&value), sizeof(T));
}
//=============================================================================
template <typename T>
ArrayOf
integerRowVectorStorage(NelsonType type, indexType len)
{
    Dimensions dim(1, len);
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(type, len, stringVector(), true));
    return ArrayOf(type, dim, data);
}
//=============================================================================
template <typename T>
ArrayOf
integerRowVectorStorage(NelsonType type, const std::vector<T>& values)
{
    ArrayOf vector = integerRowVectorStorage<T>(type, values.size());
    std::memcpy((T*)vector.dataPointer(), values.data(), values.size() * sizeof(T));
    return vector;
}
//=============================================================================
ArrayOf
ArrayOf::uint8Scalar(uint8 v)
{
    return integerScalarStorage<uint8>(NLS_UINT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::int8Scalar(int8 v)
{
    return integerScalarStorage<int8>(NLS_INT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint16Scalar(uint16 v)
{
    return integerScalarStorage<uint16>(NLS_UINT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::int16Scalar(int16 v)
{
    return integerScalarStorage<int16>(NLS_INT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint32Scalar(uint32 v)
{
    return integerScalarStorage<uint32>(NLS_UINT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::int32Scalar(int32 v)
{
    return integerScalarStorage<int32>(NLS_INT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint64Scalar(uint64 v)
{
    return integerScalarStorage<uint64>(NLS_UINT64, v);
}
//=============================================================================
ArrayOf
ArrayOf::int64Scalar(int64 v)
{
    return integerScalarStorage<int64>(NLS_INT64, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint8RowVector(indexType len)
{
    return integerRowVectorStorage<uint8>(NLS_UINT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::int8RowVector(indexType len)
{
    return integerRowVectorStorage<int8>(NLS_INT8, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint16RowVector(indexType len)
{
    return integerRowVectorStorage<uint16>(NLS_UINT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::int16RowVector(indexType len)
{
    return integerRowVectorStorage<int16>(NLS_INT16, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint32RowVector(indexType len)
{
    return integerRowVectorStorage<uint32>(NLS_UINT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::int32RowVector(indexType len)
{
    return integerRowVectorStorage<int32>(NLS_INT32, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint64RowVector(indexType len)
{
    return integerRowVectorStorage<uint64>(NLS_UINT64, len);
}
//=============================================================================
ArrayOf
ArrayOf::int64RowVector(indexType len)
{
    return integerRowVectorStorage<int64>(NLS_INT64, len);
}
//=============================================================================
ArrayOf
ArrayOf::uint8RowVector(const std::vector<uint8>& v)
{
    return integerRowVectorStorage<uint8>(NLS_UINT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::int8RowVector(const std::vector<int8>& v)
{
    return integerRowVectorStorage<int8>(NLS_INT8, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint16RowVector(const std::vector<uint16>& v)
{
    return integerRowVectorStorage<uint16>(NLS_UINT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::int16RowVector(const std::vector<int16>& v)
{
    return integerRowVectorStorage<int16>(NLS_INT16, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint32RowVector(const std::vector<uint32>& v)
{
    return integerRowVectorStorage<uint32>(NLS_UINT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::int32RowVector(const std::vector<int32>& v)
{
    return integerRowVectorStorage<int32>(NLS_INT32, v);
}
//=============================================================================
ArrayOf
ArrayOf::uint64RowVector(const std::vector<uint64>& v)
{
    return integerRowVectorStorage<uint64>(NLS_UINT64, v);
}
//=============================================================================
ArrayOf
ArrayOf::int64RowVector(const std::vector<int64>& v)
{
    return integerRowVectorStorage<int64>(NLS_INT64, v);
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
    if (hasScalarStorage()) {
        if (scalarType_ == NLS_LOGICAL) {
            return false;
        }
        if (scalarType_ == NLS_SINGLE) {
            return IsIntegerFormOrNotFinite((single*)dataPointer(), 1);
        }
        return IsIntegerFormOrNotFinite((double*)dataPointer(), 1);
    }
    if (!dp) {
        return false;
    }
    if (IS_INTEGER_TYPE(dp->dataClass)) {
        return true;
    }
    if (dp->dataClass == NLS_DOUBLE) {
        return IsIntegerFormOrNotFinite((double*)dataPointer(), elementCount());
    }
    if (dp->dataClass == NLS_SINGLE) {
        return IsIntegerFormOrNotFinite((single*)dp->data(), elementCount());
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
    return IS_UNSIGNED_INTEGER_TYPE(dataClass());
}
//=============================================================================
bool
ArrayOf::isSignedIntegerType() const
{
    return IS_SIGNED_INTEGER_TYPE(dataClass());
}
//=============================================================================
template <class T>
T
asIntegerScalarStorage(
    const ArrayOf& M, NelsonType destinationClass, bool arrayAsScalar, bool checkIsIntegerValue)
{
    T value;
    if (M.dataClass() != destinationClass && !M.isSparse()) {
        if (M.isEmpty() || M.isComplex() || M.isReferenceType() || M.isCharacterArray()
            || (!arrayAsScalar && !M.isScalar())) {
            Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
        }
        ArrayOf P(M);
        P.promoteType(destinationClass);
        T* ptr = (T*)P.dataPointer();
        value = ptr[0];
        if (checkIsIntegerValue) {
            double f = M.asDoubleScalar(arrayAsScalar, false);
            if (std::abs(f - value) >= std::numeric_limits<double>::epsilon()) {
                Error(L"Nelson:integerValueExpected", L"A real integer value scalar expected.");
            }
        }
    } else {
        T* ptr = (T*)M.dataPointer();
        value = ptr[0];
    }
    return value;
}
//=============================================================================
uint8
ArrayOf::asUint8Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<uint8>(*this, NLS_UINT8, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int8
ArrayOf::asInt8Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<int8>(*this, NLS_INT8, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int16
ArrayOf::asInt16Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<int16>(*this, NLS_INT16, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint16
ArrayOf::asUint16Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<uint16>(*this, NLS_UINT16, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int32
ArrayOf::asInt32Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<int32>(*this, NLS_INT32, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint32
ArrayOf::asUint32Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<uint32>(*this, NLS_UINT32, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
int64
ArrayOf::asInt64Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<int64>(*this, NLS_INT64, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
uint64
ArrayOf::asUint64Scalar(bool arrayAsScalar, bool checkIsIntegerValue) const
{
    return asIntegerScalarStorage<uint64>(*this, NLS_UINT64, arrayAsScalar, checkIsIntegerValue);
}
//=============================================================================
ArrayOf
ArrayOf::integerRange(indexType minval, indexType stepsize, indexType maxval, bool vert)
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
        return ArrayOf(classC, Cdim, (void*)nullptr, false);
    }
    if (minval < maxval) {
        if (stepsize < 0) {
            Cdim[0] = 1;
            Cdim[1] = 0;
            return ArrayOf(classC, Cdim, (void*)nullptr, false);
        }
    }
    if (minval > maxval) {
        Cdim[0] = 0;
        Cdim[1] = 1;
        return ArrayOf(classC, Cdim, (void*)nullptr, false);
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
        // Non-unit step, e.g. 1:2:10
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
integerMatrix2dStorage(NelsonType type, indexType m, indexType n)
{
    Dimensions dim(m, n);
    T* data
        = static_cast<T*>(ArrayOf::allocateArrayOf(type, dim.elementCount(), stringVector(), true));
    return ArrayOf(type, dim, data);
}
//=============================================================================
ArrayOf
ArrayOf::int8Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<int8>(NLS_INT8, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint8Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<uint8>(NLS_UINT8, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int16Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<int16>(NLS_INT16, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint16Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<uint16>(NLS_UINT16, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int32Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<int32>(NLS_INT32, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint32Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<uint32>(NLS_UINT32, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::int64Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<int64>(NLS_INT64, m, n);
}
//=============================================================================
ArrayOf
ArrayOf::uint64Matrix2d(indexType m, indexType n)
{
    return integerMatrix2dStorage<uint64>(NLS_UINT64, m, n);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
