//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include "UnaryMinus.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
uminusReal(const ArrayOf& A)
{
    indexType nbElements = A.getElementCount();
    T* pRes
        = (T*)ArrayOf::allocateArrayOf(A.getDataClass(), nbElements, Nelson::stringVector(), false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(pRes, nbElements, 1);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matA((T*)A.getDataPointer(), nbElements, 1);
    matRes = -matA.array();
    return ArrayOf(A.getDataClass(), A.getDimensions(), pRes);
}
//=============================================================================
template <class T>
ArrayOf
uminusComplex(const ArrayOf& A)
{
    indexType nbElements = A.getElementCount();
    void* pRes
        = ArrayOf::allocateArrayOf(A.getDataClass(), nbElements, Nelson::stringVector(), false);
    std::complex<T>* pResz = reinterpret_cast<std::complex<T>*>(pRes);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matRes(pResz, nbElements, 1);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matA(Az, nbElements, 1);
    matRes = -matA.array();
    return ArrayOf(A.getDataClass(), A.getDimensions(), pRes);
}
//=============================================================================
template <class T>
ArrayOf
uminusInteger(const ArrayOf& A)
{
    indexType nbElements = A.getElementCount();
    T* pRes
        = (T*)ArrayOf::allocateArrayOf(A.getDataClass(), nbElements, Nelson::stringVector(), false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(pRes, nbElements, 1);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matA((T*)A.getDataPointer(), nbElements, 1);
    matRes = -matA.array();
    return ArrayOf(A.getDataClass(), A.getDimensions(), pRes);
}
//=============================================================================
ArrayOf
UnaryMinus(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    ArrayOf res;
    switch (A.getDataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = A;
        res.ensureSingleOwner();
        res.promoteType(NLS_DOUBLE);
        res = uminusReal<double>(res);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        void* Cp
            = ArrayOf::allocateArrayOf(A.getDataClass(), A.getElementCount(), stringVector(), true);
        res = ArrayOf(A.getDataClass(), A.getDimensions(), Cp);
    } break;
    case NLS_INT8: {
        res = uminusInteger<int8>(A);
    } break;
    case NLS_INT16: {
        res = uminusInteger<int16>(A);
    } break;
    case NLS_INT32: {
        res = uminusInteger<int32>(A);
    } break;
    case NLS_INT64: {
        res = uminusInteger<int64>(A);
    } break;
    case NLS_SINGLE: {
        res = uminusReal<single>(A);
    } break;
    case NLS_DOUBLE: {
        res = uminusReal<double>(A);
    } break;
    case NLS_SCOMPLEX: {
        res = uminusComplex<single>(A);
    } break;
    case NLS_DCOMPLEX: {
        res = uminusComplex<double>(A);
    } break;
    default: {
        needToOverload = true;
        return ArrayOf();
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
