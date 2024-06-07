//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "AllFinite.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
AllFiniteComplex(const ArrayOf& A)
{
    if (A.isSparse()) {
        Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)(A.getSparseDataPointer());
        std::complex<T>* ptrZ = spMat->valuePtr();
        T* ptr = reinterpret_cast<T*>(ptrZ);
        Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matA(
            ptr, (Eigen::Index)1, (Eigen::Index)A.getElementCount() * 2);
        return matA.allFinite();
    }
    auto* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, 1, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)1, (Eigen::Index)A.getElementCount());
    return matA.allFinite();
}
//=============================================================================
template <class T>
bool
AllFiniteReal(const ArrayOf& A)
{
    if (A.isSparse()) {
        Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
            = (Eigen::SparseMatrix<T, 0, signedIndexType>*)(A.getSparseDataPointer());
        T* ptr = spMat->valuePtr();
        Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matA(
            ptr, (Eigen::Index)1, (Eigen::Index)A.getElementCount());
        return matA.allFinite();
    }
    Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matA(
        (T*)A.getDataPointer(), (Eigen::Index)1, (Eigen::Index)A.getElementCount());
    return matA.allFinite();
}
//=============================================================================
bool
AllFinite(const ArrayOf& arrayIn, bool& needToOverload)
{
    needToOverload = false;
    switch (arrayIn.getDataClass()) {
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        needToOverload = true;
        return false;
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL:
    case NLS_CHAR:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64: {
        return true;
    } break;
    case NLS_SINGLE: {
        return AllFiniteReal<single>(arrayIn);
    } break;
    case NLS_DOUBLE: {
        return AllFiniteReal<double>(arrayIn);
    } break;
    case NLS_DCOMPLEX: {
        return AllFiniteComplex<double>(arrayIn);
    } break;
    case NLS_SCOMPLEX: {
        return AllFiniteComplex<single>(arrayIn);
    } break;
    }
    needToOverload = true;
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
