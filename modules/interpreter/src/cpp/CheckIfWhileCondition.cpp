//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CheckIfWhileCondition.hpp"
#include <Eigen/Sparse>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
checkIfWhileCondition(const ArrayOf& A)
{
    bool res = true;
    T first = ((T*)A.getDataPointer())[0];
    if (A.isScalar()) {
        return first != T(0);
    } else {
        if (!first) {
            return false;
        }
        indexType elementCount = A.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            T valueToCompare = ((T*)A.getDataPointer())[i];
            if (!valueToCompare) {
                res = false;
                break;
            }
        }
    }
    return res;
}
//=============================================================================
bool
checkIfWhileCondition(const ArrayOf& A)
{
    bool res = true;
    if (A.isEmpty()) {
        res = false;
    } else {
        if (A.isSparse()) {
            res = false;
            switch (A.getDataClass()) {
            case NLS_LOGICAL: {
                Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
                    = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.getSparseDataPointer();
                Eigen::Index nnz = spMatA->nonZeros();
                if (A.isScalar()) {
                    return (nnz == 1);
                }
                return (nnz == A.getElementCount());

            } break;
            case NLS_DOUBLE: {
                Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
                Eigen::Index nnz = spMatA->nonZeros();
                if (A.isScalar()) {
                    return (nnz == 1);
                }
                return (nnz == A.getElementCount());

            } break;
            case NLS_DCOMPLEX: {
                Error(_W("Complex cannot be converted to logical."));
            } break;
            default: {
                Error(_W("Unable to convert variable type to test for if/while statement"));
            } break;
            }
        } else {
            switch (A.getDataClass()) {
            case NLS_LOGICAL: {
                return checkIfWhileCondition<logical>(A);
            } break;
            case NLS_UINT8: {
                return checkIfWhileCondition<uint8>(A);
            } break;
            case NLS_INT8: {
                return checkIfWhileCondition<int8>(A);
            } break;
            case NLS_UINT16: {
                return checkIfWhileCondition<uint16>(A);
            } break;
            case NLS_INT16: {
                return checkIfWhileCondition<int16>(A);
            } break;
            case NLS_UINT32: {
                return checkIfWhileCondition<uint32>(A);
            } break;
            case NLS_INT32: {
                return checkIfWhileCondition<int32>(A);
            } break;
            case NLS_UINT64: {
                return checkIfWhileCondition<uint64>(A);
            } break;
            case NLS_INT64: {
                return checkIfWhileCondition<int64>(A);
            } break;
            case NLS_SINGLE: {
                return checkIfWhileCondition<single>(A);
            } break;
            case NLS_DOUBLE: {
                return checkIfWhileCondition<double>(A);
            } break;
            case NLS_CHAR: {
                return checkIfWhileCondition<charType>(A);
            } break;
            case NLS_SCOMPLEX:
            case NLS_DCOMPLEX: {
                Error(_W("Complex cannot be converted to logical."));
            } break;
            default: {
                Error(_W("Unable to convert variable type to test for if/while statement"));
            } break;
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
