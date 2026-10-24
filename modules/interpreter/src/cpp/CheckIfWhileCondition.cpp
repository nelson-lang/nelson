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
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
checkIfWhileCondition(const ArrayOf& A)
{
    bool res = true;
    T first = ((T*)A.dataPointer())[0];
    if (A.isScalar()) {
        return first != T(0);
    } else {
        if (!first) {
            return false;
        }
        indexType elementCount = A.elementCount();
        for (indexType i = 0; i < elementCount; i++) {
            T valueToCompare = ((T*)A.dataPointer())[i];
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
            switch (A.dataClass()) {
            case NLS_LOGICAL: {
                Eigen::SparseMatrix<logical, 0, signedIndexType>* spMatA
                    = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)A.sparseDataPointer();
                Eigen::Index nnz = spMatA->nonZeros();
                if (A.isScalar()) {
                    return (nnz == 1);
                }
                return (nnz == A.elementCount());

            } break;
            case NLS_DOUBLE: {
                Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.sparseDataPointer();
                Eigen::Index nnz = spMatA->nonZeros();
                if (A.isScalar()) {
                    return (nnz == 1);
                }
                return (nnz == A.elementCount());

            } break;
            case NLS_DCOMPLEX: {
                Error(L"Nelson:interpreter:complexToLogical",
                    L"Complex cannot be converted to logical.");
            } break;
            default: {
                Error(L"Nelson:interpreter:invalidCondition",
                    L"Unable to convert variable type to test for if/while statement");
            } break;
            }
        } else {
            switch (A.dataClass()) {
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
                Error(L"Nelson:interpreter:complexToLogical",
                    L"Complex cannot be converted to logical.");
            } break;
            default: {
                Error(L"Nelson:interpreter:invalidCondition",
                    L"Unable to convert variable type to test for if/while statement");
            } break;
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
