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
#include "CheckIfWhileCondition.hpp"
#include <Eigen/Sparse>
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
        for (indexType i = 0; i < A.getDimensions().getElementCount(); i++) {
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
                } else {
                    return (nnz == A.getDimensions().getElementCount());
                }
            } break;
            case NLS_DOUBLE: {
                Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
                Eigen::Index nnz = spMatA->nonZeros();
                if (A.isScalar()) {
                    return (nnz == 1);
                } else {
                    return (nnz == A.getDimensions().getElementCount());
                }
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
