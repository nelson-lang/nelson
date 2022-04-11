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
#include "VertCatGO.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatGO(ArrayOf A, ArrayOf B)
{
    if (A.getDataClass() != NLS_GO_HANDLE) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_GRAPHIC_OBJECT_EXPECTED);
    }
    if (B.getDataClass() != NLS_GO_HANDLE) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_GRAPHIC_OBJECT_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!A.isEmpty(true) && !B.isEmpty(true)) {
        if (dimsA.getRows() != dimsB.getRows()) {
            Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
        }
    }
    if (A.isEmpty(true)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(true)) {
        ArrayOf C(A);
        return C;
    }
    indexType newColumnsSize = dimsA.getColumns();
    indexType newRowsSize = dimsA.getRows() + dimsB.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    auto* ptrA = (nelson_handle*)A.getDataPointer();
    auto* ptrB = (nelson_handle*)B.getDataPointer();
    void* pRes = ArrayOf::allocateArrayOf(NLS_GO_HANDLE, newSize, stringVector(), false);
    if (newSize != 0) {
        auto* ptrC = static_cast<nelson_handle*>(pRes);
        Eigen::Map<Eigen::Matrix<nelson_handle, Eigen::Dynamic, Eigen::Dynamic>> matA(
            ptrA, dimsA.getRows(), dimsA.getColumns());
        Eigen::Map<Eigen::Matrix<nelson_handle, Eigen::Dynamic, Eigen::Dynamic>> matB(
            ptrB, dimsB.getRows(), dimsB.getColumns());
        Eigen::Map<Eigen::Matrix<nelson_handle, Eigen::Dynamic, Eigen::Dynamic>> matC(
            ptrC, dimsC.getRows(), dimsC.getColumns());
        matC << matA, matB;
    }
    return ArrayOf(NLS_GO_HANDLE, dimsC, pRes);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
