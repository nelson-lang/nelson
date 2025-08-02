//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HorzCatHandle.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
HorzCatHandle(const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    if (!B.isHandle()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_HANDLE_EXPECTED);
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
    indexType newColumnsSize = dimsA.getColumns() + dimsB.getColumns();
    indexType newRowsSize = dimsA.getRows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    auto* ptrA = (nelson_handle*)A.getDataPointer();
    auto* ptrB = (nelson_handle*)B.getDataPointer();
    if (newSize != 0) {
        HandleGenericObject* hlObjA = HandleManager::getInstance()->getPointer(ptrA[0]);
        HandleGenericObject* hlObjB = HandleManager::getInstance()->getPointer(ptrB[0]);
        if (hlObjA->getCategory() != hlObjB->getCategory()
            || (hlObjA == nullptr || hlObjB == nullptr)) {
            Error(_W("Handles being catenated have incompatible classes."));
        }
    }
    void* pRes = ArrayOf::allocateArrayOf(NLS_HANDLE, newSize, stringVector(), false);
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
    return ArrayOf(NLS_HANDLE, dimsC, pRes);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
