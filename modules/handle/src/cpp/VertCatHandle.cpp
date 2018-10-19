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
#include "VertCatHandle.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatHandle(ArrayOf A, ArrayOf B)
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
        if (dimsA.getColumns() != dimsB.getColumns()) {
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
    void* pRes = nullptr;
    nelson_handle* ptrA = (nelson_handle*)A.getDataPointer();
    nelson_handle* ptrB = (nelson_handle*)B.getDataPointer();
    if (newSize != 0) {
        HandleGenericObject* hlObjA = HandleManager::getInstance()->getPointer(ptrA[0]);
        HandleGenericObject* hlObjB = HandleManager::getInstance()->getPointer(ptrB[0]);
        if (hlObjA->getCategory() != hlObjB->getCategory()
            || (hlObjA == nullptr || hlObjB == nullptr)) {
            Error(_W("Handles being catenated have incompatible classes."));
        }
    }
    pRes = ArrayOf::allocateArrayOf(NLS_HANDLE, newSize);
    if (newSize != 0) {
        nelson_handle* ptrC = (nelson_handle*)pRes;
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
}
//=============================================================================
