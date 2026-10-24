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
#include "ConcatenateNdArray.hpp"
#include "ConcatenateDimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr indexType HORZCAT_AXIS = 1;
//=============================================================================
static ArrayOf
concatenateNd(const ArrayOf& A, const ArrayOf& B);
//=============================================================================
static void
checkHandleClassCompatibility(const ArrayOf& A, const ArrayOf& B)
{
    if (A.handleCategory() != B.handleCategory()) {
        Error(L"Nelson:handle:incompatibleClasses",
            L"Handles being catenated have incompatible classes.");
    }
}
//=============================================================================
ArrayOf
HorzCatHandle(const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    if (!B.isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((2)), L"handle");
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (A.isEmpty(false) || B.isEmpty(false)) {
        checkHandleClassCompatibility(A, B);
        if (A.isEmpty(false) && B.isEmpty(false)) {
            Dimensions dimsC = concatenatedEmptyDimensions(dimsA, dimsB, HORZCAT_AXIS);
            return ArrayOf::emptyHandle(A.handleCategory(), dimsC);
        }
        if (A.isEmpty(false)) {
            checkEmptyOperandDimensions(dimsA, dimsB, HORZCAT_AXIS);
            return ArrayOf(B);
        }
        checkEmptyOperandDimensions(dimsB, dimsA, HORZCAT_AXIS);
        return ArrayOf(A);
    }
    checkConcatenationDimensions(dimsA, dimsB, HORZCAT_AXIS);
    indexType newColumnsSize = dimsA.columns() + dimsB.columns();
    indexType newRowsSize = dimsA.rows();
    indexType newSize = newColumnsSize * newRowsSize;
    Dimensions dimsC = Dimensions(newRowsSize, newColumnsSize);
    auto* ptrA = (HandleRef*)A.dataPointer();
    auto* ptrB = (HandleRef*)B.dataPointer();
    if (newSize != 0) {
        if (!ptrA[0] || !ptrB[0] || ptrA[0]->category() != ptrB[0]->category()) {
            Error(L"Nelson:handle:incompatibleClasses",
                L"Handles being catenated have incompatible classes.");
        }
    }
    if (!A.is2D() || !B.is2D()) {
        return concatenateNd(A, B);
    }
    void* pRes = ArrayOf::allocateArrayOf(NLS_HANDLE, newSize, stringVector(), false);
    if (newSize != 0) {
        auto* ptrC = static_cast<HandleRef*>(pRes);
        indexType rows = dimsC.rows();
        for (indexType col = 0; col < dimsA.columns(); ++col) {
            for (indexType row = 0; row < rows; ++row) {
                ptrC[col * rows + row] = ptrA[col * rows + row];
            }
        }
        for (indexType col = 0; col < dimsB.columns(); ++col) {
            for (indexType row = 0; row < rows; ++row) {
                ptrC[(col + dimsA.columns()) * rows + row] = ptrB[col * rows + row];
            }
        }
    }
    return ArrayOf(NLS_HANDLE, dimsC, pRes);
}
//=============================================================================
static ArrayOf
concatenateNd(const ArrayOf& A, const ArrayOf& B)
{
    // N-D operands: same element mapping as the numeric N-D concatenation.
    ArrayOfMatrix m;
    ArrayOfVector v;
    v.push_back(A);
    v.push_back(B);
    m.push_back(v);
    return ConcatenateNdArray(m, NLS_HANDLE);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
