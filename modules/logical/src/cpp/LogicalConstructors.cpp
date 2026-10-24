//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LogicalConstructors.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseType.hpp"
//=============================================================================
namespace Nelson {
Dimensions
LogicalConstructorDimensions(const ArrayOfVector& argIn, sizeType nRhs)
{
    Dimensions dims;
    if (nRhs == 1) {
        ArrayOf arg = argIn[0];
        if (!(arg.isNumeric() || arg.dataClass() == NLS_LOGICAL) || arg.isSparse()) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"numeric value");
        }
        if (arg.isEmpty()) {
            // an empty size argument gives an empty array
            dims[0] = 0;
            dims[1] = 0;
            return dims;
        }
        if (!arg.isRowVector()) {
            Error(L"Nelson:expectedRow", L"%s must be a row vector.", L"Argument #1");
        }
        if (arg.elementCount() >= Nelson::maxDims) {
            RaiseError(
                resolveMessageTemplate(L"Nelson:constructors_functions:tooManyDimensionsCurrent",
                    L"Too many dimensions! Current limit is")
                    + L" " + std::to_wstring(Nelson::maxDims) + L".",
                L"Nelson:logical:tooManyDimensions");
        }
        ArrayOf dimVector(arg);
        auto* ptrValues = static_cast<const indexType*>(dimVector.indexPointer());
        ompIndexType elementCount = dimVector.elementCount();
        for (ompIndexType k = 0; k < elementCount; k++) {
            dims[k] = static_cast<indexType>(ptrValues[k]);
        }
    } else {
        for (sizeType k = 0; k < nRhs; k++) {
            dims[k] = argIn[k].isEmpty() ? 0 : argIn[k].asScalarIndex(true, true, true);
        }
    }
    if (dims.length() == 1) {
        dims[1] = dims[0];
    }
    dims.simplify();
    return dims;
}
//=============================================================================
ArrayOf
TrueConstructor(Dimensions& dim, bool bIsSparse)
{
    ArrayOf res;
    if (bIsSparse) {
        if (dim.length() > 2) {
            Error(L"Nelson:logical:ndSparseNotSupported",
                L"N-dimensional sparse arrays are not supported.");
        } else {
            void* pLogicalSparse = Eigen_LogicalSparseMatrixConstructor(
                dim.rows(), dim.columns(), static_cast<logical>(1));
            res = ArrayOf(NLS_LOGICAL, dim, pLogicalSparse, true);
        }
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.elementCount(), stringVector(), true));
        memset(pLogical, 1, sizeof(logical) * dim.elementCount());
        res = ArrayOf(NLS_LOGICAL, dim, (void*)pLogical, false);
    }
    return res;
}
//=============================================================================
ArrayOf
FalseConstructor(Dimensions& dim, bool bIsSparse)
{
    ArrayOf res;
    if (bIsSparse) {
        if (dim.length() > 2) {
            Error(L"Nelson:logical:ndSparseNotSupported",
                L"N-dimensional sparse arrays are not supported.");
        } else {
            void* pLogicalSparse = Eigen_LogicalSparseMatrixConstructor(
                dim.rows(), dim.columns(), static_cast<logical>(0));
            res = ArrayOf(NLS_LOGICAL, dim, pLogicalSparse, true);
        }
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.elementCount(), stringVector(), true));
        res = ArrayOf(NLS_LOGICAL, dim, (void*)pLogical, false);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
