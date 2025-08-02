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
#include "i18n.hpp"
#include "SparseDynamicFunctions.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
TrueConstructor(Dimensions& dim, bool bIsSparse)
{
    ArrayOf res;
    if (bIsSparse) {
        if (dim.getLength() > 2) {
            Error(_W("N-dimensional sparse arrays are not supported."));
        } else {
            void* pLogicalSparse = LogicalSparseMatrixConstructorDynamicFunction(
                dim.getRows(), dim.getColumns(), static_cast<logical>(1));
            res = ArrayOf(NLS_LOGICAL, dim, pLogicalSparse, true);
        }
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.getElementCount(), stringVector(), true));
        memset(pLogical, 1, sizeof(logical) * dim.getElementCount());
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
        if (dim.getLength() > 2) {
            Error(_W("N-dimensional sparse arrays are not supported."));
        } else {
            void* pLogicalSparse = LogicalSparseMatrixConstructorDynamicFunction(
                dim.getRows(), dim.getColumns(), static_cast<logical>(0));
            res = ArrayOf(NLS_LOGICAL, dim, pLogicalSparse, true);
        }
    } else {
        logical* pLogical = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.getElementCount(), stringVector(), true));
        res = ArrayOf(NLS_LOGICAL, dim, (void*)pLogical, false);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
