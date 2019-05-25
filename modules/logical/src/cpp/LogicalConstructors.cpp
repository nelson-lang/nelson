//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "LogicalConstructors.hpp"
#include "Error.hpp"
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
        logical* pLogical
            = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.getElementCount()));
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
        logical* pLogical
            = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, dim.getElementCount()));
        res = ArrayOf(NLS_LOGICAL, dim, (void*)pLogical, false);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
