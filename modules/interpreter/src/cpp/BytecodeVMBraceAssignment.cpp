//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Brace read of the parent level of a chained assignment (x{2}.a = v,
// x{2}(1) = v, c{1}{1} = v).
//=============================================================================
#include "BytecodeVMInternal.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
static bool
braceReadTargetsBeyondCellBounds(const ArrayOf& base, const ArrayOfVector& indices);
//=============================================================================
bool
braceAssignmentParentIsNew(const ArrayOf& base, const ArrayOfVector& indices)
{
    if (base.isEmpty()) {
        return true;
    }
    if (!base.isCell() && !base.isStringArray()) {
        Error(L"Nelson:cellAssToNonCell",
            L"Unable to perform assignment because brace indexing is not supported for "
            L"variables of this type.");
    }
    return braceReadTargetsBeyondCellBounds(base, indices);
}
//=============================================================================
// Colon and empty subscripts never exceed the existing range and are left to
// the normal read.
bool
braceReadTargetsBeyondCellBounds(const ArrayOf& base, const ArrayOfVector& indices)
{
    if (!base.isCell() || base.isSparse() || indices.empty()) {
        return false;
    }
    for (const auto& index : indices) {
        if (index.isRowVectorCharacterArray() || index.isEmpty()) {
            return false;
        }
    }
    if (indices.size() == 1) {
        ArrayOf linearIndex(indices[0]);
        linearIndex.toOrdinalType();
        return linearIndex.maxAsIndex() > base.elementCount();
    }
    for (size_t d = 0; d < indices.size(); ++d) {
        ArrayOf subscript(indices[d]);
        subscript.toOrdinalType();
        if (subscript.maxAsIndex() > base.dimensionLength(static_cast<int>(d))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
