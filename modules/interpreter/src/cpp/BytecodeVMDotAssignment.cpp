//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Target checks of a dot assignment (x.b = v, s.a.b = v, x(2).b = v) on a
// value that is neither an object nor a handle. Cold: only reached on the
// error path.
//=============================================================================
#include "BytecodeVMInternal.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson::bcvm_detail {
//=============================================================================
void
raiseStructAssToNonStruct()
{
    Error(L"Nelson:structAssToNonStruct",
        L"Unable to perform assignment because dot indexing is not supported for variables of "
        L"this type.");
}
//=============================================================================
void
checkDotAssignTarget(const ArrayOf& base)
{
    if (base.isFunctionHandle() || (base.isStringArray() && !base.isEmpty())) {
        Error(L"Nelson:interpreter:invalidFieldAssignment",
            L"Cannot apply A.field_name = B to non struct-array object A.");
    }
    if (!base.isEmpty() && !base.isStruct()) {
        raiseStructAssToNonStruct();
    }
}
//=============================================================================
void
checkDotAssignParent(const ArrayOf& base)
{
    if (!base.isEmpty() && !base.isStruct() && !base.isStringArray() && !base.isClassType()
        && !base.isHandle() && !base.isGraphicsObject()) {
        raiseStructAssToNonStruct();
    }
}
//=============================================================================
} // namespace Nelson::bcvm_detail
//=============================================================================
