//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FunctionHandleIsEqual.hpp"
#include "MacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FunctionHandleIsEqual(const ArrayOf& A, const ArrayOf& B)
{
    bool bRes = false;
    if (A.isFunctionHandle() && B.isFunctionHandle()) {
        function_handle fhA = A.getContentAsFunctionHandle();
        function_handle fhB = B.getContentAsFunctionHandle();
        bRes = (fhA.name == fhB.name) && (fhA.anonymousHandle == fhB.anonymousHandle);
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
