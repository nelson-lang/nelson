//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FunctionHandleIsEqual.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FunctionHandleIsEqual(const ArrayOf& A, const ArrayOf& B)
{
    if (A.isFunctionHandle() && B.isFunctionHandle()) {
        function_handle fhA = A.getContentAsFunctionHandle();
        function_handle fhB = B.getContentAsFunctionHandle();
        bool bRes = (fhA.anonymousHandle == fhB.anonymousHandle);
        if (bRes) {
            return true;
        }
        if (!fhA.anonymousHandle || !fhB.anonymousHandle) {
            return false;
        }
        AnonymousMacroFunctionDef* anonymousA
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fhA.anonymousHandle);
        AnonymousMacroFunctionDef* anonymousB
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fhB.anonymousHandle);
        if (anonymousA->isFunctionHandle() && anonymousB->isFunctionHandle()) {
            return anonymousA->toString() == anonymousB->toString();
        } else if (!anonymousA->isFunctionHandle() && !anonymousB->isFunctionHandle()) {
            if (anonymousA->toString() == anonymousB->toString()) {
                stringVector variableNamesA = anonymousA->getVariableNames();
                stringVector variableNamesB = anonymousB->getVariableNames();
                if (variableNamesA.size() == variableNamesB.size()) {
                    return variableNamesA == variableNamesB;
                }
            }
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
