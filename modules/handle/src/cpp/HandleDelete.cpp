//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleDelete.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "ClassdefHandleObject.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define HANDLE_DELETE_FUNCTION "delete"
//=============================================================================
void
HandleDelete(Evaluator* eval, const ArrayOf& A)
{
    if (A.isHandle()) {
        Dimensions dimsA = A.dimensions();
        auto* qp = (HandleRef*)A.dataPointer();
        indexType elementCount = dimsA.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            HandleRef ref = qp[k];
            if (!ref || !ref->isValid()) {
                continue;
            }
            nelson_handle hl = ref->id();
            HandleGenericObject* hlObj = ref->object();
            if (hlObj != nullptr) {
                if (dynamic_cast<ClassdefHandleObject*>(hlObj) != nullptr) {
                    // classdef objects share the destructor protocol used by the
                    // last-reference finalization (errors propagate here)
                    RunClassdefDelete(eval, ref, false);
                    continue;
                }
                std::string handleTypeName = hlObj->category();
                bool doOverload = false;
                if (!handleTypeName.empty()) {
                    std::string functionNameClearHandle
                        = getOverloadFunctionName(handleTypeName, HANDLE_DELETE_FUNCTION);
                    Context* context = eval->getContext();
                    FunctionDef* funcDef = nullptr;
                    if (context->lookupFunction(functionNameClearHandle, funcDef)) {
                        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                            || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                            int nLhs = 0;
                            ArrayOfVector argIn;
                            argIn.push_back(ArrayOf::handle(hl));
                            funcDef->evaluateFunction(eval, argIn, nLhs);
                            doOverload = true;
                        }
                    }
                }
                if (!doOverload) {
                    ref->invalidate(eval);
                }
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
