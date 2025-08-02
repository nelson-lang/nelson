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
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
HandleDelete(Evaluator* eval, const ArrayOf& A)
{
    if (A.isHandle()) {
        Dimensions dimsA = A.getDimensions();
        auto* qp = (nelson_handle*)A.getDataPointer();
        indexType elementCount = dimsA.getElementCount();
        for (indexType k = 0; k < elementCount; k++) {
            nelson_handle hl = qp[k];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj != nullptr) {
                bool doOverload = false;
                std::string handleTypeName = hlObj->getCategory();
                if (!handleTypeName.empty()) {
                    std::string functionNameClearHandle
                        = getOverloadFunctionName(handleTypeName, "delete");
                    Context* context = eval->getContext();
                    FunctionDef* funcDef = nullptr;
                    if (context->lookupFunction(functionNameClearHandle, funcDef)) {
                        if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                            || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                            int nLhs = 0;
                            ArrayOfVector argIn;
                            nelson_handle* ptrObject = static_cast<nelson_handle*>(
                                ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
                            Dimensions dims(1, 1);
                            ptrObject[0] = hl;
                            argIn.push_back(ArrayOf(NLS_HANDLE, dims, (void*)ptrObject));
                            funcDef->evaluateFunction(eval, argIn, nLhs);
                            doOverload = true;
                        }
                    }
                }
                if (!doOverload) {
                    std::string msg;
                    if (handleTypeName.empty()) {
                        msg = "delete " + _("not defined.");
                    } else {
                        std::string overloadName
                            = getOverloadFunctionName(handleTypeName, "delete");
                        msg = overloadName + " " + _("not defined.");
                    }
                    Error(msg);
                }
            }
            HandleManager::getInstance()->removeHandle(hl);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
