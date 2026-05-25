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
#include "ClassdefHandleObject.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define HANDLE_DELETE_EVENT_OBJECT_BEING_DESTROYED "ObjectBeingDestroyed"
#define HANDLE_DELETE_FUNCTION "delete"
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
                std::string handleTypeName = hlObj->getCategory();
                auto* classdefObject = dynamic_cast<ClassdefHandleObject*>(hlObj);
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
                            argIn.push_back(ArrayOf::handleConstructor(hl));
                            funcDef->evaluateFunction(eval, argIn, nLhs);
                            doOverload = true;
                        }
                    }
                }
                if (classdefObject != nullptr && doOverload) {
                    continue;
                }
                if (classdefObject != nullptr) {
                    classdefObject->notifyEvent(eval, hl,
                        HANDLE_DELETE_EVENT_OBJECT_BEING_DESTROYED, ArrayOf::emptyConstructor());
                    hlObj = HandleManager::getInstance()->getPointer(hl);
                    if (hlObj == nullptr) {
                        continue;
                    }
                }
                if (!doOverload) {
                    std::string msg;
                    if (handleTypeName.empty()) {
                        msg = std::string(HANDLE_DELETE_FUNCTION) + " " + _("not defined.");
                    } else {
                        std::string overloadName
                            = getOverloadFunctionName(handleTypeName, HANDLE_DELETE_FUNCTION);
                        msg = overloadName + " " + _("not defined.");
                    }
                    Error(msg);
                }
            }
            HandleGenericObject* liveObject = HandleManager::getInstance()->getPointer(hl);
            if (liveObject != nullptr) {
                auto* classdefObject = dynamic_cast<ClassdefHandleObject*>(liveObject);
                if (classdefObject != nullptr) {
                    classdefObject->deleteListeners();
                    delete classdefObject;
                }
            }
            HandleManager::getInstance()->removeHandle(hl);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
