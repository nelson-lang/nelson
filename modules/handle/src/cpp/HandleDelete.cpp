//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleDelete.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
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
                std::wstring handleTypeName = hlObj->getCategory();
                if (!handleTypeName.empty()) {
                    std::wstring ufunctionNameClearHandle = handleTypeName + L"_delete";
                    std::string functionNameClearHandle = wstring_to_utf8(ufunctionNameClearHandle);
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
                    std::wstring msg;
                    if (handleTypeName.empty()) {
                        msg = L"delete " + _W("not defined.");
                    } else {
                        msg = handleTypeName + L"_delete" + L" " + _W("not defined.");
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
