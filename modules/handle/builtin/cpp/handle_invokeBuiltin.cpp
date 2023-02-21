//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_invokeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_invokeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isEmpty()) {
        auto* qp = (nelson_handle*)param1.getDataPointer();
        if (qp) {
            std::wstring handleTypeName = utf8_to_wstring(NLS_HANDLE_STR);
            Dimensions dimsParam1 = param1.getDimensions();
            indexType elementCount = dimsParam1.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    std::wstring currentType = hlObj->getCategory();
                    if (!currentType.empty() || currentType != utf8_to_wstring(NLS_HANDLE_STR)) {
                        handleTypeName.assign(currentType);
                        break;
                    }
                }
            }
            if (handleTypeName != utf8_to_wstring(NLS_HANDLE_STR)) {
                bool doOverload = false;
                std::wstring ufunctionNameGetHandle = handleTypeName + L"_set";
                std::string functionNameGetHandle = wstring_to_utf8(ufunctionNameGetHandle);
                Context* context = eval->getContext();
                FunctionDef* funcDef = nullptr;
                if (context->lookupFunction(functionNameGetHandle, funcDef)) {
                    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                        ArrayOfVector args;
                        args.push_back(param1);
                        funcDef->evaluateFunction(eval, args, nLhs);
                        doOverload = true;
                    }
                }
                if (!doOverload) {
                    std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
                    Error(msg);
                }
            } else {
                Error(_W("Invalid handle."));
            }
        } else {
            Error(_W("Invalid handle."));
        }
    } else {
        if (nLhs > 0) {
            Dimensions dims(0, 0);
            retval << ArrayOf::emptyConstructor(dims);
        }
    }
    return retval;
}
//=============================================================================
