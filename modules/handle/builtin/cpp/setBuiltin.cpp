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
#include "setBuiltin.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::setBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (param1.isGraphicObject()) {
        bool doOverload = false;
        std::wstring ufunctionNameGetHandle = ClassToString(param1.getDataClass()) + L"_set";
        std::string functionNameGetHandle = wstring_to_utf8(ufunctionNameGetHandle);
        Context* context = eval->getContext();
        FunctionDef* funcDef = nullptr;
        if (context->lookupFunction(functionNameGetHandle, funcDef)) {
            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                ArrayOfVector argInCopy(argIn);
                funcDef->evaluateFunction(eval, argInCopy, nLhs);
                doOverload = true;
            }
        }
        if (!doOverload) {
            std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
            Error(msg);
        }
    } else {
        if (!param1.isEmpty()) {
            auto* qp = (nelson_handle*)param1.getDataPointer();
            if (qp) {
                std::wstring handleTypeName = utf8_to_wstring(NLS_HANDLE_STR);
                Dimensions dimsParam1 = param1.getDimensions();
                indexType elementCount = dimsParam1.getElementCount();
                for (indexType k = 0; k < dimsParam1.getElementCount(); k++) {
                    nelson_handle hl = qp[k];
                    HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                    if (hlObj) {
                        std::wstring currentType = hlObj->getCategory();
                        if (!currentType.empty()
                            || currentType != utf8_to_wstring(NLS_HANDLE_STR)) {
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
                            ArrayOfVector argInCopy(argIn);
                            funcDef->evaluateFunction(eval, argInCopy, nLhs);
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
    }
    return retval;
}
//=============================================================================
