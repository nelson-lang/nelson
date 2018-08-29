//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_invokeBuiltin.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_invokeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isEmpty()) {
        nelson_handle* qp = (nelson_handle*)param1.getDataPointer();
        std::wstring handleTypeName = utf8_to_wstring(NLS_HANDLE_STR);
        if (qp) {
            Dimensions dimsParam1 = param1.getDimensions();
            for (indexType k = 0; k < dimsParam1.getElementCount(); k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    std::wstring currentType = hlObj->getCategory();
                    if (currentType != L"" || currentType != utf8_to_wstring(NLS_HANDLE_STR)) {
                        handleTypeName = currentType;
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
            retval.push_back(ArrayOf::emptyConstructor(dims));
        }
    }
    return retval;
}
//=============================================================================
