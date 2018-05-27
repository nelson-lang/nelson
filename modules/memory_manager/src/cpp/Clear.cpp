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
#include "Clear.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
callClearHandle(Evaluator* eval, Scope* scope, std::string variable)
{
    bool res = false;
    ArrayOf val;
    if (scope->lookupVariable(variable, val)) {
        if (val.isHandle()) {
            Dimensions dimsVal = val.getDimensions();
            nelson_handle* qp = (nelson_handle*)val.getDataPointer();
            for (indexType k = 0; k < dimsVal.getElementCount(); k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    std::wstring handleTypeName = hlObj->getCategory();
                    if (!handleTypeName.empty()) {
                        std::wstring ufunctionNameClearHandle = handleTypeName + L"_clear";
                        std::string functionNameClearHandle
                            = wstring_to_utf8(ufunctionNameClearHandle);
                        Context* context = eval->getContext();
                        FunctionDef* funcDef = nullptr;
                        if (context->lookupFunction(functionNameClearHandle, funcDef)) {
                            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                                || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                                int nLhs = 0;
                                ArrayOfVector argIn;
                                nelson_handle* ptrObject
                                    = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, 1);
                                Dimensions dims(1, 1);
                                ptrObject[0] = hl;
                                argIn.push_back(ArrayOf(NLS_HANDLE, dims, (void*)ptrObject));
                                funcDef->evaluateFunction(eval, argIn, nLhs);
                                res = true;
                            }
                        }
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
bool
ClearVariable(Evaluator* eval, std::wstring variable)
{
    return ClearVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearVariable(Evaluator* eval, std::string variable)
{
    Scope* currentScope = eval->getContext()->getCurrentScope();
    if (currentScope->getName() == "base") {
        Scope* globalScope = eval->getContext()->getGlobalScope();
        globalScope->deleteVariable(variable);
    }
    callClearHandle(eval, currentScope, variable);
    return currentScope->deleteVariable(variable);
}
//=============================================================================
bool
ClearAllVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names = eval->getContext()->getCurrentScope()->getVariablesList(true);
    for (size_t k = 0; k < names.size(); k++) {
        if (ClearVariable(eval, names[k])) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
}
//=============================================================================
