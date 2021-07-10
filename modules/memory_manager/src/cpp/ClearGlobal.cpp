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
#include <boost/algorithm/string.hpp>
#include "ClearGlobal.hpp"
#include "HandleManager.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
callClearHandle(Evaluator* eval, Scope* scope, const std::string& variable)
{
    bool res = false;
    ArrayOf val;
    if (scope->lookupVariable(variable, val)) {
        if (val.isHandle()) {
            Dimensions dimsVal = val.getDimensions();
            auto* qp = (nelson_handle*)val.getDataPointer();
            indexType elementCount = dimsVal.getElementCount();
            for (indexType k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
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
                                nelson_handle* ptrObject = static_cast<nelson_handle*>(
                                    ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
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
ClearGlobalVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearGlobalVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearGlobalVariable(Evaluator* eval, const std::string& variable)
{
    Scope* globalScope = eval->getContext()->getGlobalScope();
    callClearHandle(eval, globalScope, variable);
    bool res = eval->getContext()->getGlobalScope()->deleteVariable(variable);
    return res;
}
//=============================================================================
bool
ClearAllGlobalVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    eval->getContext()->getGlobalScope()->getVariablesList(true, names);
    for (const auto& name : names) {
        if (!eval->getContext()->getGlobalScope()->deleteVariable(name)) {
            bUnlocked = false;
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearAllPersistentVariables(Evaluator* eval)
{
    bool bUnlocked = true;
    stringVector names;
    Scope* globalScope = eval->getContext()->getGlobalScope();
    globalScope->getVariablesList(true, names);
    for (const auto& name : names) {
        if (!globalScope->isVariablePersistent(name)) {
            if (!globalScope->deleteVariable(name)) {
                bUnlocked = false;
            }
        }
    }
    return bUnlocked;
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::wstring& variable)
{
    return ClearPersistentVariable(eval, wstring_to_utf8(variable));
}
//=============================================================================
bool
ClearPersistentVariable(Evaluator* eval, const std::string& variable)
{
    bool res = false;
    FuncPtr func = nullptr;
    std::string path;

    if (FunctionsInMemory::getInstance()->find(variable, func)) { 
      if (func->type() == NLS_MACRO_FUNCTION) { 
        stringVector allVariableNames;
          eval->getContext()->getGlobalScope()->getVariablesList(true, allVariableNames);
          for (std::string name : allVariableNames) {
              if (boost::algorithm::starts_with(name, "_" + variable + "_")) {
                  res = res || eval->getContext()->getGlobalScope()->deleteVariable(name);
              }
          }
      }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
