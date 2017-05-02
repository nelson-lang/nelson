//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "HandleDisplay.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    void HandleDisplay(Evaluator *eval, ArrayOf A)
    {
        if (eval)
        {
            bool doOverload = false;
            std::wstring handleTypeName = utf8_to_wstring(NLS_HANDLE_STR);
            Interface *io = eval->getInterface();
            if (io)
            {
                Dimensions dimsA = A.getDimensions();
                if (!A.isEmpty())
                {
                    nelson_handle *qp = (nelson_handle*)A.getDataPointer();
                    for (indexType k = 0; k < dimsA.getElementCount(); k++)
                    {
                        nelson_handle hl = qp[k];
                        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
                        if (hlObj)
                        {
                            std::wstring currentType = hlObj->getCategory();
                            if (currentType != L"" || currentType != utf8_to_wstring(NLS_HANDLE_STR))
                            {
                                handleTypeName = currentType;
                                break;
                            }
                        }
                    }
                    if (handleTypeName != utf8_to_wstring(NLS_HANDLE_STR))
                    {
                        std::wstring ufunctionNameDispHandle = handleTypeName + L"_disp";
                        std::string functionNameDispHandle = wstring_to_utf8(ufunctionNameDispHandle);
                        Context *context = eval->getContext();
                        FunctionDef *funcDef = nullptr;
                        if (context->lookupFunction(functionNameDispHandle, funcDef))
                        {
                            if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION))
                            {
                                int nLhs = 0;
                                ArrayOfVector argIn;
                                argIn.push_back(A);
                                funcDef->evaluateFunction(eval, argIn, nLhs);
                                doOverload = true;
                            }
                        }
                    }
                }
                if (!doOverload)
                {
                    io->outputMessage(L"[" + handleTypeName + L"] - size: ");
                    dimsA.printMe(io);
                    io->outputMessage("\n");
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
