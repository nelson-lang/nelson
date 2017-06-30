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
#include <Windows.h>
#include <Ole2.h>
#include <ocidl.h>
#include "DispComHandleObject.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "classnameComHandleObject.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static void DispComHandleObject(Interface *io, ComHandleObject *comHandle)
    {
        if (comHandle != nullptr)
        {
            std::wstring fullClassName;
            classnameComHandle(comHandle, fullClassName);
            io->outputMessage(L"\t" + fullClassName);
            io->outputMessage("\n");
        }
        io->outputMessage("\n");
    }
    //=============================================================================
    void DispComHandleObject(Evaluator *eval, ArrayOf A)
    {
        if (eval != nullptr)
        {
            Interface *io = eval->getInterface();
            if (io)
            {
                if (A.isHandle())
                {
                    if (A.isScalar())
                    {
                        nelson_handle *qp = (nelson_handle*)A.getDataPointer();
                        nelson_handle hl = qp[0];
                        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
                        if (hlObj->getCategory() != COM_CATEGORY_STR)
                        {
                            throw Exception(_W("COM handle expected."));
                        }
                        Dimensions dimsA = A.getDimensions();
                        io->outputMessage(L"[COM] - size: ");
                        dimsA.printMe(io);
                        io->outputMessage("\n");
                        io->outputMessage("\n");
                        ComHandleObject *comhandleobj = (ComHandleObject *)hlObj;
                        DispComHandleObject(io, comhandleobj);
                    }
                    else
                    {
                        Dimensions dimsA = A.getDimensions();
                        io->outputMessage(L"[COM] - size: ");
                        dimsA.printMe(io);
                        io->outputMessage("\n");
                    }
                }
                else
                {
                    Error(eval, _W("COM handle expected."));
                }
            }
        }
    }
    //=============================================================================
}
//=============================================================================
