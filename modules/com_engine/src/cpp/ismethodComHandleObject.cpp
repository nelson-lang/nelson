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
#include <Windows.h>
#include "ismethodComHandleObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "ComHandleObject.hpp"
#include "characters_encoding.hpp"
#include "ComHelpers.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool ismethodComHandleObject(ComHandleObject *comhandleobj, const std::wstring &methodname)
    {
        void *ptr = comhandleobj->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        VARIANT *pVariant = (VARIANT *)ptr;
        return isMethodCom(pVariant->pdispVal, methodname);
    }
    //=============================================================================
    ArrayOf ismethodComHandleObject(ArrayOf A, const std::wstring &methodname)
    {
        HandleGenericObject *hlObj = A.getContentAsHandleScalar();
        if (hlObj->getCategory() != COM_CATEGORY_STR)
        {
            throw Exception(_W("COM handle expected."));
        }
        ComHandleObject *comhandleobj = (ComHandleObject *)hlObj;
        bool res = ismethodComHandleObject(comhandleobj, methodname);
        return ArrayOf::logicalConstructor(res);
    }
    //=============================================================================
}
//=============================================================================
