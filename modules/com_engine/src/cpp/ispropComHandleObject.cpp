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
#include "ispropComHandleObject.hpp"
#include "Exception.hpp"
#include "HandleManager.hpp"
#include "ComHandleObject.hpp"
#include "characters_encoding.hpp"
#include "ComHelpers.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool ispropComHandleObject(ComHandleObject *comhandleobj, const std::wstring &propertyName)
    {
        void *ptr = comhandleobj->getPointer();
        if (ptr == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        VARIANT *pVariant = (VARIANT *)ptr;
        return isPropertyGetCom(pVariant->pdispVal, propertyName) || isPropertyPutCom(pVariant->pdispVal, propertyName);
    }
    //=============================================================================
    ArrayOf ispropComHandleObject(ArrayOf A, const std::wstring &propertyName)
    {
        if (!A.isHandle())
        {
            throw Exception(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
        }
        if (!A.isScalar())
        {
            throw Exception(ERROR_SIZE_SCALAR_EXPECTED);
        }
        nelson_handle *qp = (nelson_handle*)A.getDataPointer();
        if (qp == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        nelson_handle hl = qp[0];
        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
        if (hlObj == nullptr)
        {
            throw Exception(_W("COM valid handle expected."));
        }
        if (hlObj->getCategory() != COM_CATEGORY_STR)
        {
            throw Exception(_W("COM handle expected."));
        }
        ComHandleObject *comhandleobj = (ComHandleObject *)hlObj;
        bool res = ispropComHandleObject(comhandleobj, propertyName);
        return ArrayOf::logicalConstructor(res);
    }
    //=============================================================================
}
//=============================================================================
