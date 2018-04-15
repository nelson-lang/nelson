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
#include "CreateDynamicLinkLibraryObject.hpp"
#include "Exception.hpp"
#include "dynamic_library.hpp"
#include "characters_encoding.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "HandleManager.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf createDynamicLinkSymbolObject(ArrayOf dllibObject,
                                          std::wstring symbol,
                                          std::wstring returnType,
                                          wstringVector argumentsType)
    {
        ArrayOf handle;
        if (!dllibObject.isScalar())
        {
            throw Exception(_W("Wrong size for argument #1: dllib scalar handle expected."));
        }
        std::wstring classname;
        ClassName(dllibObject, classname);
        if (DLLIB_CATEGORY_STR != classname)
        {
            throw Exception(_W("Wrong type for argument #1: dllib scalar handle expected."));
        }
        nelson_handle *qp = (nelson_handle*)dllibObject.getDataPointer();
        nelson_handle hl = qp[0];
        HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
        DynamicLinkLibraryObject *obj = (DynamicLinkLibraryObject *)hlObj;
        if (!obj->getPointer())
        {
            Exception(_W("Valid handle expected."));
        }
        if (!DynamicLinkSymbolObject::isValidParamType(returnType, true))
        {
            throw Exception(_W("Invalid argument type:") + returnType);
        }
        for (std::wstring arg : argumentsType)
        {
            if (!DynamicLinkSymbolObject::isValidParamType(arg, false))
            {
                throw Exception(_W("Invalid argument type:") + arg);
            }
        }
        void *ptr = obj->getFunctionPointer(wstring_to_utf8(symbol));
        if (!ptr)
        {
            throw Exception(_W("Invalid symbol name."));
        }
        DynamicLinkSymbolObject *dlSymbolObject = new DynamicLinkSymbolObject(dllibObject, ptr, symbol, returnType, argumentsType);
        return ArrayOf::handleConstructor(dlSymbolObject);
    }
    //=============================================================================
    bool checkParamType(std::wstring paramType)
    {
        wstringVector supportedType =
        {
            L"void",
            L"logical",
            L"uint8",
            L"int8",
            L"uint16",
            L"int16",
            L"uint32",
            L"int32",
            L"uint64",
            L"int64",
            L"single",
            L"double",
            L"single",
            L"double",
            L"char",
            L"voidPtr",
            L"logicalPtr",
            L"uint8Ptr",
            L"int8Ptr",
            L"uint16Ptr",
            L"int16Ptr",
            L"uint32Ptr",
            L"int32Ptr",
            L"uint64Ptr",
            L"int64Ptr",
            L"singlePtr",
            L"doublePtr",
            L"singlePtr",
            L"doublePtr",
            L"charPtr"
        };
        auto it = std::find(supportedType.begin(), supportedType.end(), paramType);
        return (it != supportedType.end());
    }
    //=============================================================================
}
//=============================================================================
