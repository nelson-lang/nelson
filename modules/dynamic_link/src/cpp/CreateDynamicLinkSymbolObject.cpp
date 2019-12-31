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
#include "CreateDynamicLinkLibraryObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include "StringFormat.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
createDynamicLinkSymbolObject(ArrayOf dllibObject, const std::wstring& symbol,
    const std::wstring& returnType, const wstringVector& argumentsType)
{
    ArrayOf handle;
    HandleGenericObject* hlObj = dllibObject.getContentAsHandleScalar();
    if (hlObj->getCategory() != DLLIB_CATEGORY_STR) {
        Error(_W("Wrong type for argument #1: dllib scalar handle expected."));
    }
    auto* obj = (DynamicLinkLibraryObject*)hlObj;
    if (!obj->getPointer()) {
        Error(_W("Valid handle expected."));
    }
    if (!DynamicLinkSymbolObject::isValidDataType(returnType)) {
        Error(StringFormat(_W("Invalid argument type: %ls.").c_str(), returnType.c_str()));
    }
    for (std::wstring arg : argumentsType) {
        if (!DynamicLinkSymbolObject::isValidDataType(arg)) {
            Error(StringFormat(_W("Invalid argument type: %ls.").c_str(), arg.c_str()));
        }
    }
    void* ptr = obj->getFunctionPointer(wstring_to_utf8(symbol));
    if (!ptr) {
        Error(StringFormat(_W("Invalid symbol name: %ls").c_str(), symbol.c_str()));
    }
    DynamicLinkSymbolObject* dlSymbolObject
        = new DynamicLinkSymbolObject(dllibObject, ptr, symbol, returnType, argumentsType);
    return ArrayOf::handleConstructor(dlSymbolObject);
}
//=============================================================================
static bool
checkParamType(const std::wstring& paramType)
{
    wstringVector supportedType = { L"void", L"logical", L"uint8", L"int8", L"uint16", L"int16",
        L"uint32", L"int32", L"uint64", L"int64", L"single", L"double", L"single", L"double",
        L"char", L"voidPtr", L"logicalPtr", L"uint8Ptr", L"int8Ptr", L"uint16Ptr", L"int16Ptr",
        L"uint32Ptr", L"int32Ptr", L"uint64Ptr", L"int64Ptr", L"singlePtr", L"doublePtr",
        L"singlePtr", L"doublePtr", L"charPtr" };
    auto it = std::find(supportedType.begin(), supportedType.end(), paramType);
    return (it != supportedType.end());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
