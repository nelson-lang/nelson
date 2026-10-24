//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <string>
#include "CreateDynamicLinkLibraryObject.hpp"
#include "DynamicLinkSymbolObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static stringVector
getPossibleSymbolNames(const std::string& userSymbolName)
{
    std::string cleanedSymbolName = userSymbolName;
    if (cleanedSymbolName[0] == '_') {
        cleanedSymbolName.erase(0, 1);
    }
    if (cleanedSymbolName.back() == '_') {
        cleanedSymbolName.pop_back();
    }
    std::string cleanedSymbolNameUpperCase = StringHelpers::to_upper_copy(cleanedSymbolName);
    std::string cleanedSymbolNameLowerCase = StringHelpers::to_lower_copy(cleanedSymbolName);
    stringVector possibleSymbolNames;
    possibleSymbolNames.reserve(8);
    possibleSymbolNames.push_back("_" + cleanedSymbolNameLowerCase);
    possibleSymbolNames.push_back(cleanedSymbolNameLowerCase);
    possibleSymbolNames.push_back(cleanedSymbolNameLowerCase + "_");
    possibleSymbolNames.push_back("_" + cleanedSymbolNameLowerCase + "_");
    possibleSymbolNames.push_back("_" + cleanedSymbolNameUpperCase);
    possibleSymbolNames.push_back(cleanedSymbolNameUpperCase);
    possibleSymbolNames.push_back(cleanedSymbolNameUpperCase + "_");
    possibleSymbolNames.push_back("_" + cleanedSymbolNameUpperCase + "_");
    return possibleSymbolNames;
}
//=============================================================================
ArrayOf
createDynamicLinkSymbolObject(const ArrayOf& dllibObject, const std::wstring& symbol,
    const std::wstring& returnType, const wstringVector& argumentsType)
{
    ArrayOf handle;
    HandleGenericObject* hlObj = dllibObject.asHandleScalar();
    if (hlObj->category() != NLS_HANDLE_DLLIB_CATEGORY_STR) {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", 1, L"dllib scalar handle");
    }
    auto* obj = (DynamicLinkLibraryObject*)hlObj;
    if (!obj->pointer()) {
        Error(L"Nelson:dlsym:invalidLibraryHandle", L"Valid handle expected.");
    }
    if (!DynamicLinkSymbolObject::isValidDataType(returnType)) {
        Error(L"Nelson:dlsym:invalidArgumentType", L"Invalid argument type: %s.", returnType);
    }
    for (const std::wstring& arg : argumentsType) {
        if (!DynamicLinkSymbolObject::isValidDataType(arg)) {
            Error(L"Nelson:dlsym:invalidArgumentType", L"Invalid argument type: %s.", arg);
        }
    }
    std::wstring symbolUsed = symbol;
    std::string utf8Symbol = wstring_to_utf8(symbol);
    void* ptr = obj->getFunctionPointer(utf8Symbol);
    if (!ptr) {
        stringVector symbolNames = getPossibleSymbolNames(utf8Symbol);
        stringVector symbolsFound;
        for (const auto& name : symbolNames) {
            void* ptrSymbolName = obj->getFunctionPointer(name);
            if (ptrSymbolName) {
                ptr = ptrSymbolName;
                symbolsFound.push_back(name);
                symbolUsed = utf8_to_wstring(name);
            }
        }
        if (symbolsFound.size() > 1) {
            ptr = nullptr;
            Error("Nelson:dlsym:multipleSymbolsFound", "Multiple possible symbol name found: %s",
                StringHelpers::join(symbolsFound, ", "));
        }
    }
    if (!ptr) {
        Error(L"Nelson:dlsym:symbolNotFound", L"Invalid symbol name: %s", symbol);
    }

    DynamicLinkSymbolObject* dlSymbolObject
        = new DynamicLinkSymbolObject(dllibObject, ptr, symbolUsed, returnType, argumentsType);
    return ArrayOf::handle(dlSymbolObject);
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
