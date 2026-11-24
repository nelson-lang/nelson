//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiDialogsHelpers.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson::GuiDialogHelpers {
//=============================================================================
wstringVector
asTextList(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return { value.asWideString() };
    }
    if (value.isStringArray()) {
        return value.asWideStringVector(false);
    }
    if (value.isCellArrayOfCharacterVectors()) {
        return value.asWideStringVector();
    }
    Error(L"Nelson:gui:invalidArgument", L"'%s' expected.", L"Text");
    return {};
}
//=============================================================================
std::wstring
asText(const ArrayOf& value)
{
    wstringVector lines = asTextList(value);
    std::wstring text;
    for (size_t k = 0; k < lines.size(); ++k) {
        if (k != 0) {
            text += L"\n";
        }
        text += lines[k];
    }
    return text;
}
//=============================================================================
std::wstring
argText(const ArrayOfVector& args, size_t index, const std::wstring& defaultValue)
{
    return args.size() > index ? args[index].asWideString() : defaultValue;
}
//=============================================================================
bool
isName(const ArrayOf& value, const std::wstring& expected)
{
    return StringHelpers::iequals(value.asWideString(), expected);
}
//=============================================================================
double
numericElement(const ArrayOf& value, indexType index, double defaultValue)
{
    if (value.elementCount() <= index) {
        return defaultValue;
    }
    std::vector<double> values = value.asDoubleVector();
    return values[index];
}
//=============================================================================
FunctionDef*
findFunction(const std::string& name)
{
    FunctionDef* funcDef = nullptr;
    if (!PathFunctionIndexerManager::getInstance()->find(name, funcDef)) {
        BuiltInFunctionDefManager::getInstance()->find(name, funcDef);
    }
    if (funcDef == nullptr) {
        Error(L"Nelson:UI:functionNotFound", L"Required function not found.");
    }
    return funcDef;
}
//=============================================================================
} // namespace Nelson::GuiDialogHelpers
//=============================================================================
