//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JsonTableIOHelpers.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cwctype>
//=============================================================================
namespace Nelson::SpreadsheetGateway {
//=============================================================================
static std::wstring
lowerText(std::wstring value);
static bool
isTextScalar(const ArrayOf& value);
static bool
findFileType(const ArrayOfVector& argIn, size_t first, std::wstring& fileType);
static bool
hasJsonExtension(const std::wstring& filename);
//=============================================================================
bool
isJsonReadRequest(const std::wstring& filename, const ArrayOfVector& argIn, size_t first)
{
    if (argIn.size() > first && argIn[first].isClassType()
        && argIn[first].classType() == NELSON_JSON_IMPORT_OPTIONS_CLASS) {
        return true;
    }
    std::wstring fileType;
    if (findFileType(argIn, first, fileType)) {
        return fileType == L"json";
    }
    return hasJsonExtension(filename);
}
//=============================================================================
bool
isJsonWriteRequest(const std::wstring& filename, const ArrayOfVector& argIn, size_t first)
{
    std::wstring fileType;
    if (findFileType(argIn, first, fileType)) {
        return fileType == L"json";
    }
    return hasJsonExtension(filename);
}
//=============================================================================
ArrayOfVector
callJsonTableFunction(Evaluator* eval, const std::string& name, const ArrayOfVector& args, int nLhs)
{
    Context* context = eval->getContext();
    FunctionDef* functionDef = nullptr;
    if (!context->lookupFunction(name, functionDef)) {
        Error("Nelson:spreadsheet:jsonFunctionNotFound", "Function not found: %s", name);
    }
    return functionDef->evaluateFunction(eval, args, nLhs);
}
//=============================================================================
std::wstring
lowerText(std::wstring value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](wchar_t c) { return (wchar_t)std::towlower(c); });
    return value;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
bool
findFileType(const ArrayOfVector& argIn, size_t first, std::wstring& fileType)
{
    for (size_t k = first; k + 1 < argIn.size(); ++k) {
        if (isTextScalar(argIn[k]) && lowerText(argIn[k].asWideString()) == L"filetype"
            && isTextScalar(argIn[k + 1])) {
            fileType = lowerText(argIn[k + 1].asWideString());
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
hasJsonExtension(const std::wstring& filename)
{
    size_t dot = filename.rfind(L'.');
    return dot != std::wstring::npos && lowerText(filename.substr(dot)) == L".json";
}
//=============================================================================
} // namespace Nelson::SpreadsheetGateway
//=============================================================================
