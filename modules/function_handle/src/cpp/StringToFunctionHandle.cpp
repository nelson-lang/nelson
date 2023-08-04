//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <regex>
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isValidNelsonFunctionName(const std::string& functionName)
{
    std::regex pattern("^[_a-zA-Z][a-zA-Z0-9_]*$");
    return std::regex_match(functionName, pattern);
}
//=============================================================================
bool
isValidAnonymousFunction(const std::string& functionName)
{
    std::regex pattern("^\\s*@(\\s*\\(.*?\\)\\s*)\\s*(.*)$");
    return std::regex_match(functionName, pattern);
}
//=============================================================================
function_handle
StringToFunctionHandle(Evaluator* eval, const std::wstring& functionName)
{
    function_handle functionHandle;
    functionHandle.name.clear();
    functionHandle.anonymousHandle = nullptr;

    std::string trimmed = wstring_to_utf8(functionName);
    StringHelpers::trim_left(trimmed);
    StringHelpers::trim_right(trimmed);

    if (isValidNelsonFunctionName(trimmed)) {
        functionHandle.name = trimmed;
    } else {
        if (isValidAnonymousFunction(trimmed)) {
            std::string withoutArobase = trimmed;
            withoutArobase.erase(0, 1);
            AnonymousMacroFunctionDef* cp = nullptr;
            try {
                cp = new AnonymousMacroFunctionDef(trimmed);
            } catch (std::bad_alloc&) {
                cp = nullptr;
            } catch (Exception&) {
                cp = nullptr;
            }
            if (cp) {
                functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
            }
        } else {
            std::string withoutArobase = trimmed;
            withoutArobase.erase(0, 1);
            if (isValidNelsonFunctionName(withoutArobase)) {
                functionHandle.name = withoutArobase;
            }
        }
    }
    return functionHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
