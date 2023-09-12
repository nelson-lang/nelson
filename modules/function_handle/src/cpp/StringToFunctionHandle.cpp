//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Error.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
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
    if (functionName.size() > 0) {
        size_t pos = 0;
        if (functionName[0] == '@') {
            pos = 1;
        }
        if (!(isalpha(functionName[pos]) || functionName[pos] == '_')) {
            return false;
        }
        for (size_t i = pos; i < functionName.size(); ++i) {
            char currentChar = functionName[i];
            if (!(isalnum(currentChar) || currentChar == '_')) {
                return false;
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
function_handle
StringToFunctionHandle(const std::wstring& functionName)
{
    std::string trimmed = wstring_to_utf8(functionName);
    StringHelpers::trim_left(trimmed);
    StringHelpers::trim_right(trimmed);
    if (trimmed.empty()) {
        Error(_("Invalid function name ''"), "Nelson:dispatcher:invalidFunctionName");
    }
    if (trimmed[0] != '@') {
        trimmed = "@" + trimmed;
    }
    function_handle functionHandle;
    functionHandle.name.clear();
    functionHandle.anonymousHandle = nullptr;

    if (isValidNelsonFunctionName(trimmed)) {
        if (trimmed[0] == '@') {
            std::string withoutArobase = trimmed;
            withoutArobase.erase(0, 1);
            functionHandle.name = withoutArobase;
        } else {
            functionHandle.name = trimmed;
        }
    } else {
        AnonymousMacroFunctionDef* cp = nullptr;
        try {
            cp = new AnonymousMacroFunctionDef(trimmed);
        } catch (std::bad_alloc&) {
            cp = nullptr;
        } catch (Exception&) {
            delete cp;
            cp = nullptr;
        }
        if (cp) {
            functionHandle.anonymousHandle = reinterpret_cast<nelson_handle*>(cp);
        } else {
            Error(_("A valid function name expected."), "Nelson:dispatcher:invalidFunctionName");
        }
    }
    return functionHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
