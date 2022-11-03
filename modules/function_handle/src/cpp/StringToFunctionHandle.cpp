//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
function_handle
StringToFunctionHandle(Evaluator* eval, const std::wstring& functionName)
{
    function_handle functionHandle;
    functionHandle.name.clear();
    functionHandle.anonymousHandle = nullptr;

    if (IsValidVariableName(functionName)) {
        functionHandle.name = wstring_to_utf8(functionName);
    } else {
        std::string trimmed = wstring_to_utf8(functionName);
        StringHelpers::trim_left(trimmed);
        StringHelpers::trim_right(trimmed);
        if (trimmed[0] == L'@') {
            std::string withoutArobase = trimmed;
            withoutArobase.erase(0, 1);
            if (IsValidVariableName(withoutArobase)) {
                functionHandle.name = withoutArobase;
            } else {
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
            }
        }
    }
    return functionHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
