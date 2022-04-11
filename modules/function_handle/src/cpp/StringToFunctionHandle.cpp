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
//=============================================================================
namespace Nelson {
//=============================================================================
function_handle
StringToFunctionHandle(const std::wstring& functionName)
{
    function_handle functionHandle;
    if (IsValidVariableName(functionName)) {
        functionHandle.name = wstring_to_utf8(functionName);
        functionHandle.anonymous.clear();
    } else {
        functionHandle.name.clear();
        functionHandle.anonymous.clear();
    }
    return functionHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
