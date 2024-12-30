//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MacroCompleter.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
MacroCompleter(const std::wstring& prefix)
{
    wstringVector res;
    wstringVector macros = PathFunctionIndexerManager::getInstance()->getMacrosList();
    for (auto& macro : macros) {
        if (StringHelpers::starts_with(macro, prefix)) {
            res.push_back(macro);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
