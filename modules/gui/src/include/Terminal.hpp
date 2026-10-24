//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "Types.hpp"
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson::Terminal {
//=============================================================================
struct TerminalOptions
{
    std::wstring name;
    std::wstring shell;
    std::wstring windowStyle;
    std::wstring theme;
    std::wstring startupCommand;
};
//=============================================================================
NLSGUI_IMPEXP ArrayOf
create(const TerminalOptions& options);
//=============================================================================
NLSGUI_IMPEXP void
write(const ArrayOf& handle, const std::wstring& text);
//=============================================================================
NLSGUI_IMPEXP std::wstring
read(const ArrayOf& handle);
//=============================================================================
NLSGUI_IMPEXP void
close(Evaluator* eval, const ArrayOf& handle);
//=============================================================================
NLSGUI_IMPEXP void
focus(const ArrayOf& handle);
//=============================================================================
NLSGUI_IMPEXP void
clear(const ArrayOf& handle);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
get(const ArrayOf& handle, const std::wstring& propertyName);
//=============================================================================
NLSGUI_IMPEXP void
set(const ArrayOf& handle, const std::wstring& propertyName, const ArrayOf& value);
//=============================================================================
NLSGUI_IMPEXP ArrayOf
list();
//=============================================================================
NLSGUI_IMPEXP void
closeAll(Evaluator* eval);
//=============================================================================
NLSGUI_IMPEXP wstringVector
themes();
//=============================================================================
NLSGUI_IMPEXP std::wstring
version();
//=============================================================================
} // namespace Nelson::Terminal
//=============================================================================
