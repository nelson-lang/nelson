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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSINTERPRETER_IMPEXP std::wstring
smartIndentCode(const std::wstring& source, int tabSize = 2, const std::wstring& sourcePath = L"",
    bool fullFormat = true);
//=============================================================================
NLSINTERPRETER_IMPEXP void
smartIndentFile(
    const std::wstring& filename, int tabSize = 2, bool doBackup = false, bool fullFormat = true);
//=============================================================================
} // namespace Nelson
//=============================================================================
