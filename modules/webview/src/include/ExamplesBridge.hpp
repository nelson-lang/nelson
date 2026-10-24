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
#include "nlsWebview_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSWEBVIEW_IMPEXP bool
BuildExamplesCommand(const std::string& method, const std::wstring& path,
    const std::vector<std::wstring>& allowedPaths, std::wstring& command, std::string& error);
//=============================================================================
NLSWEBVIEW_IMPEXP bool
BuildExamplesCommandFromCatalogCommand(const std::string& method,
    const std::wstring& requestedCommand, const std::vector<std::wstring>& allowedCommands,
    std::wstring& command, std::string& error);
//=============================================================================
} // namespace Nelson
//=============================================================================
