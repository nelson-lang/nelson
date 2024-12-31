//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsHistory_manager_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson::History {
//=============================================================================
NLSHISTORY_MANAGER_IMPEXP Nelson::wstringVector
get();
NLSHISTORY_MANAGER_IMPEXP std::string
getFilename();
NLSHISTORY_MANAGER_IMPEXP bool
addLine(const std::wstring& line);
NLSHISTORY_MANAGER_IMPEXP bool
setToken(const std::wstring& line);
NLSHISTORY_MANAGER_IMPEXP std::wstring
getNextLine();
NLSHISTORY_MANAGER_IMPEXP std::wstring
getPreviousLine();
//=============================================================================
} // namespace Nelson
//=============================================================================
