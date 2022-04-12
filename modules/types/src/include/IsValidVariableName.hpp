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
#include <string>
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
NLSTYPES_IMPEXP bool
IsValidVariableName(const std::string& varname, bool withUnderscore = false);
NLSTYPES_IMPEXP bool
IsValidVariableName(const std::wstring& varname, bool withUnderscore = false);
} // namespace Nelson
//=============================================================================
