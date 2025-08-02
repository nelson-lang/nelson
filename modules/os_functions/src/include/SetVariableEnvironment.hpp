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
#include "nlsOs_functions_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSOS_FUNCTIONS_IMPEXP bool
SetVariableEnvironmentW(const std::wstring& envVarName, const std::wstring& Value = L"");
NLSOS_FUNCTIONS_IMPEXP bool
SetVariableEnvironmentU(const std::string& envVarName, const std::string& Value = "");
} // namespace Nelson
//=============================================================================
