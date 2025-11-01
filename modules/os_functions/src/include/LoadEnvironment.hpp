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
#include <vector>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP
std::vector<std::pair<std::wstring, std::wstring>>
LoadEnvironment(const std::wstring& filename, bool applyToEnv, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
