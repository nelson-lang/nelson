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
#include "ArrayOf.hpp"
#include "nlsOs_functions_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSOS_FUNCTIONS_IMPEXP std::wstring
GetVariableEnvironment(const std::wstring& envVarName, const std::wstring& defaultValue = L"");
}
//=============================================================================
