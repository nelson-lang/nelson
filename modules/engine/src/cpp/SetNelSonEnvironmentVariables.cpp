//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SetNelSonEnvironmentVariables.hpp"
#include "AddPathToEnvironmentVariable.hpp"
#include "GetNelsonBinariesPath.hpp"
#include "GetNelsonPath.hpp"
#include "SetVariableEnvironment.hpp"
#include <vector>
#include <cstdio>
#include <cstdlib>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
SetNelSonEnvironmentVariables()
{
    std::wstring binPath = GetNelsonBinariesPath();
    if (!binPath.empty()) {
        AddPathToEnvironmentVariable(std::wstring(L"PATH"), binPath);
        std::wstring envVarName = std::wstring(L"NELSON_BINARY_PATH");
        SetVariableEnvironmentW(envVarName, binPath);
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
