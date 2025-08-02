//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <cstdlib>
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP bool
SetVariableEnvironmentW(const std::wstring& envVarName, const std::wstring& Value)
{
#ifdef _MSC_VER
    if (::SetEnvironmentVariableW(envVarName.c_str(), Value.c_str()) != 0) {
        return true;
    }
#else
    return SetVariableEnvironmentU(
        wstring_to_utf8(envVarName).c_str(), wstring_to_utf8(Value).c_str());
#endif
    return false;
}
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP bool
SetVariableEnvironmentU(const std::string& envVarName, const std::string& Value)
{
    bool bRes = false;
#ifdef _MSC_VER
    bRes = ::SetEnvironmentVariableA(envVarName.c_str(), Value.c_str()) != 0;
#else
    if (setenv(envVarName.c_str(), Value.c_str(), 1)) {
        bRes = false;
    } else {
        bRes = true;
    }
#endif
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
