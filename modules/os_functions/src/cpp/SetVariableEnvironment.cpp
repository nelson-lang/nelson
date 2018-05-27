//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <Windows.h>
#endif
#include "SetVariableEnvironment.hpp"
#include "characters_encoding.hpp"
#include <stdlib.h>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP bool
SetVariableEnvironmentW(std::wstring envVarName, std::wstring Value)
{
#ifdef _MSC_VER
    if (::SetEnvironmentVariableW(envVarName.c_str(), Value.c_str())) {
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
SetVariableEnvironmentU(std::string envVarName, std::string Value)
{
    bool bRes = false;
#ifdef _MSC_VER
    if (::SetEnvironmentVariableA(envVarName.c_str(), Value.c_str())) {
        bRes = true;
    } else {
        bRes = false;
    }
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
}
//=============================================================================
