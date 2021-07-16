//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
function_handle
StringToFunctionHandle(const std::wstring& functionName)
{
    function_handle functionHandle;
    if (IsValidVariableName(functionName)) {
        functionHandle.name = wstring_to_utf8(functionName);
        functionHandle.anonymous.clear();
    } else {
        functionHandle.name.clear();
        functionHandle.anonymous.clear();
    }
    return functionHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
