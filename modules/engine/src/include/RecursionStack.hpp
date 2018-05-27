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
#pragma once
//=============================================================================
#include <cstdlib>
#include "nlsEngine_exports.h"
//=============================================================================
namespace Nelson {
#define SIZE_MAX_RECURSION_CALL 60 * 1024 * 1024
// SIZE_MAX_RECURSION_CALL OS dependant current value works on linux, mac, windows
// On Windows, you need to set /STACK:reserce
#define MAX_RECURSION_FUNCTION_CALL 1936
// ideally, it should be 5000
#define DEFAULT_RECURSION_FUNCTION_CALL 500
NLSENGINE_IMPEXP size_t
setRecursionStacksize(size_t sizemb);
NLSENGINE_IMPEXP size_t
getRecursionStacksize();
} // namespace Nelson
//=============================================================================
