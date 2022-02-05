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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Validate number of input arguments
 */
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
}
//=============================================================================
/**
 * Validate number of input arguments
 */
inline void
nargincheck(const ArrayOfVector& argIn, int minArgs, int maxArgs)
{
    if (argIn.size() < (size_t)minArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() > (size_t)maxArgs) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
}
//=============================================================================
/**
 * Validate number of output arguments
 */
inline void
nargoutcheck(int nLhs, int minArgs)
{
    if (nLhs < minArgs) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
}
//=============================================================================
/**
 * Validate number of output arguments
 */
inline void
nargoutcheck(int nLhs, int minArgs, int maxArgs)
{
    if (nLhs < minArgs) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (nLhs > maxArgs) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
}
//=============================================================================
}
//=============================================================================
