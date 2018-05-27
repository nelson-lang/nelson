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
#include "ArrayOf.hpp"
#include "nlsRandom_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
typedef enum
{
    RNG_ERROR = -1,
    RNG_TWISTER = 0,
    RNG_TWISTER64,
    RNG_LAGGED_FIBONACCI_607,
} RNG_TYPE;

NLSRANDOM_IMPEXP wstringVector
getSupportedRngEngineName();

NLSRANDOM_IMPEXP std::wstring
getRngTypeAsString(RNG_TYPE rngType);
NLSRANDOM_IMPEXP RNG_TYPE
getRngType(const std::wstring& enginename);
NLSRANDOM_IMPEXP bool
isRngType(const std::wstring& enginename);

} // namespace Nelson
//=============================================================================
