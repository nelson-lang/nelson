//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "nlsInteger_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
    NLSINTEGER_IMPEXP ArrayOf ToInt8(ArrayOf a);
    NLSINTEGER_IMPEXP int8 ToInt8(int8 a);
    NLSINTEGER_IMPEXP int8 ToInt8(float a);
    NLSINTEGER_IMPEXP int8 ToInt8(double a);
    NLSINTEGER_IMPEXP int8 ToInt8(uint8 a);
    NLSINTEGER_IMPEXP int8 ToInt8(int16 a);
    NLSINTEGER_IMPEXP int8 ToInt8(uint16 a);
    NLSINTEGER_IMPEXP int8 ToInt8(int32 a);
    NLSINTEGER_IMPEXP int8 ToInt8(uint32 a);
    NLSINTEGER_IMPEXP int8 ToInt8(int64 a);
    NLSINTEGER_IMPEXP int8 ToInt8(uint64 a);

}
//=============================================================================
