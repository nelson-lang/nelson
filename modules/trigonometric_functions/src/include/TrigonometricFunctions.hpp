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
#include "nlsTrigonometric_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Cos(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Sin(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Tan(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Acos(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Asin(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Atan(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Cosh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Sinh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
NLSTRIGONOMETRIC_FUNCTIONS_IMPEXP ArrayOf
Tanh(const ArrayOf& A, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================