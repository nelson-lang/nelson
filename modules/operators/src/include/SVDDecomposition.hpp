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
#include "nlsOperators_exports.h"
#include "Types.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionDouble(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionDoubleComplex(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionSingle(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
NLSOPERATORS_IMPEXP
ArrayOf
solveSVDDecompositionSingleComplex(const ArrayOf& matA, const ArrayOf& matB);
//=============================================================================
}
//=============================================================================
