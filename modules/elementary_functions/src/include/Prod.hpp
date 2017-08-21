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
#include "nlsElementary_functions_exports.h"
#include "ArrayOf.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    /* prod */
    /* if d == 0 --> Prod(X) */
    /* if d != 0 --> Prod(X, n) */
    NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOf Prod(ArrayOf A, indexType d, const std::wstring &strtype, bool withnan);
}
//=============================================================================
