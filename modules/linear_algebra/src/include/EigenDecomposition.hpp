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
#include <string>
#include "ArrayOf.hpp"
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
NLSLINEAR_ALGEBRA_IMPEXP bool
EigenDecompositionFullSymmetric(
    const ArrayOf& A, ArrayOf& V, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
EigenDecompositionFullGeneral(const ArrayOf& A, bool balance, ArrayOf& V, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
EigenDecompositionCompactSymmetric(
    const ArrayOf& A, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
EigenDecompositionCompactGeneral(
    const ArrayOf& A, bool balance, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionCompactSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionFullSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& V,
    ArrayOf& D, bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionFullGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& V, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP
bool
GeneralizedEigenDecompositionCompactGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage);
//=============================================================================
}
//=============================================================================
