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
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "VertCatSparseLogical.hpp"
#include "HorzCatSparseLogical.hpp"
#include "CtransposeSparseLogical.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_LOGICAL_EXPECTED);
    }
    if (!B.isSparseLogicalType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_LOGICAL_EXPECTED);
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    A = CtransposeSparseLogical(A);
    B = CtransposeSparseLogical(B);
    C = HorzCatSparseLogical(A, B);
    C = CtransposeSparseLogical(C);
    return C;
}
//=============================================================================
}
//=============================================================================
