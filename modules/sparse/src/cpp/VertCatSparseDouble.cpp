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
#include "VertCatSparseDouble.hpp"
#include "CtransposeSparseDouble.hpp"
#include "HorzCatSparseDouble.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatSparseDouble(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseDoubleType()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_DOUBLE_EXPECTED);
    }
    if (!B.isSparseDoubleType()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_DOUBLE_EXPECTED);
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getColumns() != dimsB.getColumns()) {
        Error(ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    A = CtransposeSparseDouble(A);
    B = CtransposeSparseDouble(B);
    C = HorzCatSparseDouble(A, B);
    C = CtransposeSparseDouble(C);
    return C;
}
//=============================================================================
}
//=============================================================================
