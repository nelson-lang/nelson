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
#include "diagBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::diagBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1 || argIn.size() > 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "diag", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "diag", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        ArrayOf b;
        int64* dp;
        int64 diagonalOrder;
        if (argIn.size() == 1) {
            diagonalOrder = 0;
        } else {
            b = argIn[1];
            if (!b.isScalar()) {
                Error(_W("Second argument must be a scalar."));
            }
            b.promoteType(NLS_INT64);
            dp = (int64*)b.getDataPointer();
            diagonalOrder = dp[0];
        }

        ArrayOf a = argIn[0];
        if (!a.is2D()) {
            Error(_W("First argument to 'diag' function must be 2D."));
        }
        ArrayOf diag;
        if ((a.getDimensionLength(1) == 1) || (a.getDimensionLength(0) == 1)) {
            diag = ArrayOf::diagonalConstructor(a, diagonalOrder);
        } else {
            diag = a.getDiagonal(diagonalOrder);
        }
        retval.push_back(diag);
    }
    return retval;
}
//=============================================================================
