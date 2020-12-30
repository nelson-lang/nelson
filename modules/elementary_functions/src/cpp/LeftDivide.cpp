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
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "DotLeftDivide.hpp"
#include "LinearEquationSolver.hpp"
#include "LeastSquareSolver.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
promoteCommonType(ArrayOf& A, ArrayOf& B)
{
    bool wasPromoted = true;
    bool isComplex = A.isComplex() || B.isComplex();
    bool isSingle = A.isSingleClass() || B.isSingleClass();
    if ((A.isSingleClass() || A.isDoubleClass()) && (B.isSingleClass() || B.isDoubleClass())) {
        if (isComplex) {
            if (isSingle) {
                A.promoteType(NLS_SCOMPLEX);
                B.promoteType(NLS_SCOMPLEX);
            } else {
                A.promoteType(NLS_DCOMPLEX);
                B.promoteType(NLS_DCOMPLEX);
            }
        } else {
            if (isSingle) {
                A.promoteType(NLS_SINGLE);
                B.promoteType(NLS_SINGLE);

            } else {
                A.promoteType(NLS_DOUBLE);
                B.promoteType(NLS_DOUBLE);
            }
        }
    } else {
        wasPromoted = false;
    }
    return wasPromoted;
}
//=============================================================================
ArrayOf
LeftDivide(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dims(0, 1);
        return ArrayOf::emptyConstructor(dims);
    }

    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }

    if (A.isScalar()) {
        return DotLeftDivide(A, B, needToOverload);
    }

    if (A.getDimensionLength(0) != B.getDimensionLength(0)) {
        Error(_W("Requested divide operation requires arguments to have correct dimensions."));
    }
    std::wstring warningId;
    std::string warningMessage;
    ArrayOf res;

    if (!promoteCommonType(A, B)) {
        needToOverload = true;
        return res;
    }

    bool isSquare = A.getDimensionLength(0) == A.getDimensionLength(1);

    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        if (isSquare) {
            res = solveLinearEquationDouble(A, B, warningId, warningMessage);
            if (warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
                std::string firstMessage = warningMessage;
                res = solveLeastSquareDouble(A, B, warningId, warningMessage);
                warningMessage = firstMessage;
            }
        } else {
            res = solveLeastSquareDouble(A, B, warningId, warningMessage);
        }
    } break;
    case NLS_SINGLE: {
        if (isSquare) {
            res = solveLinearEquationSingle(A, B, warningId, warningMessage);
            if (warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
                std::string firstMessage = warningMessage;
                res = solveLeastSquareSingle(A, B, warningId, warningMessage);
                warningMessage = firstMessage;
            }
        } else {
            res = solveLeastSquareSingle(A, B, warningId, warningMessage);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationDoubleComplex(A, B, warningId, warningMessage);
            if (warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
                std::string firstMessage = warningMessage;
                res = solveLeastSquareDoubleComplex(A, B, warningId, warningMessage);
                warningMessage = firstMessage;
            }
        } else {
            res = solveLeastSquareDoubleComplex(A, B, warningId, warningMessage);
        }
    } break;
    case NLS_SCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationSingleComplex(A, B, warningId, warningMessage);
            if (warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
                std::string firstMessage = warningMessage;
                res = solveLeastSquareSingleComplex(A, B, warningId, warningMessage);
                warningMessage = firstMessage;
            }
        } else {
            res = solveLeastSquareSingleComplex(A, B, warningId, warningMessage);
        }
    } break;

    default: {
        needToOverload = true;
    } break;
    }
    if (!warningMessage.empty()) {
        Warning(warningMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
