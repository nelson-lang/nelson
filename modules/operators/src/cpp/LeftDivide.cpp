//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "DotLeftDivide.hpp"
#include "LinearEquationSolver.hpp"
#include "LeastSquareSolver.hpp"
#include "SVDDecomposition.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
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
        return {};
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

    bool isSquare = A.getDimensionLength(0) == A.getDimensionLength(1);

    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        if (isSquare) {
            res = solveLinearEquationDouble(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareDouble(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionDouble(A, B);
        }
    } break;
    case NLS_SINGLE: {
        if (isSquare) {
            res = solveLinearEquationSingle(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareSingle(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionSingle(A, B);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationDoubleComplex(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareDoubleComplex(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionDoubleComplex(A, B);
        }
    } break;
    case NLS_SCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationSingleComplex(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareSingleComplex(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionDoubleComplex(A, B);
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
