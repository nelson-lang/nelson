//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
// static bool
// promoteCommonType(ArrayOf& A, ArrayOf& B)
//{
//    bool wasPromoted = true;
//    if (A.getDataClass() != B.getDataClass()) {
//        if (A.isComplex() || B.isComplex()) {
//            if (A.isComplex()) {
//                if (B.getDataClass() == NLS_DOUBLE) {
//                    B.promoteType(NLS_DCOMPLEX);
//                } else if (B.getDataClass() == NLS_SINGLE) {
//                    B.promoteType(NLS_SCOMPLEX);
//                } else {
//                    wasPromoted = false;
//                }
//            } else {
//                if (A.getDataClass() == NLS_DOUBLE) {
//                    A.promoteType(NLS_DCOMPLEX);
//                } else if (A.getDataClass() == NLS_SINGLE) {
//                    A.promoteType(NLS_SCOMPLEX);
//                } else {
//                    wasPromoted = false;
//                }
//            }
//        } else {
//            if (A.getDataClass() == NLS_SINGLE || B.getDataClass() == NLS_SINGLE) {
//                A.promoteType(NLS_SINGLE);
//                B.promoteType(NLS_SINGLE);
//            } else {
//                wasPromoted = false;
//            }
//        }
//    }
//    return wasPromoted;
//}
////=============================================================================
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

    if (A.getDataClass() != B.getDataClass()) {
        bool isComplex = A.isComplex() || B.isComplex();
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return LeftDivide(AA, BB, needToOverload);
        }
        if (A.isDoubleType() && B.isDoubleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_DCOMPLEX);
            BB.promoteType(NLS_DCOMPLEX);
            return LeftDivide(AA, BB, needToOverload);
        }
        if (A.isSingleType() && B.isSingleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_SCOMPLEX);
            BB.promoteType(NLS_SCOMPLEX);
            return LeftDivide(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (B.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf AA = A;
            AA.promoteType(B.getDataClass());
            ArrayOf res = LeftDivide(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.getDataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (A.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf BB = B;
            BB.promoteType(A.getDataClass());
            ArrayOf res = LeftDivide(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.getDataClass());
            }
            return res;
        }
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
