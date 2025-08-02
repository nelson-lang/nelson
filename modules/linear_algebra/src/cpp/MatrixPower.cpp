//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MatrixPower.hpp"
#include "DotPower.hpp"
#include "InverseMatrix.hpp"
#include "IsSymmetric.hpp"
#include "EigenDecomposition.hpp"
#include "MatrixMultiplication.hpp"
#include "RightDivide.hpp"
#include "Eye.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrixScalarPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    std::wstring errorMessage;
    ArrayOf BB = B;
    double value = BB.getContentAsDoubleScalar();
    if (value == rint(value)) {
        if (value == 0) {
            Dimensions dimsA = A.getDimensions();
            return Eye(dimsA.getRows(), dimsA.getColumns(), A.getDataClass());
        }
        if (value == 1) {
            return A;
        }
        ArrayOf res;
        if (value > 0) {
            res = A;
            auto v = (long int)value;
            long int md = v / 2;
            long int mr = v % 2;
            for (long int k = 1; k < md; k++) {
                res = matrixMultiplication(res, A, needToOverload);
            }
            res = matrixMultiplication(res, res, needToOverload);
            if (mr != 0) {
                res = matrixMultiplication(res, A, needToOverload);
            }
        } else {
            ArrayOf invA = A;
            invA = InverseMatrix(invA, needToOverload);
            res = invA;
            auto v = (long int)abs(value);
            long int md = v / 2;
            long int mr = v % 2;
            for (long int k = 1; k < md; k++) {
                res = matrixMultiplication(res, invA, needToOverload);
            }
            res = matrixMultiplication(res, res, needToOverload);
            if (mr != 0) {
                res = matrixMultiplication(res, invA, needToOverload);
            }
        }
        return res;
    }
    ArrayOf V;
    ArrayOf D;
    if (IsSymmetricWithoutSkew(A, needToOverload)) {
        EigenDecompositionFullSymmetric(A, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(A, false, V, D, needToOverload, errorMessage);
    }
    ArrayOf E = D.getDiagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(E, B);
    ArrayOf G = ArrayOf::diagonalConstructor(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    return RightDivide(E, V, needToOverload);
}
//=============================================================================
static ArrayOf
scalarMatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    std::wstring errorMessage;
    ArrayOf V;
    ArrayOf D;
    if (IsSymmetricWithoutSkew(B, needToOverload)) {
        EigenDecompositionFullSymmetric(B, V, D, needToOverload, errorMessage);
    } else {
        EigenDecompositionFullGeneral(B, false, V, D, needToOverload, errorMessage);
    }
    ArrayOf E = D.getDiagonal(0);
    ArrayOf F = DoPowerTwoArgFunction(A, E);
    ArrayOf G = ArrayOf::diagonalConstructor(F, 0);
    E = matrixMultiplication(V, G, needToOverload);
    return RightDivide(E, V, needToOverload);
}
//=============================================================================
ArrayOf
MatrixPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isScalar() && B.isScalar()) {
        return DotPower(A, B, needToOverload);
    }
    // Check for A & B numeric
    if (A.getDataClass() > NLS_CHAR || A.isSparse()) {
        needToOverload = true;
        return {};
    }

    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dims;
        if (A.isEmpty()) {
            dims = A.getDimensions();
        } else {
            dims = B.getDimensions();
        }
        return ArrayOf::emptyConstructor(dims);
    }

    // Test for 2D on both A & B
    if (!A.is2D() || !B.is2D()) {
        Error(_W("Cannot apply exponential operation to N-Dimensional arrays."));
    }

    if (B.isReal() && B.isScalar()) {
        ArrayOf C(B);
        C.promoteType(NLS_DOUBLE);
        double value = C.getContentAsDoubleScalar();
        if (value == rint(value) && (value == -1)) {
            return InverseMatrix(A, needToOverload);
        }
    }
    // Both arguments must be square
    if ((A.getDimensionLength(0) != A.getDimensionLength(1))
        || (B.getDimensionLength(0) != B.getDimensionLength(1))) {
        Error(_W("Power (^) operator can only be applied to scalar and square arguments."));
    }

    bool isSupportedType = (A.isSingleClass() || A.isDoubleClass())
        && (B.isSingleClass() || B.isDoubleClass()) && !A.isSparse() && !B.isSparse();
    if (!isSupportedType) {
        needToOverload = true;
        return {};
    }

    ArrayOf res;
    if (B.isScalar()) {
        res = matrixScalarPower(A, B, needToOverload);
    } else {
        res = scalarMatrixPower(A, B, needToOverload);
    }
    if (res.allReal() && !needToOverload) {
        if (res.getDataClass() == NLS_SCOMPLEX) {
            res.promoteType(NLS_SINGLE);
        }
        if (res.getDataClass() == NLS_DCOMPLEX) {
            res.promoteType(NLS_DOUBLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
