//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
CheckNumeric(const ArrayOf& A, const ArrayOf& B, const std::string& opname)
{
    bool Anumeric, Bnumeric;
    Anumeric = !A.isReferenceType();
    Bnumeric = !B.isReferenceType();
    if (!(Anumeric && Bnumeric)) {
        Error("Nelson:types:conversionNotSupported:cannotApplyNumericOperationValueReference",
            "Cannot apply numeric operation %s to reference types.", opname);
    }
}
//=============================================================================
void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    if (!(A.isReferenceType() && B.isReferenceType())) {
        Error(L"Nelson:types:sameReferenceTypeExpected", L"Same reference type expected.");
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
            "Size mismatch on arguments to arithmetic operator %s", opname);
    }
}
//=============================================================================
bool
MatrixCheck(const ArrayOf& A, const ArrayOf& B, const std::string& opname)
{
    if (A.isScalar() || B.isScalar()) {
        return false;
    }
    CheckNumeric(A, B, opname);
    if (!A.is2D() || !B.is2D()) {
        Error("Nelson:types:conversionNotSupported:cannotApplyMatrixOperationValueN",
            "Cannot apply matrix operation %s to N-Dimensional arrays.", opname);
    }
    return true;
}
//=============================================================================
bool
SameSizeCheck(Dimensions& Adim, Dimensions& Bdim)
{
    Adim.simplify();
    Bdim.simplify();
    return (Adim.equals(Bdim));
}
//=============================================================================
void
VectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    CheckNumeric(A, B, opname);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error("Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue",
            "Size mismatch on arguments to arithmetic operator %s", opname);
    }
}
//=============================================================================
void
PromoteToLogicalVectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    A.promoteType(NLS_LOGICAL);
    B.promoteType(NLS_LOGICAL);

    bool haveScalar = A.isScalar() || B.isScalar();

    if (A.isVector() && B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) && !haveScalar) {
            if (A.elementCount() != B.elementCount()) {
                Error("Nelson:dimagree:sizeMismatchArgumentsValue",
                    "Size mismatch on arguments to %s", opname);
            }
        }
    } else {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || haveScalar)) {
            Error("Nelson:dimagree:sizeMismatchArgumentsValue", "Size mismatch on arguments to %s",
                opname);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
