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
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
        raiseError(L"Nelson:types:ERROR_CANNOT_APPLY_NUMERIC_OPERATION_TO_REFERENCE_TYPES",
            ERROR_CANNOT_APPLY_NUMERIC_OPERATION_TO_REFERENCE_TYPES, utf8_to_wstring(opname));
    }
}
//=============================================================================
void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    if (!(A.isReferenceType() && B.isReferenceType())) {
        raiseError(
            L"Nelson:types:ERROR_SAME_REFERENCE_TYPE_EXPECTED", ERROR_SAME_REFERENCE_TYPE_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        raiseError(L"Nelson:types:ERROR_SIZE_MISMATCH_ARITHMETIC_OPERATOR",
            ERROR_SIZE_MISMATCH_ARITHMETIC_OPERATOR, utf8_to_wstring(opname));
    }
}
//=============================================================================
bool
MatrixCheck(const ArrayOf& A, const ArrayOf& B, const std::string& opname)
{
    // Test for either a scalar (test 1)
    if (A.isScalar() || B.isScalar()) {
        return false;
    }
    // Test for A & B numeric
    CheckNumeric(A, B, opname);
    // Test for 2D
    if (!A.is2D() || !B.is2D()) {
        raiseError(L"Nelson:types:ERROR_CANNOT_APPLY_MATRIX_OPERATION_TO_N_DIMENSIONAL_ARRAYS",
            ERROR_CANNOT_APPLY_MATRIX_OPERATION_TO_N_DIMENSIONAL_ARRAYS, utf8_to_wstring(opname));
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
    // Check for numeric types
    CheckNumeric(A, B, opname);
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        raiseError(L"Nelson:types:ERROR_SIZE_MISMATCH_ARITHMETIC_OPERATOR",
            ERROR_SIZE_MISMATCH_ARITHMETIC_OPERATOR, utf8_to_wstring(opname));
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
            if (A.getElementCount() != B.getElementCount()) {
                raiseError(L"Nelson:types:ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO",
                    ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO, utf8_to_wstring(opname));
            }
        }
    } else {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || haveScalar)) {
            raiseError(L"Nelson:types:ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO",
                ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO, utf8_to_wstring(opname));
        }
    }
}
//=============================================================================

} // namespace Nelson
//=============================================================================
