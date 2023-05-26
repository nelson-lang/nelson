//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MatrixCheck.hpp"
#include "Error.hpp"
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
        Error(std::string(_("Cannot apply numeric operation ")) + opname
            + std::string(_(" to reference types.")));
    }
}
//=============================================================================
void
VectorCheckReference(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    if (!(A.isReferenceType() && B.isReferenceType())) {
        Error(_W("Same reference type expected."));
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + opname);
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
        Error(std::string(_("Cannot apply matrix operation ")) + opname
            + std::string(_(" to N-Dimensional arrays.")));
    }
    return true;
}
//=============================================================================
NelsonType
FindCommonType(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType Aclass = A.getDataClass();
    NelsonType Bclass = B.getDataClass();
    if ((Aclass == Bclass) && (Aclass <= NLS_CHAR)) {
        return Aclass;
    }
    // An integer or double mixed with a complex is promoted to a dcomplex type
    if ((Aclass == NLS_SCOMPLEX) && ((Bclass == NLS_DOUBLE) || (Bclass < NLS_SINGLE))) {
        Bclass = NLS_DCOMPLEX;
    }
    if ((Bclass == NLS_SCOMPLEX) && ((Aclass == NLS_DOUBLE) || (Aclass < NLS_SINGLE))) {
        Aclass = NLS_DCOMPLEX;
    }
    // The output class is now the dominant class remaining:
    bool isObjectA = A.isClassStruct() || (Aclass == NLS_HANDLE);
    bool isObjectB = B.isClassStruct() || (Bclass == NLS_HANDLE);

    if (isObjectA) {
        return Aclass;
    }
    if (isObjectB) {
        return Bclass;
    }

    if (A.isIntegerType() && B.isIntegerType()) {
        return (Aclass > Bclass) ? Aclass : Bclass;
    }

    bool isDoubleTypeA = Aclass == NLS_DOUBLE || Aclass == NLS_DCOMPLEX;
    bool isDoubleTypeB = Bclass == NLS_DOUBLE || Bclass == NLS_DCOMPLEX;

    if (isDoubleTypeA) {
        return Aclass;
    }
    if (isDoubleTypeB) {
        return Bclass;
    }

    bool isSingleTypeA = Aclass == NLS_SINGLE || Aclass == NLS_SCOMPLEX;
    bool isSingleTypeB = Bclass == NLS_SINGLE || Bclass == NLS_SCOMPLEX;

    if (isSingleTypeA) {
        return Aclass;
    }
    if (isSingleTypeB) {
        return Bclass;
    }

    return (Aclass > Bclass) ? Aclass : Bclass;
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
        Error(std::string(_("Size mismatch on arguments to arithmetic operator")) + " " + opname);
    }
}
//=============================================================================
void
BoolVectorCheck(ArrayOf& A, ArrayOf& B, const std::string& opname)
{
    A.promoteType(NLS_LOGICAL);
    B.promoteType(NLS_LOGICAL);

    bool haveScalar = A.isScalar() || B.isScalar();

    if (A.isVector() && B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) && !haveScalar) {
            if (A.getElementCount() != B.getElementCount()) {
                Error(std::string(_("Size mismatch on arguments to ")) + opname);
            }
        }
    } else {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || haveScalar)) {
            Error(std::string(_("Size mismatch on arguments to ")) + opname);
        }
    }
}
//=============================================================================

} // namespace Nelson
//=============================================================================
