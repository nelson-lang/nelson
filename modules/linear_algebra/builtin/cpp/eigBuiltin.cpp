//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "eigBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "EigenDecomposition.hpp"
#include "GeneralizedEigenDecomposition.hpp"
#include "IsHermitian.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// e = eig(A)
// [V, D ] = eig(A)
// e = eig(A, B)
// [V, D ] = eig(A, B)
// e = eig(A, balanceOption)
// e = eig(A, B)
// [V, D ] = eig(A, balanceOption)
// [V, D ] = eig(A, B)
//=============================================================================
static ArrayOfVector
eigFull(int nLhs, bool balance, bool generalizedDecomposition, const ArrayOfVector& argIn,
    bool& needToOverload, bool& bSuccess, std::wstring& errorMessage);
//=============================================================================
static ArrayOfVector
eigCompact(int nLhs, bool balance, bool generalizedDecomposition, const ArrayOfVector& argIn,
    bool& needToOverload, bool& bSuccess, std::wstring& errorMessage);
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::eigBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool balance = false;
    bool needToOverload = false;
    nargoutcheck(nLhs, 0, 2);
    bool compactDecomposition = (nLhs < 2);
    bool generalizedDecomposition = false;

    nargincheck(argIn, 1);
    if (argIn.size() == 1) {
        balance = true;
    } else {
        ArrayOf B(argIn[1]);
        if (B.isCharacterArray() || (B.isStringArray() && B.isScalar())) {
            std::wstring balanceOption = argIn[1].getContentAsWideString();
            if (balanceOption == L"balance") {
                balance = true;
            } else if (balanceOption == L"nobalance") {
                balance = false;
            } else {
                Error(_W("option #2 must be 'balance', or 'nobalance'."));
            }
        } else {
            generalizedDecomposition = true;
        }
    }
    bool bSuccess;
    std::wstring errorMessage;
    if (compactDecomposition) {
        retval = eigCompact(
            nLhs, balance, generalizedDecomposition, argIn, needToOverload, bSuccess, errorMessage);
    } else {
        retval = eigFull(
            nLhs, balance, generalizedDecomposition, argIn, needToOverload, bSuccess, errorMessage);
    }
    if (needToOverload) {
        OverloadRequired("eig");
    } else {
        if (!bSuccess) {
            Error(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
eigCompact(int nLhs, bool balance, bool generalizedDecomposition, const ArrayOfVector& argIn,
    bool& needToOverload, bool& bSuccess, std::wstring& errorMessage)
{
    ArrayOfVector retval;
    ArrayOf D;
    if (generalizedDecomposition) {
        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        bool isHermitianA = IsHermitianWithoutSkew(A, needToOverload);
        bool isHermitianB = IsHermitianWithoutSkew(B, needToOverload);
        if (isHermitianA && isHermitianB) {
            bSuccess = GeneralizedEigenDecompositionCompactSymmetric(
                A, B, D, needToOverload, errorMessage);
            if (!bSuccess && errorMessage.empty()) {
                bSuccess = GeneralizedEigenDecompositionCompactGeneral(
                    A, B, D, needToOverload, errorMessage);
            }
        } else {
            bSuccess = GeneralizedEigenDecompositionCompactGeneral(
                A, B, D, needToOverload, errorMessage);
        }
    } else {
        ArrayOf A(argIn[0]);
        bool isHermitianA = IsHermitianWithoutSkew(A, needToOverload);
        if (isHermitianA) {
            bSuccess = EigenDecompositionCompactSymmetric(A, D, needToOverload, errorMessage);
            if (!bSuccess && errorMessage.empty()) {
                bSuccess
                    = EigenDecompositionCompactGeneral(A, balance, D, needToOverload, errorMessage);
            }
        } else {
            bSuccess
                = EigenDecompositionCompactGeneral(A, balance, D, needToOverload, errorMessage);
        }
    }
    if (bSuccess) {
        if (D.allReal()) {
            if (D.getDataClass() == NLS_DCOMPLEX) {
                D.promoteType(NLS_DOUBLE);
            }
            if (D.getDataClass() == NLS_SCOMPLEX) {
                D.promoteType(NLS_SINGLE);
            }
        }
        retval << D;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
eigFull(int nLhs, bool balance, bool generalizedDecomposition, const ArrayOfVector& argIn,
    bool& needToOverload, bool& bSuccess, std::wstring& errorMessage)
{
    ArrayOfVector retval;
    ArrayOf V;
    ArrayOf D;
    if (generalizedDecomposition) {
        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        bool isHermitianA = IsHermitianWithoutSkew(A, needToOverload);
        bool isHermitianB = IsHermitianWithoutSkew(B, needToOverload);
        if (isHermitianA && isHermitianB) {
            bSuccess = GeneralizedEigenDecompositionFullSymmetric(
                A, B, V, D, needToOverload, errorMessage);
            if (!bSuccess && errorMessage.empty()) {
                bSuccess = GeneralizedEigenDecompositionFullGeneral(
                    A, B, V, D, needToOverload, errorMessage);
            }
        } else {
            bSuccess = GeneralizedEigenDecompositionFullGeneral(
                A, B, V, D, needToOverload, errorMessage);
        }
    } else {
        ArrayOf A(argIn[0]);
        bool isHermitianA = IsHermitianWithoutSkew(A, needToOverload);
        if (isHermitianA) {
            bSuccess = EigenDecompositionFullSymmetric(A, V, D, needToOverload, errorMessage);
        } else {
            bSuccess
                = EigenDecompositionFullGeneral(A, balance, V, D, needToOverload, errorMessage);
        }
    }
    if (bSuccess) {
        if (V.allReal()) {
            if (V.getDataClass() == NLS_DCOMPLEX) {
                V.promoteType(NLS_DOUBLE);
            }
            if (V.getDataClass() == NLS_SCOMPLEX) {
                V.promoteType(NLS_SINGLE);
            }
        }
        retval << V;
        if (D.allReal()) {
            if (D.getDataClass() == NLS_DCOMPLEX) {
                D.promoteType(NLS_DOUBLE);
            }
            if (D.getDataClass() == NLS_SCOMPLEX) {
                D.promoteType(NLS_SINGLE);
            }
        }
        retval << D;
    }
    return retval;
}
//=============================================================================
