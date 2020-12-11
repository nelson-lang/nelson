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
#include "eigBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "EigenDecomposition.hpp"
#include "GeneralizedEigenDecomposition.hpp"
#include "IsHermitian.hpp"
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
eigFull(Evaluator* eval, int nLhs, bool balance, bool generalizedDecomposition,
    const ArrayOfVector& argIn, bool& needToOverload, bool& bSuccess, std::wstring& errorMessage);
//=============================================================================
static ArrayOfVector
eigCompact(Evaluator* eval, int nLhs, bool balance, bool generalizedDecomposition,
    const ArrayOfVector& argIn, bool& needToOverload, bool& bSuccess, std::wstring& errorMessage);
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::eigBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool balance = false;
    bool needToOverload = false;
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "eig", bSuccess);
        if (bSuccess) {
            return retval;
        }
    }
    bool compactDecomposition = (nLhs < 2);
    bool generalizedDecomposition = false;

    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
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
    std::wstring errorMessage;
    if (compactDecomposition) {
        retval = eigCompact(eval, nLhs, balance, generalizedDecomposition, argIn, needToOverload,
            bSuccess, errorMessage);
    } else {
        retval = eigFull(eval, nLhs, balance, generalizedDecomposition, argIn, needToOverload,
            bSuccess, errorMessage);
    }
    if (needToOverload) {
        retval = OverloadFunction(eval, nLhs, argIn, "eig");
    } else {
        if (!bSuccess) {
            Error(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
eigCompact(Evaluator* eval, int nLhs, bool balance, bool generalizedDecomposition,
    const ArrayOfVector& argIn, bool& needToOverload, bool& bSuccess, std::wstring& errorMessage)
{
    ArrayOfVector retval;
    ArrayOf D;
    if (generalizedDecomposition) {
        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        bool isHermitianA = IsHermitian(A, (bool)false, needToOverload);
        bool isHermitianB = IsHermitian(B, (bool)false, needToOverload);
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
        bool isHermitianA = IsHermitian(A, (bool)false, needToOverload);
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
        retval.push_back(D);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
eigFull(Evaluator* eval, int nLhs, bool balance, bool generalizedDecomposition,
    const ArrayOfVector& argIn, bool& needToOverload, bool& bSuccess, std::wstring& errorMessage)
{
    ArrayOfVector retval;
    ArrayOf V;
    ArrayOf D;
    if (generalizedDecomposition) {
        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        bool isHermitianA = IsHermitian(A, (bool)false, needToOverload);
        bool isHermitianB = IsHermitian(B, (bool)false, needToOverload);
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
        bool isHermitianA = IsHermitian(A, (bool)false, needToOverload);
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
        retval.push_back(V);
        if (D.allReal()) {
            if (D.getDataClass() == NLS_DCOMPLEX) {
                D.promoteType(NLS_DOUBLE);
            }
            if (D.getDataClass() == NLS_SCOMPLEX) {
                D.promoteType(NLS_SINGLE);
            }
        }
        retval.push_back(D);
    }
    return retval;
}
//=============================================================================
