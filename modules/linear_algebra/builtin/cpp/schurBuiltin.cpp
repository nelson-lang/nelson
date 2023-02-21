//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "schurBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "SchurDecompostion.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::schurBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "schur", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
            || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
            retval = OverloadFunction(eval, nLhs, argIn, "schur", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION);
        }
        bool asComplex = false;
        if (argIn.size() == 2) {
            ArrayOf param2 = argIn[1];
            std::wstring str = param2.getContentAsWideString();
            if (str == L"complex" || str == L"real") {
                if (str == L"complex") {
                    asComplex = true;
                }
            } else {
                Error(_W("Second input argument must be 'real' or 'complex'."));
            }
        }
        if (nLhs == 2) {
            ArrayOf U;
            ArrayOf T;
            SchurDecomposition(argIn[0], asComplex, U, T);
            retval << U;
            retval << T;
        } else {
            ArrayOf T;
            SchurDecomposition(argIn[0], asComplex, T);
            retval << T;
        }
    }
    return retval;
}
//=============================================================================
