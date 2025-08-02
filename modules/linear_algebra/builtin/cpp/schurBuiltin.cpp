//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "schurBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "SchurDecompostion.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::schurBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
        || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
        OverloadRequired("schur");
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
    return retval;
}
//=============================================================================
