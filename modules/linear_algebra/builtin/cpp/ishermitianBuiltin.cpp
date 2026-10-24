//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ishermitianBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "IsHermitian.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::ishermitianBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool skew = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            std::wstring str = param2.asWideString();
            if (str == L"skew" || str == L"nonskew") {
                if (str == L"skew") {
                    skew = true;
                } else {
                    skew = false;
                }
            } else {
                Error(L"Nelson:ishermitian:invalidOption",
                    L"Second input must be 'skew' or 'nonskew'.");
            }
        } else {
            Error(
                L"Nelson:ishermitian:invalidOption", L"Second input must be 'skew' or 'nonskew'.");
        }
    }
    if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isCharacterArray()) {
        OverloadRequired("ishermitian");
    }
    retval << ArrayOf::logicalScalar(IsHermitian(argIn[0], skew, "ishermitian"));
    return retval;
}
//=============================================================================
