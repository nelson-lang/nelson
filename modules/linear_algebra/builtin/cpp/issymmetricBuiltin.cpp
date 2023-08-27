//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "issymmetricBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsSymmetric.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::issymmetricBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool skew = false;
    double tol = 0;
    bool withTol = false;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isRowVectorCharacterArray()) {
            withTol = false;
            std::wstring str = param2.getContentAsWideString();
            if (str == L"skew" || str == L"nonskew") {
                if (str == L"skew") {
                    skew = true;
                } else {
                    skew = false;
                }
            } else {
                Error(_W("Second input must be 'skew' or 'nonskew'."));
            }
        } else {
            withTol = true;
            tol = param2.getContentAsDoubleScalar();
            if (!std::isfinite(tol) || tol < 0.) {
                Error(_W("Second input must be finite and >= 0."));
            }
        }
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "issymmetric", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isCharacterArray()) {
            retval = OverloadFunction(eval, nLhs, argIn, "issymmetric", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired("issymmetric");
        }
        if (withTol) {
            retval << ArrayOf::logicalConstructor(IsSymmetric(argIn[0], tol, "issymmetric"));
        } else {
            retval << ArrayOf::logicalConstructor(IsSymmetric(argIn[0], skew, "issymmetric"));
        }
    }
    return retval;
}
//=============================================================================
