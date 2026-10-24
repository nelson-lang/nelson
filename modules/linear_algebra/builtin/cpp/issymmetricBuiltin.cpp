//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "issymmetricBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "IsSymmetric.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::issymmetricBuiltin(int nLhs, const ArrayOfVector& argIn)
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
            withTol = true;
            tol = param2.asDoubleScalar();
            if (!std::isfinite(tol) || tol < 0.) {
                Error(L"Nelson:issymmetric:invalidTolerance",
                    L"Second input must be finite and >= 0.");
            }
        }
    }
    if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isCharacterArray()) {
        OverloadRequired("issymmetric");
    }
    if (withTol) {
        retval << ArrayOf::logicalScalar(IsSymmetric(argIn[0], tol, "issymmetric"));
    } else {
        retval << ArrayOf::logicalScalar(IsSymmetric(argIn[0], skew, "issymmetric"));
    }
    return retval;
}
//=============================================================================
