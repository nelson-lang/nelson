//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_isapproxBuiltin.hpp"
#include "Assert_IsApprox.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_isapproxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(nLhs);
    double precision = 0.;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        precision = param3.getContentAsDoubleScalar();
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring msg;
    bool bRes = Assert_IsApprox(eval, param1, param2, precision, msg);
    if (nLhs == 0) {
        if (!bRes) {
            Error(msg);
        }
    } else {
        retval << ArrayOf::logicalConstructor(bRes);
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(msg);
        }
    }
    return retval;
}
//=============================================================================
