//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_isfalseBuiltin.hpp"
#include "Assert_IsFalse.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_isfalseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(nLhs);
    std::wstring modifiedmsg;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        modifiedmsg = param2.asWideString();
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isLogical()) {
        Error(L"Nelson:asserts:invalidArgument", L"Wrong type for argument #%d: %s expected.", (1),
            L"logical");
    }
    if (!param1.isScalar()) {
        Error(L"Nelson:asserts:scalarExpected", L"A scalar expected.");
    }
    logical res = param1.asLogicalScalar();
    std::wstring msg;
    res = Assert_IsFalse(res, modifiedmsg, msg);
    if (nLhs == 0) {
        if (res == 0) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", msg);
        }
    } else {
        retval << ArrayOf::logicalScalar(res == 0 ? false : true);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(msg);
        }
    }
    return retval;
}
//=============================================================================
