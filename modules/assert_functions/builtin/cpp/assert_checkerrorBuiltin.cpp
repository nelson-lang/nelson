//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_checkerrorBuiltin.hpp"
#include "Assert_CheckError.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_checkerrorBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring command = param1.asWideString();
    std::wstring expectedmsg = param2.asWideString();

    if (expectedmsg.empty()) {
        Error(L"Nelson:asserts:invalidArgument:emptyExpectedMessage",
            L"empty string not allowed as expected message.");
    }
    std::wstring msg;
    bool res;
    if (argIn.size() == 3) {
        std::wstring expectedid = argIn[2].asWideString();
        res = Assert_CheckError(eval, command, expectedmsg, expectedid, msg);
    } else {
        res = Assert_CheckError(eval, command, expectedmsg, msg);
    }

    if (nLhs == 0) {
        if (!res) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", msg);
        }
    } else {
        retval << ArrayOf::logicalScalar(res);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(msg);
        }
    }
    return retval;
}
//=============================================================================
