//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_isequalBuiltin.hpp"
#include "Assert_IsEqual.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(nLhs);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring customMessage;
    if (argIn.size() == 3) {
        customMessage = argIn[2].asWideString();
    }
    std::wstring msg;
    bool bRes = Assert_IsEqual(eval, param1, param2, msg);
    if (!bRes && !customMessage.empty()) {
        msg = customMessage + L" " + msg;
    }
    if (nLhs == 0) {
        if (!bRes) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", msg);
        }
    } else {
        retval << ArrayOf::logicalScalar(bRes);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(msg);
        }
    }
    return retval;
}
//=============================================================================
