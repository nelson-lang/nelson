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
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        modifiedmsg = param2.getContentAsWideString();
    }
    ArrayOf param1 = argIn[0];
    if (!param1.isLogical()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    if (!param1.isScalar()) {
        Error(ERROR_SIZE_SCALAR_EXPECTED);
    }
    logical res = param1.getContentAsLogicalScalar();
    std::wstring msg;
    res = Assert_IsFalse(res, modifiedmsg, msg);
    if (nLhs == 0) {
        if (res == 0) {
            Error(msg);
        }
    } else {
        retval << ArrayOf::logicalConstructor(res == 0 ? false : true);
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(msg);
        }
    }
    return retval;
}
//=============================================================================
