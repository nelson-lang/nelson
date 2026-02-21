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
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadName.hpp"
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
    std::wstring command = param1.getContentAsWideString();
    std::wstring expectedmsg;
    std::wstring expectedid;
    if (param2.isClassType() && param2.getClassType() == "message") {
        expectedid = param2.getField("Identifier").getContentAsWideString();
        FunctionDefPtr funcDef = nullptr;
        if (!eval->lookupFunction(OVERLOAD_FUNCTION_NAME("message", "getString"), funcDef)) {
            raiseError2(_E("nelson:runtime:functionNotFound"), L"getString");
        }
        ArrayOfVector inputs;
        inputs << param2;
        try {
            ArrayOfVector ouputs = funcDef->evaluateFunction(eval, inputs, 1);
            expectedmsg = ouputs[0].getContentAsWideString();
        } catch (const Exception&) {
            raiseError2(_E("nelson:runtime:incorrectHoleCount"), expectedid);
        }
    } else {
        expectedmsg = param2.getContentAsWideString();
    }
    if (expectedmsg.empty()) {
        raiseError2(_E("nelson:validators:mustBeNonemptyAtPosition"), 2);
    }
    if (argIn.size() == 3) {
        expectedid = argIn[2].getContentAsWideString();
    }
    std::wstring msg;
    bool res;
    if (!expectedid.empty()) {
        res = Assert_CheckError(eval, command, expectedmsg, expectedid, msg);
    } else {
        res = Assert_CheckError(eval, command, expectedmsg, msg);
    }

    if (nLhs == 0) {
        if (!res) {
            Error(msg, _E("nelson:assert:assertionFailed"));
        }
    } else {
        retval << ArrayOf::logicalConstructor(res);
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(msg);
        }
    }
    return retval;
}
//=============================================================================
