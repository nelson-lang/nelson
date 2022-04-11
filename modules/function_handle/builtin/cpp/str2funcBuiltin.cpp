//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "str2funcBuiltin.hpp"
#include "Error.hpp"
#include "StringToFunctionHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::str2funcBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring wfunctionname;
    if (argIn[0].isRowVectorCharacterArray()) {
        wfunctionname = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    function_handle fptr = StringToFunctionHandle(wfunctionname);
    if (fptr.name.empty() && fptr.anonymous.empty()) {
        Error(_W("A valid function name expected."));
    }
    retval << ArrayOf::functionHandleConstructor(fptr);
    return retval;
}
//=============================================================================
