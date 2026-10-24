//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "str2funcBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringToFunctionHandle.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::str2funcBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::string functionname;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        functionname = argIn[0].asUtf8String();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    function_handle fptr = StringToFunctionHandle(eval, functionname);
    if (fptr.anonymousHandle == nullptr) {
        Error(L"Nelson:dispatcher:invalidFunctionName", L"A valid function name expected.");
    }
    retval << ArrayOf::functionHandle(fptr);
    return retval;
}
//=============================================================================
