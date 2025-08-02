//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "parfevalBuiltin.hpp"
#include "BackgroundPoolObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "FevalFutureObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::parfevalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, std::numeric_limits<int>::max());
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("backgroundPool handle expected."));
    }
    if (param1.getHandleCategory() != NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
        Error(_W("backgroundPool handle expected."));
    }
    ArrayOf param2 = argIn[1];
    if (!param2.isFunctionHandle()) {
        Error(_W("function handle handle expected."));
    }
    function_handle fh = param2.getContentAsFunctionHandle();
    if (fh.anonymousHandle == nullptr) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    FunctionDef* funcDef = nullptr;
    ArrayOfVector args;
    for (size_t k = 3; k < argIn.size(); ++k) {
        args << argIn[k];
    }
    if (fh.anonymousHandle != nullptr) {
        funcDef = (FunctionDef*)fh.anonymousHandle;
    }
    if (!funcDef) {
        Error(_W("Invalid anonymous function."));
    }
    ArrayOf param3 = argIn[2];
    bool isReal = param3.getDataClass() == NLS_DOUBLE || param3.getDataClass() == NLS_SINGLE;
    if (!isReal) {
        Error(_W("integer value expected."));
    }
    double value = param3.getContentAsDoubleScalar();
    int ivalue = (int)(value);
    if (value != (double)ivalue) {
        Error(_W("integer value expected."));
    }
    if (ivalue < 0) {
        Error(_W("non negative value expected."));
    }
    auto* backgroundPoolObject = (BackgroundPoolObject*)param1.getContentAsHandleScalar();
    if (backgroundPoolObject) {
        retval << backgroundPoolObject->feval(funcDef, ivalue, args);
    } else {
        Error(_W("Invalid backgroundPool handle."));
    }
    return retval;
}
//=============================================================================
