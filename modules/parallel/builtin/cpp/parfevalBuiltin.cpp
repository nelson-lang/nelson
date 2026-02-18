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
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:parallel:ERROR_BACKGROUNDPOOL_HANDLE_EXPECTED",
            ERROR_BACKGROUNDPOOL_HANDLE_EXPECTED);
    }
    if (param1.getHandleCategory() != NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
        raiseError(L"Nelson:parallel:ERROR_BACKGROUNDPOOL_HANDLE_EXPECTED",
            ERROR_BACKGROUNDPOOL_HANDLE_EXPECTED);
    }
    ArrayOf param2 = argIn[1];
    if (!param2.isFunctionHandle()) {
        raiseError(L"Nelson:parallel:ERROR_FUNCTION_HANDLE_HANDLE_EXPECTED",
            ERROR_FUNCTION_HANDLE_HANDLE_EXPECTED);
    }
    function_handle fh = param2.getContentAsFunctionHandle();
    if (fh.anonymousHandle == nullptr) {
        raiseError2(L"nelson:validators:mustBeType", 1, NLS_FUNCTION_HANDLE_STR);
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
        raiseError(
            L"Nelson:parallel:ERROR_INVALID_ANONYMOUS_FUNCTION", ERROR_INVALID_ANONYMOUS_FUNCTION);
    }
    ArrayOf param3 = argIn[2];
    bool isReal = param3.getDataClass() == NLS_DOUBLE || param3.getDataClass() == NLS_SINGLE;
    if (!isReal) {
        raiseError2(L"nelson:validators:mustBeIntegerAtPosition", 3);
    }
    double value = param3.getContentAsDoubleScalar();
    int ivalue = (int)(value);
    if (value != (double)ivalue) {
        raiseError2(L"nelson:validators:mustBeIntegerAtPosition", 3);
    }
    if (ivalue < 0) {
        raiseError2(L"nelson:validators:mustBeNonNegativeIntegerValue", 3);
    }
    auto* backgroundPoolObject = (BackgroundPoolObject*)param1.getContentAsHandleScalar();
    if (backgroundPoolObject) {
        retval << backgroundPoolObject->feval(funcDef, ivalue, args);
    } else {
        raiseError(L"Nelson:parallel:ERROR_INVALID_BACKGROUNDPOOL_HANDLE",
            ERROR_INVALID_BACKGROUNDPOOL_HANDLE);
    }
    return retval;
}
//=============================================================================
