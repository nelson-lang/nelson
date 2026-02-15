//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <thread>
#include "afterAllBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "FutureObjectHelpers.hpp"
#include "FevalFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::afterAllBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        raiseError(L"Nelson:parallel:ERROR_FUTURE_HANDLE_EXPECTED", ERROR_FUTURE_HANDLE_EXPECTED);
    }
    bool isSupportedType = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);
    if (!isSupportedType) {
        raiseError(L"Nelson:parallel:ERROR_FEVALFUTURE_HANDLE_EXPECTED",
            ERROR_FEVALFUTURE_HANDLE_EXPECTED);
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
        raiseError(L"Nelson:parallel:ERROR_INTEGER_VALUE_EXPECTED", ERROR_INTEGER_VALUE_EXPECTED);
    }
    double value = param3.getContentAsDoubleScalar();
    int ivalue = (int)(value);
    if (value != (double)ivalue) {
        raiseError(L"Nelson:parallel:ERROR_INTEGER_VALUE_EXPECTED", ERROR_INTEGER_VALUE_EXPECTED);
    }
    if (ivalue < 0) {
        raiseError(L"Nelson:parallel:ERROR_NON_NEGATIVE_VALUE_EXPECTED",
            ERROR_NON_NEGATIVE_VALUE_EXPECTED);
    }

    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);

    AfterAllFutureObject* afterAllFuture
        = new AfterAllFutureObject(utf8_to_wstring(funcDef->getName()), futures);
    std::thread _thread
        = std::thread(&AfterAllFutureObject::afterAll, afterAllFuture, funcDef, ivalue, false);
    _thread.detach();
    return ArrayOf::handleConstructor(afterAllFuture);
}
//=============================================================================
