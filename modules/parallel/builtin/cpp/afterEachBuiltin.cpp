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
#include "afterEachBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "AfterEachFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "FutureObjectHelpers.hpp"
#include "HandleState.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::afterEachBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(L"Nelson:future:handleExpected", L"Future handle expected.");
    }
    bool isSupportedType = (param1.handleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.handleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.handleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);
    if (!isSupportedType) {
        Error(L"Nelson:parallel:fevalFutureExpected", L"FevalFuture handle expected.");
    }
    ArrayOf param2 = argIn[1];
    if (!param2.isFunctionHandle()) {
        Error(L"Nelson:parallel:invalidFunctionHandle", L"function handle handle expected.");
    }
    function_handle fh = param2.asFunctionHandle();
    if (fh.anonymousHandle == nullptr) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
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
        Error(L"Nelson:parallel:invalidAnonymousFunction", L"Invalid anonymous function.");
    }
    ArrayOf param3 = argIn[2];
    bool isReal = param3.dataClass() == NLS_DOUBLE || param3.dataClass() == NLS_SINGLE;
    if (!isReal) {
        Error(L"Nelson:validators:mustBeInteger", L"%s must be an integer.", L"Value");
    }
    double value = param3.asDoubleScalar();
    int ivalue = (int)(value);
    if (value != (double)ivalue) {
        Error(L"Nelson:validators:mustBeInteger", L"%s must be an integer.", L"Value");
    }
    if (ivalue < 0) {
        Error(L"Nelson:parallel:nonNegativeValueExpected", L"non negative value expected.");
    }

    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);

    AfterEachFutureObject* afterEachFuture
        = new AfterEachFutureObject(utf8_to_wstring(funcDef->getName()), futures);
    ArrayOf result = ArrayOf::handle(afterEachFuture);
    HandleRef* qp = (HandleRef*)(result.dataPointer());
    HandleRef futureRef = qp[0];
    afterEachFuture->asNelsonHandle = futureRef ? futureRef->id() : 0;
    // The thread keeps a copy of the function handle value so the definition
    // behind funcDef outlives the caller's variables.
    ArrayOf functionHandle = param2;
    std::thread _thread([afterEachFuture, funcDef, ivalue, functionHandle]() {
        afterEachFuture->afterEach(funcDef, ivalue, false);
    });
    _thread.detach();
    return result;
}
//=============================================================================
