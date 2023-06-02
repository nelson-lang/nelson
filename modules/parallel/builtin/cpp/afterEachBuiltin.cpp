//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <thread>
#include "afterEachBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "AfterEachFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "FutureObjectHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::afterEachBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("Future handle expected."));
    }
    bool isSupportedType = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTERALLFUTURE_CATEGORY_STR)
        || (param1.getHandleCategory() == NLS_HANDLE_AFTEREACHFUTURE_CATEGORY_STR);
    if (!isSupportedType) {
        Error(_W("FevalFuture handle expected."));
    }
    ArrayOf param2 = argIn[1];
    if (!param2.isFunctionHandle()) {
        Error(_W("function handle handle expected."));
    }
    function_handle fh = param2.getContentAsFunctionHandle();
    if (fh.anonymousHandle == nullptr && fh.name.empty()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    std::string fname = fh.name;
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    ArrayOfVector args;
    for (size_t k = 3; k < argIn.size(); ++k) {
        args << argIn[k];
    }
    if (!fh.name.empty()) {
        context->lookupFunction(fname, funcDef);
    } else if (fh.anonymousHandle != nullptr) {
        funcDef = (FunctionDef*)fh.anonymousHandle;
    }
    if (!funcDef) {
        if (!fh.name.empty()) {
            Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a function."));
        } else {
            Error(_W("Invalid anonymous function."));
        }
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

    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);

    AfterEachFutureObject* afterEachFuture
        = new AfterEachFutureObject(utf8_to_wstring(funcDef->getName()), futures);
    std::thread _thread
        = std::thread(&AfterEachFutureObject::afterEach, afterEachFuture, funcDef, ivalue, false);
    _thread.detach();
    return ArrayOf::handleConstructor(afterEachFuture);
}
//=============================================================================
