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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "FevalFutureObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::parfevalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, std::numeric_limits<int>::max());
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(L"Nelson:backgroundPool:handleExpected", L"backgroundPool handle expected.");
    }
    if (param1.handleCategory() != NLS_HANDLE_BACKGROUNDPOOL_CATEGORY_STR) {
        Error(L"Nelson:backgroundPool:handleExpected", L"backgroundPool handle expected.");
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
    auto* anonymousDef = dynamic_cast<AnonymousMacroFunctionDef*>(funcDef);
    if (anonymousDef != nullptr && anonymousDef->isFunctionHandle()
        && !anonymousDef->isBoundNestedFunctionHandle()) {
        FunctionDef* resolvedFunction = nullptr;
        if (eval != nullptr && eval->lookupFunction(anonymousDef->getContent(), resolvedFunction)
            && resolvedFunction != nullptr) {
            funcDef = resolvedFunction;
        }
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
    auto* backgroundPoolObject = (BackgroundPoolObject*)param1.asHandleScalar();
    if (backgroundPoolObject) {
        retval << backgroundPoolObject->feval(param2, funcDef, ivalue, args);
    } else {
        Error(L"Nelson:parfeval:invalidBackgroundPool", L"Invalid backgroundPool handle.");
    }
    return retval;
}
//=============================================================================
