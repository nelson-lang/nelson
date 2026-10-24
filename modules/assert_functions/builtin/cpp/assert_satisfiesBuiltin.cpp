//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_extraBuiltins.hpp"
#include "ErrorID.hpp"
#include "assert_modernHelpers.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_satisfiesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    FunctionDef* funcDef = nullptr;
    if (argIn[1].isFunctionHandle()) {
        function_handle fh = argIn[1].asFunctionHandle();
        funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    } else if (argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray()) {
        std::wstring name = argIn[1].asWideString();
        if (!eval->getContext()->lookupFunction(name, funcDef)) {
            Error(L"Nelson:asserts:unknownPredicate", L"Unknown predicate function.");
        }
    } else {
        Error(L"Nelson:asserts:invalidPredicate:notFunctionHandle",
            L"Predicate must be a function handle or function name.");
    }
    if (funcDef == nullptr) {
        Error(L"Nelson:asserts:invalidPredicate:invalid", L"Invalid predicate function.");
    }
    ArrayOfVector inputs;
    inputs << argIn[0];
    ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
    if (outputs.size() != 1 || !outputs[0].isLogical() || !outputs[0].isScalar()) {
        Error(L"Nelson:asserts:invalidPredicate:notScalarLogical",
            L"Predicate must return a scalar logical.");
    }
    bool passed = outputs[0].asLogicalScalar() ? true : false;
    std::wstring msg = passed
        ? L""
        : resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsSatisfies",
              L"Assertion failed: asserts.satisfies predicate returned false.");
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
