//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <numeric>
#include <chrono>
#include "timeitBuiltin.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "ParallelSort.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static double
evaluateFunctionWithTimerRaw(Evaluator* eval, AnonymousMacroFunctionDef* funcDef, int nLhs,
    const ArrayOfVector& inputVariables);
//=============================================================================
static double
evaluateFunctionWithTimerNth(Evaluator* eval, AnonymousMacroFunctionDef* funcDef, int nLhs, int Nth,
    const ArrayOfVector& inputVariables);
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timeitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);

    if (argIn.size() < 1) {
        raiseError(L"Nelson:time:ERROR_WRONG_NUMBERS_INPUT_ARGS", ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    int nLhsTimeIt = -1;
    if (argIn.size() > 1) {
        nLhsTimeIt = argIn[1].getContentAsInteger32Scalar();
    }
    if (!argIn[0].isFunctionHandle()) {
        raiseError(L"Nelson:timeit:ERROR_TIMEIT_INVALID_FUNCTION_HANDLE",
            ERROR_TIMEIT_INVALID_FUNCTION_HANDLE);
    }

    if (eval == nullptr) {
        raiseError(L"Nelson:time:ERROR_EVALUATOR_NOT_DEFINED", ERROR_EVALUATOR_NOT_DEFINED);
    }
    AnonymousMacroFunctionDef* funcDef = nullptr;
    function_handle fh = argIn[0].getContentAsFunctionHandle();
    if (fh.anonymousHandle == nullptr) {
        raiseError(L"Nelson:time:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_FUNCTION_HANDLE_STR);
    }
    if (fh.anonymousHandle != nullptr) {
        funcDef = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    }
    if (funcDef == nullptr) {
        raiseError(L"Nelson:time:ERROR_FUNCTION_NOT_FOUND", ERROR_FUNCTION_NOT_FOUND);
    }

    if (argIn.size() > 2) {
        if (funcDef->inputArgCount() != -1) {
            if (funcDef->inputArgCount() != argIn.size() - 2) {
                raiseError(
                    L"Nelson:time:ERROR_WRONG_NUMBERS_INPUT_ARGS", ERROR_WRONG_NUMBERS_INPUT_ARGS);
            }
        }
    }

    ArrayOfVector inputs;
    if (argIn.size() > 2) {
        inputs = argIn;
        inputs.pop_front();
        inputs.pop_front();
    }

    double rawTime = evaluateFunctionWithTimerRaw(eval, funcDef, nLhsTimeIt, inputs);

    double desiredInnerLoopTime = 0.001;
    double numberInnerIterations = std::max(ceil(desiredInnerLoopTime / rawTime), 1.);
    double numberOuterIterations = 11;

    double estimatedRunningTime = numberOuterIterations * numberInnerIterations * rawTime;
    double longTime = 15;
    if (estimatedRunningTime > longTime) {
        double minimunOuterIterations = 3;
        numberOuterIterations = ceil(longTime / (numberInnerIterations * rawTime));
        numberOuterIterations = std::max(numberOuterIterations, minimunOuterIterations);
    }

    std::vector<double> runtime;

    for (size_t k = 0; k < (size_t)numberOuterIterations; ++k) {
        runtime.push_back(evaluateFunctionWithTimerNth(
            eval, funcDef, nLhsTimeIt, (int)numberInnerIterations, inputs));
    }
    parallelSort(runtime);
    double median = runtime[runtime.size() / 2];
    double t = median / numberInnerIterations;
    t = std::max(t, 0.);
    retval << ArrayOf::doubleConstructor(t);
    return retval;
}
//=============================================================================
double
evaluateFunctionWithTimerRaw(Evaluator* eval, AnonymousMacroFunctionDef* funcDef, int nLhs,
    const ArrayOfVector& inputVariables)
{
    std::vector<double> runtime;
    double timeThreshold = 3;
    double iterationCount = 0;
    ArrayOfVector inputEmpty;

    while (std::accumulate(runtime.begin(), runtime.end(), 0.) < 0.001) {
        iterationCount++;
        std::chrono::nanoseconds begin_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        ArrayOfVector resultEvaluate = funcDef->evaluateFunction(eval, inputVariables, nLhs);
        std::chrono::nanoseconds end_time
            = std::chrono::high_resolution_clock::now().time_since_epoch();
        double seconds = (end_time - begin_time).count() * 1e-9;
        runtime.push_back(seconds);
        if (iterationCount == 1) {
            if (seconds > timeThreshold) {
                break;
            }
            runtime.clear();
        }
    }
    parallelSort(runtime);
    double median = runtime[runtime.size() / 2];
    return (double)median;
}
//=============================================================================
static double
evaluateFunctionWithTimerNth(Evaluator* eval, AnonymousMacroFunctionDef* funcDef, int nLhs, int Nth,
    const ArrayOfVector& inputVariables)
{
    std::chrono::nanoseconds begin_time
        = std::chrono::high_resolution_clock::now().time_since_epoch();
    for (int n = 0; n < Nth; ++n) {
        ArrayOfVector resultEvaluate = funcDef->evaluateFunction(eval, inputVariables, nLhs);
    }
    std::chrono::nanoseconds end_time
        = std::chrono::high_resolution_clock::now().time_since_epoch();
    std::chrono::nanoseconds difftime = (end_time - begin_time);
    return (double)(difftime.count() * 1e-9);
}
//=============================================================================
