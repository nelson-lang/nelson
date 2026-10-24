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
#include "ErrorID.hpp"
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
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    int nLhsTimeIt = -1;
    if (argIn.size() > 1) {
        nLhsTimeIt = argIn[1].asInt32Scalar();
    }
    if (!argIn[0].isFunctionHandle()) {
        Error(L"Nelson:timeit:InvalidFunctionHandle",
            L"First argument must be a function handle that takes no input argument.");
    }

    if (eval == nullptr) {
        Error(L"Nelson:time:evaluatorDefined", L"evaluator is not defined.");
    }
    AnonymousMacroFunctionDef* funcDef = nullptr;
    function_handle fh = argIn[0].asFunctionHandle();
    if (fh.anonymousHandle == nullptr) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    if (fh.anonymousHandle != nullptr) {
        funcDef = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    }
    if (funcDef == nullptr) {
        Error(L"Nelson:time:functionFound", L"Function not found.");
    }

    if (argIn.size() > 2) {
        if (funcDef->inputArgCount() != -1) {
            size_t suppliedInputs = argIn.size() - 2;
            if (suppliedInputs < (size_t)funcDef->inputArgCount()) {
                Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
            }
            if (suppliedInputs > (size_t)funcDef->inputArgCount()) {
                Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
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
        double minimumOuterIterations = 3;
        numberOuterIterations = ceil(longTime / (numberInnerIterations * rawTime));
        numberOuterIterations = std::max(numberOuterIterations, minimumOuterIterations);
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
    retval << ArrayOf::doubleScalar(t);
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
