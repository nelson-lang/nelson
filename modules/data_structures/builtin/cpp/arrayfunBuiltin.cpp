//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/xchar.h>
#include "arrayfunBuiltin.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "AnonymousElementwiseAnalyzer.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BytecodeVM.hpp"
#include "ClassName.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ErrorToStruct.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "omp_for_loop.hpp"
#include "NewWithException.hpp"
#include "ArrayOfView.hpp"
#include "MutableArrayOfView.hpp"
#include "FunctionHandleCallback.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// case-insensitive prefix abbreviations of the option names
// 'UniformOutput' and 'ErrorHandler' (at least two characters, e.g. 'un').
static bool
matchesFunctionHandleOption(const std::wstring& argName, const std::wstring& fullName)
{
    if (argName.size() < 2 || argName.size() > fullName.size()) {
        return false;
    }
    return StringHelpers::iequals(argName, fullName.substr(0, argName.size()));
}
//=============================================================================
// One-time resolution of the direct VM execution path for a capture-free
// anonymous callback: avoids the per-element lock/recheck round-trip of
// AnonymousMacroFunctionDef::evaluateFunction. Scope-free chunks contain no
// call opcodes, so the chunk cannot be invalidated while the loop runs.
static const BytecodeChunk*
scopeFreeCallbackChunk(
    FunctionDefPtr fptr, FunctionDefPtr fptrHandleError, indexType argcount, int nargout)
{
    if (fptrHandleError != nullptr || fptr == nullptr
        || fptr->type() != NLS_ANONYMOUS_MACRO_FUNCTION) {
        return nullptr;
    }
    auto* anonymousDef = dynamic_cast<AnonymousMacroFunctionDef*>(fptr);
    if (anonymousDef == nullptr) {
        return nullptr;
    }
    return anonymousDef->scopeFreeBytecode(static_cast<size_t>(argcount - 1), nargout);
}
//=============================================================================
// Auto-vectorization fast path. Every element handed to the callback is a
// scalar and all inputs have the same size, so an element-wise body such as
// `@(x,y) sqrt(x.^2 + y.^2)`, or a handle like @sin, gives the same result when
// evaluated ONCE on the full arrays. The strict predicates
// (isAnonymousBodyElementwiseVectorizable, isNamedHandleElementwiseSameSize)
// live in the interpreter and reject anything whose array behavior differs from
// its scalar behavior. When neither matches, the caller uses the per-element loop.
namespace {
//=============================================================================
// Returns true and fills `result` on success. Returns false, leaving `result`
// untouched, when the callback is not provably element-wise.
static bool
tryElementwiseVectorize(ArrayOfVector& result, int nargout, const ArrayOfVector& argIn,
    Evaluator* eval, const Dimensions& argdims, indexType nbInputs, FunctionDef* funcDef,
    bool isUniform, FunctionDefPtr fptrError)
{
    if (!isUniform || nargout != 1 || fptrError != nullptr || funcDef == nullptr) {
        return false;
    }
    if (funcDef->type() != NLS_ANONYMOUS_MACRO_FUNCTION) {
        return false;
    }
    auto* def = dynamic_cast<AnonymousMacroFunctionDef*>(funcDef);
    if (def == nullptr) {
        return false;
    }
    // The one-shot result inherits its shape from the inputs; an empty input
    // set has no defined shape here, so leave that (rare) case to the loop.
    if (argdims.elementCount() == 0) {
        return false;
    }
    const size_t numArrayInputs = static_cast<size_t>(nbInputs - 1);
    bool eligible = false;
    if (def->isFunctionHandle()) {
        eligible = isNamedHandleElementwiseSameSize(def, numArrayInputs);
    } else {
        stringVector parameters = def->getArguments();
        if (parameters.size() != numArrayInputs) {
            return false;
        }
        for (const std::string& p : parameters) {
            if (p == "varargin") {
                return false;
            }
        }
        eligible = isAnonymousBodyElementwiseVectorizable(def);
    }
    if (!eligible) {
        return false;
    }
    // Cells, structs, strings and objects are excluded: their element
    // semantics differ from a scalar.
    ArrayOfVector callInputs;
    callInputs.reserve(numArrayInputs);
    for (indexType j = 1; j < nbInputs; ++j) {
        const ArrayOf& in = argIn[j];
        if (in.isSparse() || !(in.isNumeric() || in.isLogical())) {
            return false;
        }
        callInputs.push_back(in);
    }
    ArrayOfVector ret = funcDef->evaluateFunction(eval, callInputs, 1);
    if (ret.size() < 1) {
        return false;
    }
    ArrayOf out = ret[0];
    // A body that ignores its arguments and returns a scalar does not
    // reproduce the input shape: leave it to the per-element loop.
    if (out.elementCount() != argdims.elementCount()) {
        return false;
    }
    Dimensions outputDims = argdims;
    out.reshape(outputDims);
    result.clear();
    result.push_back(out);
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
static ArrayOfVector
arrayfun_nonuniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    const Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType nbElements = argdims.elementCount();
    for (int j = 0; j < nargout; j++) {
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        for (indexType k = 0; k < nbElements; ++k) {
            elements[k] = ArrayOf::empty();
        }
        ArrayOf arrayResult(NLS_CELL_ARRAY, argdims, elements);
        outputs.push_back(arrayResult);
    }
    std::vector<MutableArrayOfView<ArrayOf>> outputViews;
    outputViews.reserve(outputs.size());
    for (auto& output : outputs) {
        outputViews.emplace_back(output, ArrayOfViewAssumeValidated {});
    }

    std::vector<ArrayOf> inputArguments;
    inputArguments.reserve(static_cast<size_t>(argcount - 1));
    for (indexType j = 1; j < argcount; j++) {
        if (argIn[j].isSparse()) {
            Error(L"Nelson:data_structures:sparseInputsSupported",
                L"Sparse inputs are not supported.");
        }
        inputArguments.push_back(argIn[j]);
    }
    const BytecodeChunk* directChunk
        = scopeFreeCallbackChunk(fptr, fptrHandleError, argcount, nargout);
    for (indexType i = 0; i < nbElements; i++) {
        ArrayOfVector input;
        input.reserve(inputArguments.size());
        for (auto& inputArgument : inputArguments) {
            input.push_back(inputArgument.valueAtIndex(i));
        }

        ArrayOfVector ret = DataStructuresGateway::evaluateCallback(
            eval, fptr, fptrHandleError, directChunk, input, nargout, false, i + 1);
        if ((int)ret.size() < nargout) {
            Error(L"Nelson:data_structures:functionReturnedFewerOutputsExpected",
                L"function returned fewer outputs than expected");
        }

        for (indexType j = 0; j < (indexType)nargout; j++) {
            outputViews[j][i] = ret[j];
        }
    }
    return outputs;
}
//=============================================================================
static ArrayOfVector
arrayfun_uniformBuiltin(int nargout, bool isStatementCall, const ArrayOfVector& argIn,
    Evaluator* eval, Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType nbElements = argdims.elementCount();
    if (nbElements == 0) {
        for (int j = 0; j < nargout; j++) {
            outputs << ArrayOf::empty(argdims);
        }
        return outputs;
    }
    std::vector<ArrayOf> inputArguments;
    inputArguments.reserve(static_cast<size_t>(argcount - 1));
    for (indexType j = 1; j < argcount; j++) {
        if (argIn[j].isSparse()) {
            Error(L"Nelson:data_structures:sparseInputsSupported",
                L"Sparse inputs are not supported.");
        }
        inputArguments.push_back(argIn[j]);
    }
    const BytecodeChunk* directChunk
        = scopeFreeCallbackChunk(fptr, fptrHandleError, argcount, nargout);
    int callNargout = nargout;
    bool noOutput = false;
    for (indexType i = 0; i < nbElements; i++) {
        ArrayOfVector input;
        input.reserve(inputArguments.size());
        for (auto& inputArgument : inputArguments) {
            input.push_back(inputArgument.valueAtIndex(i));
        }
        ArrayOfVector ret;
        if (isStatementCall && i == 0) {
            // called as a statement: a function without output (e.g. disp) is
            // then applied with nargout 0 on every element.
            try {
                ret = DataStructuresGateway::evaluateCallback(
                    eval, fptr, fptrHandleError, directChunk, input, nargout, true, i + 1);
            } catch (NelsonException& e) {
                if (!DataStructuresGateway::isTooManyOutputsError(e)) {
                    throw;
                }
                callNargout = 0;
                directChunk = scopeFreeCallbackChunk(fptr, fptrHandleError, argcount, 0);
                ret = DataStructuresGateway::evaluateCallback(
                    eval, fptr, fptrHandleError, directChunk, input, 0, false, i + 1);
            }
        } else {
            ret = DataStructuresGateway::evaluateCallback(
                eval, fptr, fptrHandleError, directChunk, input, callNargout, false, i + 1);
        }
        if ((int)ret.size() < nargout || noOutput) {
            if (callNargout == 0 && (i == 0 || noOutput)) {
                // no output: applied for its side effects only, arrayfun returns nothing.
                noOutput = true;
                continue;
            }
            Error(L"Nelson:data_structures:functionReturnedFewerOutputsExpected",
                L"function returned fewer outputs than expected");
        }

        if (i == 0) {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!ret[j].isScalar()) {
                    Error(L"Nelson:data_structures:nonScalarUniformOutput",
                        L"Non-scalar in Uniform output.");
                }
                outputs.push_back(ret[j]);
                outputs[j].resize(argdims);
            }
        } else {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                NelsonType outputClass = outputs[j].dataClass();
                NelsonType currentClass = ret[j].dataClass();
                if (currentClass != outputClass) {
                    bool isRealComplexPair
                        = ((outputClass == NLS_DOUBLE && currentClass == NLS_DCOMPLEX)
                            || (outputClass == NLS_DCOMPLEX && currentClass == NLS_DOUBLE)
                            || (outputClass == NLS_SINGLE && currentClass == NLS_SCOMPLEX)
                            || (outputClass == NLS_SCOMPLEX && currentClass == NLS_SINGLE));
                    if (!isRealComplexPair) {
                        Error(L"Nelson:data_structures:uniformOutputMismatch",
                            L"All outputs of the function must be of the same data type when "
                            L"UniformOutput is true.");
                    }
                    if (currentClass == NLS_DCOMPLEX || currentClass == NLS_SCOMPLEX) {
                        outputs[j].promoteType(currentClass);
                    } else {
                        ArrayOf promoted = ret[j];
                        promoted.promoteType(outputClass);
                        outputs[j].setValueAtIndex(i, promoted);
                        continue;
                    }
                }
                outputs[j].setValueAtIndex(i, ret[j]);
            }
        }
    }
    return outputs;
}
//=============================================================================
static bool
getUniformOutputStatus(const ArrayOf& arg)
{
    if (arg.isScalar()) {
        if (arg.isLogical()) {
            return arg.asLogicalScalar();
        }
        if (arg.isDoubleClass()) {
            return (bool)arg.asDoubleScalar();
        }
    }
    Error(L"Nelson:validators:mustBeLogicalScalar", L"%s must be a logical scalar.",
        L"UniformOutput");
    return false;
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::arrayfunBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 2);

    // Delegate to an "@<class>/arrayfun" overload (e.g. gpuArray) when an array
    // argument defines one.
    for (size_t k = 1; k < argIn.size(); ++k) {
        const ArrayOf& candidate = argIn[k];
        if (candidate.isHandle() || candidate.isClassType()) {
            std::string overloadName = getOverloadFunctionName(ClassName(candidate), "arrayfun");
            FunctionDef* overload = nullptr;
            if (lookupOverloadFunctionCached(eval->getContext(), overloadName, overload)
                && overload != nullptr) {
                return overload->evaluateFunction(eval, argIn, nLhs);
            }
        }
    }

    int nbInputs = static_cast<int>(argIn.size());

    bool isUniform = true;
    bool hasUniformArg = false;
    bool hasErrorHandler = false;
    function_handle errorHandler;
    errorHandler.anonymousHandle = nullptr;

    // trailing 'UniformOutput' and 'ErrorHandler' name-value pairs, in any order
    for (int pair = 0; pair < 2; ++pair) {
        if (nbInputs < 4
            || !(argIn[nbInputs - 2].isRowVectorCharacterArray()
                || argIn[nbInputs - 2].isScalarStringArray())) {
            break;
        }
        std::wstring key = argIn[nbInputs - 2].asWideString();
        if (matchesFunctionHandleOption(key, L"UniformOutput") && !hasUniformArg) {
            isUniform = getUniformOutputStatus(argIn[nbInputs - 1]);
            hasUniformArg = true;
        } else if (matchesFunctionHandleOption(key, L"ErrorHandler") && !hasErrorHandler) {
            if (argIn[nbInputs - 1].isFunctionHandle()) {
                errorHandler = argIn[nbInputs - 1].asFunctionHandle();
                hasErrorHandler = true;
            } else {
                Error(L"Nelson:data_structures:errorHandlerFunctionHandle",
                    L"%s must be a function handle.", L"ErrorHandler");
            }
        } else {
            break;
        }
        nbInputs -= 2;
    }

    if (nbInputs < 2) {
        Error(L"Nelson:data_structures:functionHandleLeastOneArray",
            L"Function handle and at least one array are required.");
    }

    FunctionDef* funcDef = nullptr;
    ArrayOf funcInput = argIn[0];
    if (funcInput.isFunctionHandle()) {
        function_handle fh = funcInput.asFunctionHandle();
        funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    } else if (funcInput.isRowVectorCharacterArray()) {
        std::wstring funcName = funcInput.asWideString();
        if (!eval->getContext()->lookupFunction(funcName, funcDef)) {
            Error(L"Nelson:data_structures:unknownFunctionName", L"Unknown function name.");
        }
    } else {
        Error(L"Nelson:data_structures:invalidFunctionSpecification",
            L"Invalid function specification.");
    }

    if (funcDef == nullptr) {
        Error(L"Nelson:data_structures:invalidFunctionHandle", L"Invalid function handle.");
    }

    std::vector<indexType> dimsVector;
    for (size_t i = 1; i < nbInputs; ++i) {
        const ArrayOf& in = argIn[i];
        std::vector<indexType> inputDims = in.dimensions().asVector();
        if (inputDims.empty()) {
            continue;
        }

        if (dimsVector.empty()) {
            dimsVector.insert(dimsVector.end(), inputDims.begin(), inputDims.end());
        } else {
            if (dimsVector.size() != inputDims.size()) {
                Error(L"Nelson:data_structures:sameSizeExpected", L"Same size expected.");
            }
            for (size_t i = 0; i < dimsVector.size(); ++i) {
                if (inputDims[i] != dimsVector[i]) {
                    Error(L"Nelson:data_structures:sameSizeExpected", L"Same size expected.");
                }
            }
        }
    }
    Dimensions dims(dimsVector);
    int nargout = nLhs > 0 ? nLhs : 1;
    FunctionDefPtr fptrError = nullptr;
    if (errorHandler.anonymousHandle) {
        fptrError = reinterpret_cast<FunctionDef*>(errorHandler.anonymousHandle);
    }

    // Evaluate an element-wise callback once on the full arrays instead of
    // looping per element.
    ArrayOfVector vectorized;
    if (tryElementwiseVectorize(
            vectorized, nargout, argIn, eval, dims, nbInputs, funcDef, isUniform, fptrError)) {
        return vectorized;
    }

    if (isUniform) {
        retval = arrayfun_uniformBuiltin(
            nargout, nLhs == 0, argIn, eval, dims, nbInputs, funcDef, fptrError);
    } else {
        retval
            = arrayfun_nonuniformBuiltin(nargout, argIn, eval, dims, nbInputs, funcDef, fptrError);
    }
    return retval;
}
//=============================================================================
