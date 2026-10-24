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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "cellfunBuiltin.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BytecodeVM.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ErrorToStruct.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
static ArrayOfVector
cellfun_nonuniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    const Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    for (int j = 0; j < nargout; j++) {
        ArrayOf* elements = nullptr;
        size_t nbElements = argdims.elementCount();
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::empty();
        }
        ArrayOf c = ArrayOf(NLS_CELL_ARRAY, argdims, elements);
        outputs.push_back(c);
    }
    std::vector<MutableArrayOfView<ArrayOf>> outputViews;
    outputViews.reserve(outputs.size());
    for (auto& output : outputs) {
        outputViews.emplace_back(output, ArrayOfViewAssumeValidated {});
    }
    std::vector<ArrayOfView<ArrayOf>> inputViews;
    inputViews.reserve(static_cast<size_t>(argcount - 1));
    for (indexType j = 1; j < argcount; j++) {
        inputViews.emplace_back(argIn[j], ArrayOfViewAssumeValidated {});
    }
    const BytecodeChunk* directChunk
        = scopeFreeCallbackChunk(fptr, fptrHandleError, argcount, nargout);
    indexType elementCount = argdims.elementCount();
    for (indexType i = 0; i < elementCount; i++) {
        ArrayOfVector input;
        input.reserve(inputViews.size());
        for (auto& arg : inputViews) {
            input.push_back(arg[i]);
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
cellfun_uniformBuiltin(int nargout, bool isStatementCall, const ArrayOfVector& argIn,
    Evaluator* eval, Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType elementCount = argdims.elementCount();
    if (elementCount == 0) {
        for (int j = 0; j < nargout; j++) {
            outputs << ArrayOf::empty(argdims);
        }
        return outputs;
    }
    std::vector<ArrayOfView<ArrayOf>> inputViews;
    inputViews.reserve(static_cast<size_t>(argcount - 1));
    for (indexType j = 1; j < argcount; j++) {
        inputViews.emplace_back(argIn[j], ArrayOfViewAssumeValidated {});
    }
    const BytecodeChunk* directChunk
        = scopeFreeCallbackChunk(fptr, fptrHandleError, argcount, nargout);
    int callNargout = nargout;
    bool noOutput = false;
    for (indexType i = 0; i < elementCount; i++) {
        ArrayOfVector input;
        input.reserve(inputViews.size());
        for (auto& arg : inputViews) {
            input.push_back(arg[i]);
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
                // no output: applied for its side effects only, cellfun returns nothing.
                noOutput = true;
                continue;
            }
            Error(L"Nelson:data_structures:functionReturnedFewerOutputsExpected",
                L"function returned fewer outputs than expected");
        }
        if (i == 0) {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!ret[j].isScalar()) {
                    Error(L"Nelson:data_structures:functionReturnedNonScalarResult",
                        L"function returned non-scalar result");
                }
                outputs.push_back(ret[j]);
                outputs[j].resize(argdims);
            }
        } else {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!ret[j].isScalar()) {
                    Error(L"Nelson:data_structures:functionReturnedNonScalarResult",
                        L"function returned non-scalar result");
                }
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
static ArrayOfVector
isempty_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isEmpty();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.dimensions(), matLogical);
    return retval;
}
//=============================================================================
static ArrayOfVector
islogical_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isLogical();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.dimensions(), matLogical);
    return retval;
}
//=============================================================================
static ArrayOfVector
isreal_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].allReal();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.dimensions(), matLogical);
    return retval;
}
//=============================================================================
static ArrayOfVector
length_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            double len = 0;
            Dimensions sze(arg[k].dimensions());
            for (indexType i = 0; i < sze.length(); i++) {
                if (static_cast<double>(sze[i]) == 0) {
                    len = 0;
                    break;
                }
                if (i == 0) {
                    len = static_cast<double>(sze[i]);
                } else {
                    if (static_cast<double>(sze[i]) > len) {
                        len = static_cast<double>(sze[i]);
                    }
                }
            }
            matDouble[k] = len;
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.dimensions(), matDouble);
    return retval;
}
//=============================================================================
static ArrayOfVector
ndims_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            indexType len = arg[k].nDims();
            if (len < 2) {
                len = 2;
            }
            matDouble[k] = static_cast<double>(len);
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.dimensions(), matDouble);
    return retval;
}
//=============================================================================
static ArrayOfVector
prodofsize_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            matDouble[k] = static_cast<double>(arg[k].elementCount());
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.dimensions(), matDouble);
    return retval;
}
//=============================================================================
static ArrayOfVector
size_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param3 = argIn[2];
    indexType idx = param3.asScalarIndex(false);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            if (idx - 1 >= maxDims) {
                matDouble[k] = (1.0);
            } else {
                Dimensions sze(arg[k].dimensions());
                matDouble[k] = static_cast<double>(sze[idx - 1]);
            }
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.dimensions(), matDouble);
    return retval;
}
//=============================================================================
static ArrayOfVector
isclass_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param3 = argIn[2];
    std::wstring classExpected = param3.asWideString();
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.elementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.elementCount(), stringVector(), false));
    if (nbElements > 0) {
        ArrayOfView<ArrayOf> arg(Cell, ArrayOfViewAssumeValidated {});
        for (indexType k = 0; k < nbElements; k++) {
            std::wstring currentClass;
            ClassName(arg[k], currentClass);
            matLogical[k] = (currentClass == classExpected);
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.dimensions(), matLogical);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 2);
    if (!argIn[1].isCell()) {
        Error("Nelson:cellfun:InvalidSecondInput", "cellfun works only on cells.");
    }
    int nbElementsInput = static_cast<int>(argIn.size());
    bool bHaveErrorHandlerArgs = false;
    bool bHaveUniformOutputArgs = false;
    bool isUniformOutput = true;
    function_handle errorFunc;
    errorFunc.anonymousHandle = nullptr;
    if (nbElementsInput - 2 > 0) {
        if (argIn[(indexType)nbElementsInput - (indexType)2].isRowVectorCharacterArray()) {
            std::wstring argName = argIn[(indexType)nbElementsInput - (indexType)2].asWideString();
            if (matchesFunctionHandleOption(argName, L"UniformOutput")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)1];
                if (param.isLogical() || param.isNumeric()) {
                    if (param.isScalar()) {
                        bHaveUniformOutputArgs = true;
                        isUniformOutput = param.isLogical() ? (param.asLogicalScalar() ? 1 : 0)
                                                            : (param.asDoubleScalar() != 0.);
                    }
                } else {
                    Error(L"Nelson:data_structures:errorWrongTypeExpected",
                        L"Error wrong type expected.");
                }
            } else if (matchesFunctionHandleOption(argName, L"ErrorHandler")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)1];
                if (param.isFunctionHandle()) {
                    errorFunc = param.asFunctionHandle();
                    bHaveErrorHandlerArgs = true;
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                        (nbElementsInput), L"function handle");
                }
            }
        }
    }
    if (nbElementsInput - 4 > 0) {
        if (argIn[(indexType)nbElementsInput - (indexType)4].isRowVectorCharacterArray()) {
            std::wstring argName = argIn[(indexType)nbElementsInput - (indexType)4].asWideString();
            if (matchesFunctionHandleOption(argName, L"UniformOutput")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)3];
                if (param.isLogical() || param.isNumeric()) {
                    if (param.isScalar()) {
                        if (bHaveUniformOutputArgs) {
                            Error(L"Nelson:data_structures:errorAlreadyDefined",
                                L"Error already defined.");
                        } else {
                            bHaveUniformOutputArgs = true;
                            isUniformOutput = param.isLogical() ? (param.asLogicalScalar() ? 1 : 0)
                                                                : (param.asDoubleScalar() != 0.);
                        }
                    }
                } else {
                    Error(L"Nelson:data_structures:errorWrongTypeExpected",
                        L"Error wrong type expected.");
                }
            } else if (matchesFunctionHandleOption(argName, L"ErrorHandler")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)3];
                if (param.isFunctionHandle()) {
                    if (bHaveErrorHandlerArgs) {
                        Error(L"Nelson:data_structures:errorAlreadyDefined",
                            L"Error already defined.");
                    } else {
                        errorFunc = param.asFunctionHandle();
                        bHaveErrorHandlerArgs = true;
                    }
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.",
                        (nbElementsInput), L"function handle");
                }
            }
        }
    }
    if (bHaveErrorHandlerArgs) {
        nbElementsInput = nbElementsInput - 2;
    }
    if (bHaveUniformOutputArgs) {
        nbElementsInput = nbElementsInput - 2;
    }
    ArrayOf param1 = argIn[0];
    FunctionDef* funcDef = nullptr;
    bool isFunctionName = param1.isRowVectorCharacterArray() || param1.isScalarStringArray();
    if (!(isFunctionName || param1.isFunctionHandle())) {
        Error(L"Nelson:data_structures:wrongType1", L"wrong type #%d", 1);
    } else {
        if (isFunctionName) {
            std::wstring functionName = param1.asWideString();
            if (functionName == L"isempty") {
                return isempty_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"islogical") {
                return islogical_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"isreal") {
                return isreal_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"length") {
                return length_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"ndims") {
                return ndims_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"prodofsize") {
                return prodofsize_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"size") {
                return size_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (functionName == L"isclass") {
                return isclass_cellfunBuiltin(eval, nLhs, argIn);
            }
            if (!eval->getContext()->lookupFunction(functionName, funcDef)) {
                Error(L"Nelson:data_structures:validFunctionNameExpected",
                    L"A valid function name expected.");
            }
        } else {
            function_handle fh = param1.asFunctionHandle();
            if (fh.anonymousHandle != nullptr) {
                funcDef = (FunctionDef*)fh.anonymousHandle;
            }
            if (funcDef == nullptr) {
                Error(L"Nelson:data_structures:validFunctionNameExpected",
                    L"A valid function name expected.");
            }
        }
    }
    Dimensions dimsCells;
    for (int k = 1; k < nbElementsInput; k++) {
        ArrayOf param = argIn[(indexType)k];
        if (param.isCell()) {
            if (k == 1) {
                dimsCells = param.dimensions();
            } else {
                Dimensions dimsCurrentCell = param.dimensions();
                if (!dimsCells.equals(dimsCurrentCell)) {
                    Error(L"Nelson:data_structures:sameSizeExpected", L"Same size expected.");
                }
            }
        } else {
            Error(L"Nelson:data_structures:cellExpected", L"cell expected.");
        }
    }
    int nargout = nLhs;
    if (nargout == 0) {
        nargout = 1;
    }
    indexType nargin = argIn.size();
    if (bHaveErrorHandlerArgs) {
        nargin -= 2;
    }
    if (bHaveUniformOutputArgs) {
        nargin -= 2;
    }
    FunctionDefPtr fptrHandleError = nullptr;
    if (errorFunc.anonymousHandle) {
        fptrHandleError = reinterpret_cast<FunctionDef*>(errorFunc.anonymousHandle);
    }
    if (isUniformOutput) {
        retval = cellfun_uniformBuiltin(
            nargout, nLhs == 0, argIn, eval, dimsCells, nargin, funcDef, fptrHandleError);
    } else {
        retval = cellfun_nonuniformBuiltin(
            nargout, argIn, eval, dimsCells, nargin, funcDef, fptrHandleError);
    }
    return retval;
}
//=============================================================================
