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
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "ErrorToStruct.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "omp_for_loop.hpp"
#include "NewWithException.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
arrayfun_nonuniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    const Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType nbElements = argdims.getElementCount();
    for (int j = 0; j < nargout; j++) {
        ArrayOf* elements = nullptr;
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            raiseError(L"nelson:runtime:outOfMemory", ERROR_MEMORY_ALLOCATION);
        }
        for (indexType k = 0; k < nbElements; ++k) {
            elements[k] = ArrayOf::emptyConstructor();
        }
        ArrayOf arrayResult(NLS_CELL_ARRAY, argdims, elements);
        outputs.push_back(arrayResult);
    }

    for (indexType i = 0; i < nbElements; i++) {
        ArrayOfVector input;
        for (indexType j = 1; j < argcount; j++) {
            ArrayOf element = argIn[j];
            if (element.isSparse()) {
                raiseError(L"Nelson:data_structures:ERROR_SPARSE_INPUTS_NOT_SUPPORTED",
                    ERROR_SPARSE_INPUTS_NOT_SUPPORTED);
            }
            input.push_back(element.getValueAtIndex(i));
        }

        ArrayOfVector ret;
        if (fptrHandleError) {
            try {
                ret = fptr->evaluateFunction(eval, input, nargout);
            } catch (Exception& e) {
                ArrayOfVector in2;
                in2.push_back(ErrorToStruct(e));
                in2 += input;
                ret = fptrHandleError->evaluateFunction(eval, in2, nargout);
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
        }

        if ((int)ret.size() < nargout) {
            raiseError(
                L"Nelson:data_structures:ERROR_FUNCTION_RETURNED_FEWER_OUTPUTS_THAN_EXPECTED",
                ERROR_FUNCTION_RETURNED_FEWER_OUTPUTS_THAN_EXPECTED);
        }

        for (indexType j = 0; j < (indexType)nargout; j++) {
            auto* output = (ArrayOf*)(outputs[j].getDataPointer());
            output[i] = ret[j];
        }
    }
    return outputs;
}
//=============================================================================
static ArrayOfVector
arrayfun_uniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    Dimensions& argdims, indexType argcount, FunctionDefPtr fptr, FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType nbElements = argdims.getElementCount();
    for (indexType i = 0; i < nbElements; i++) {
        ArrayOfVector input;
        for (indexType j = 1; j < argcount; j++) {
            ArrayOf element = argIn[j];
            if (element.isSparse()) {
                raiseError(L"Nelson:data_structures:ERROR_SPARSE_INPUTS_NOT_SUPPORTED",
                    ERROR_SPARSE_INPUTS_NOT_SUPPORTED);
            }
            input.push_back(element.getValueAtIndex(i));
        }
        ArrayOfVector ret;
        if (fptrHandleError) {
            try {
                ret = fptr->evaluateFunction(eval, input, nargout);
            } catch (Exception& e) {
                ArrayOfVector in2;
                in2.push_back(ErrorToStruct(e));
                in2 += input;
                ret = fptrHandleError->evaluateFunction(eval, in2, nargout);
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
        }

        if ((int)ret.size() < nargout) {
            raiseError(
                L"Nelson:data_structures:ERROR_FUNCTION_RETURNED_FEWER_OUTPUTS_THAN_EXPECTED",
                ERROR_FUNCTION_RETURNED_FEWER_OUTPUTS_THAN_EXPECTED);
        }

        if (i == 0) {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!ret[j].isScalar()) {
                    raiseError(L"Nelson:data_structures:ERROR_NON_SCALAR_IN_UNIFORM_OUTPUT",
                        ERROR_NON_SCALAR_IN_UNIFORM_OUTPUT);
                }
                outputs.push_back(ret[j]);
                outputs[j].resize(argdims);
            }
        } else {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!outputs[j].isComplex() && ret[j].isComplex()) {
                    outputs[j].promoteType(ret[j].getDataClass());
                } else if (outputs[j].isComplex() && !ret[j].isComplex()) {
                    ret[j].promoteType(outputs[j].getDataClass());
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
            return arg.getContentAsLogicalScalar();
        }
        if (arg.isDoubleClass()) {
            return (bool)arg.getContentAsDoubleScalar();
        }
    }
    raiseError(L"Nelson:data_structures:ERROR_UNIFORMOUTPUT_MUST_BE_SCALAR_LOGICAL",
        ERROR_UNIFORMOUTPUT_MUST_BE_SCALAR_LOGICAL);
    return false;
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::arrayfunBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 2);
    int nbInputs = static_cast<int>(argIn.size());

    bool isUniform = true;
    bool hasUniformArg = false;
    bool hasErrorHandler = false;
    function_handle errorHandler;
    errorHandler.anonymousHandle = nullptr;

    if (nbInputs >= 4
        && (argIn[nbInputs - 2].isRowVectorCharacterArray()
            || argIn[nbInputs - 2].isScalarStringArray())) {
        std::wstring key = argIn[nbInputs - 2].getContentAsWideString();
        if (StringHelpers::iequals(key, L"UniformOutput")) {
            isUniform = getUniformOutputStatus(argIn[nbInputs - 1]);
            hasUniformArg = true;
        } else if (StringHelpers::iequals(key, L"ErrorHandler")) {
            if (argIn[nbInputs - 1].isFunctionHandle()) {
                errorHandler = argIn[nbInputs - 1].getContentAsFunctionHandle();
                hasErrorHandler = true;
            } else {
                raiseError(L"Nelson:data_structures:ERROR_ERRORHANDLER_MUST_BE_FUNCTION_HANDLE",
                    ERROR_ERRORHANDLER_MUST_BE_FUNCTION_HANDLE);
            }
        }
    }

    if (hasUniformArg) {
        nbInputs -= 2;
    }
    if (hasErrorHandler) {
        nbInputs -= 2;
    }

    if (nbInputs < 2) {
        raiseError(L"Nelson:data_structures:ERROR_FUNCTION_HANDLE_AND_AT_LEAST_ONE_ARRAY_REQUIRED",
            ERROR_FUNCTION_HANDLE_AND_AT_LEAST_ONE_ARRAY_REQUIRED);
    }

    FunctionDef* funcDef = nullptr;
    ArrayOf funcInput = argIn[0];
    if (funcInput.isFunctionHandle()) {
        function_handle fh = funcInput.getContentAsFunctionHandle();
        funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    } else if (funcInput.isRowVectorCharacterArray()) {
        std::wstring funcName = funcInput.getContentAsWideString();
        if (!eval->getContext()->lookupFunction(funcName, funcDef)) {
            raiseError(
                L"Nelson:data_structures:ERROR_UNKNOWN_FUNCTION_NAME", ERROR_UNKNOWN_FUNCTION_NAME);
        }
    } else {
        raiseError(L"Nelson:data_structures:ERROR_INVALID_FUNCTION_SPECIFICATION",
            ERROR_INVALID_FUNCTION_SPECIFICATION);
    }

    if (funcDef == nullptr) {
        raiseError2(L"nelson:validators:mustBeValidFunctionHandle");
    }

    // validate input dimensions
    std::vector<indexType> dimsVector;
    for (size_t i = 1; i < nbInputs; ++i) {
        const ArrayOf& in = argIn[i];
        std::vector<indexType> inputDims = in.getDimensions().getAsVector();
        if (inputDims.empty()) {
            continue;
        }

        if (dimsVector.empty()) {
            dimsVector.insert(dimsVector.end(), inputDims.begin(), inputDims.end());
        } else {
            if (dimsVector.size() != inputDims.size()) {
                raiseError(
                    L"Nelson:data_structures:ERROR_SAME_SIZE_EXPECTED", ERROR_SAME_SIZE_EXPECTED);
            }
            for (size_t i = 0; i < dimsVector.size(); ++i) {
                if (inputDims[i] != dimsVector[i]) {
                    raiseError(L"Nelson:data_structures:ERROR_SAME_SIZE_EXPECTED",
                        ERROR_SAME_SIZE_EXPECTED);
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

    if (isUniform) {
        retval = arrayfun_uniformBuiltin(nargout, argIn, eval, dims, nbInputs, funcDef, fptrError);
    } else {
        retval
            = arrayfun_nonuniformBuiltin(nargout, argIn, eval, dims, nbInputs, funcDef, fptrError);
    }
    return retval;
}
//=============================================================================
