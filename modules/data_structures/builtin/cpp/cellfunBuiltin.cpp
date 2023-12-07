//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "cellfunBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ErrorToStruct.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
cellfun_nonuniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    const Dimensions& argdims, indexType argcount, FunctionDefPtr fptr,
    FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    for (int j = 0; j < nargout; j++) {
        ArrayOf* elements = nullptr;
        size_t nbElements = argdims.getElementCount();
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::emptyConstructor();
        }
        ArrayOf c = ArrayOf(NLS_CELL_ARRAY, argdims, elements);
        outputs.push_back(c);
    }
    indexType elementCount = argdims.getElementCount();
    for (indexType i = 0; i < elementCount; i++) {
        ArrayOfVector input;
        for (indexType j = 1; j < argcount; j++) {
            auto* arg = (ArrayOf*)(argIn[j].getDataPointer());
            input.push_back(arg[i]);
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
            if ((int)ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
            if ((int)ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        }
        for (indexType j = 0; j < (indexType)nargout; j++) {
            auto* arg = (ArrayOf*)(outputs[j].getDataPointer());
            arg[i] = ret[j];
        }
    }
    return outputs;
}
//=============================================================================
static ArrayOfVector
cellfun_uniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    Dimensions& argdims, indexType argcount, FunctionDefPtr fptr, FunctionDefPtr fptrHandleError)
{
    ArrayOfVector outputs;
    indexType elementCount = argdims.getElementCount();
    if (nargout == 1 && elementCount == 0) {
        outputs << ArrayOf::emptyConstructor(Dimensions(0, 0));
        return outputs;
    }
    for (indexType i = 0; i < elementCount; i++) {
        ArrayOfVector input;
        for (indexType j = 1; j < argcount; j++) {
            auto* arg = (ArrayOf*)(argIn[j].getDataPointer());
            input.push_back(arg[i]);
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
            if ((int)ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
            if ((int)ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        }
        if (i == 0) {
            for (indexType j = 0; j < (indexType)nargout; j++) {
                if (!ret[j].isScalar()) {
                    Error(_W("function returned non-scalar result"));
                }
                outputs.push_back(ret[j]);
                outputs[j].resize(argdims);
            }
        } else {
            for (indexType j = 0; j < (indexType)nargout; j++) {
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
    indexType nbElements = Cell.getElementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isEmpty();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical);
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
    indexType nbElements = Cell.getElementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isLogical();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical);
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
    indexType nbElements = Cell.getElementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].allReal();
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical);
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
    indexType nbElements = Cell.getElementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            double len = 0;
            Dimensions sze(arg[k].getDimensions());
            for (indexType i = 0; i < sze.getLength(); i++) {
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
    retval << ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble);
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
    indexType nbElements = Cell.getElementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            indexType len = arg[k].nDims();
            if (len < 2) {
                len = 2;
            }
            matDouble[k] = static_cast<double>(len);
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble);
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
    indexType nbElements = Cell.getElementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matDouble[k] = static_cast<double>(arg[k].getElementCount());
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble);
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
    indexType idx = param3.getContentAsScalarIndex(false);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getElementCount();
    double* matDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            if (idx - 1 >= maxDims) {
                matDouble[k] = (1.0);
            } else {
                Dimensions sze(arg[k].getDimensions());
                matDouble[k] = static_cast<double>(sze[idx - 1]);
            }
        }
    }
    retval << ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble);
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
    std::wstring classExpected = param3.getContentAsWideString();
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getElementCount();
    logical* matLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getElementCount(), stringVector(), false));
    if (nbElements > 0) {
        auto* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            std::wstring currentClass;
            ClassName(arg[k], currentClass);
            if (StringHelpers::contains(currentClass, classExpected)) {
                matLogical[k] = true;
            } else {
                if ((classExpected == L"char") && (currentClass == L"string")) {
                    matLogical[k] = true;
                } else {
                    matLogical[k] = false;
                }
            }
        }
    }
    retval << ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 2);
    if (!argIn[1].isCell()) {
        Error(_("cellfun works only on cells."), "Nelson:cellfun:InvalidSecondInput");
    }
    int nbElementsInput = static_cast<int>(argIn.size());
    bool bHaveErrorHandlerArgs = false;
    bool bHaveUniformOutputArgs = false;
    bool isUniformOutput = true;
    function_handle errorFunc;
    errorFunc.anonymousHandle = nullptr;
    if (nbElementsInput - 2 > 0) {
        if (argIn[(indexType)nbElementsInput - (indexType)2].isRowVectorCharacterArray()) {
            std::wstring argName
                = argIn[(indexType)nbElementsInput - (indexType)2].getContentAsWideString();
            if (StringHelpers::iequals(argName, L"UniformOutput")) {
                if (argIn[(indexType)nbElementsInput - (indexType)1].isLogical()) {
                    if (argIn[(indexType)nbElementsInput - (indexType)1].isScalar()) {
                        bHaveUniformOutputArgs = true;
                        isUniformOutput = (argIn[(indexType)nbElementsInput - (indexType)1]
                                               .getContentAsLogicalScalar()
                                ? 1
                                : 0);
                    }
                } else {
                    Error(_W("Error wrong type expected."));
                }
            } else if (StringHelpers::iequals(argName, L"ErrorHandler")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)1];
                if (param.isFunctionHandle()) {
                    errorFunc = param.getContentAsFunctionHandle();
                    bHaveErrorHandlerArgs = true;
                } else {
                    Error(fmt::sprintf(
                        ERROR_WRONG_ARGUMENT_X_TYPE_FUNCTION_HANDLE_EXPECTED, nbElementsInput));
                }
            }
        }
    }
    if (nbElementsInput - 4 > 0) {
        if (argIn[(indexType)nbElementsInput - (indexType)4].isRowVectorCharacterArray()) {
            std::wstring argName
                = argIn[(indexType)nbElementsInput - (indexType)4].getContentAsWideString();
            if (StringHelpers::iequals(argName, L"UniformOutput")) {
                if (argIn[(indexType)nbElementsInput - (indexType)5].isLogical()) {
                    if (argIn[(indexType)nbElementsInput - (indexType)5].isScalar()) {
                        if (bHaveUniformOutputArgs) {
                            Error(_W("Error already defined."));
                        } else {
                            bHaveUniformOutputArgs = true;
                            isUniformOutput = (argIn[(indexType)nbElementsInput - (indexType)5]
                                                   .getContentAsLogicalScalar()
                                    ? 1
                                    : 0);
                        }
                    }
                } else {
                    Error(_W("Error wrong type expected."));
                }
            } else if (StringHelpers::iequals(argName, L"ErrorHandler")) {
                ArrayOf param = argIn[(indexType)nbElementsInput - (indexType)3];
                if (param.isFunctionHandle()) {
                    if (bHaveErrorHandlerArgs) {
                        Error(_W("Error already defined."));
                    } else {
                        errorFunc = param.getContentAsFunctionHandle();
                        bHaveErrorHandlerArgs = true;
                    }
                } else {
                    Error(fmt::sprintf(
                        ERROR_WRONG_ARGUMENT_X_TYPE_FUNCTION_HANDLE_EXPECTED, nbElementsInput));
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
    if (!(param1.isRowVectorCharacterArray() || param1.isFunctionHandle())) {
        Error(_W("wrong type #1"));
    } else {
        if (param1.isRowVectorCharacterArray()) {
            std::wstring functionName = param1.getContentAsWideString();
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
                Error(_W("A valid function name expected."));
            }
        } else {
            function_handle fh = param1.getContentAsFunctionHandle();
            if (fh.anonymousHandle != nullptr) {
                funcDef = (FunctionDef*)fh.anonymousHandle;
            }
            if (funcDef == nullptr) {
                Error(_W("A valid function name expected."));
            }
        }
    }
    Dimensions dimsCells;
    for (int k = 1; k < nbElementsInput; k++) {
        ArrayOf param = argIn[(indexType)k];
        if (param.isCell()) {
            if (k == 1) {
                dimsCells = param.getDimensions();
            } else {
                Dimensions dimsCurrentCell = param.getDimensions();
                if (!dimsCells.equals(dimsCurrentCell)) {
                    Error(ERROR_SAME_SIZE_EXPECTED);
                }
            }
        } else {
            Error(_W("cell expected."));
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
            nargout, argIn, eval, dimsCells, nargin, funcDef, fptrHandleError);
    } else {
        retval = cellfun_nonuniformBuiltin(
            nargout, argIn, eval, dimsCells, nargin, funcDef, fptrHandleError);
    }
    return retval;
}
//=============================================================================
