//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "cellfunBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorToStruct.hpp"
#include "PathFuncManager.hpp"
#include "StringFormat.hpp"
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
cellfun_nonuniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    const Dimensions& argdims, indexType argcount, FuncPtr fptr, FuncPtr fptrHandleError)
{
    ArrayOfVector outputs;
    for (int j = 0; j < nargout; j++) {
        ArrayOf* elements = nullptr;
        size_t nbElements = argdims.getElementCount();
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::emptyConstructor();
        }
        ArrayOf c = ArrayOf(NLS_CELL_ARRAY, argdims, elements);
        outputs.push_back(c);
    }
    for (int i = 0; i < argdims.getElementCount(); i++) {
        ArrayOfVector input;
        for (int j = 1; j < argcount; j++) {
            ArrayOf* arg = (ArrayOf*)(argIn[j].getDataPointer());
            input.push_back(arg[i]);
        }
        ArrayOfVector ret;
        if (fptrHandleError) {
            try {
                ret = fptr->evaluateFunction(eval, input, nargout);
            } catch (Exception& e) {
                ArrayOfVector in2;
                in2.push_back(ErrorToStruct(e));
                for (size_t k = 0; k < input.size(); k++) {
                    in2.push_back(input[k]);
                }
                ret = fptrHandleError->evaluateFunction(eval, in2, nargout);
            }
            if (ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
            if (ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        }
        for (int j = 0; j < nargout; j++) {
            ArrayOf* arg = (ArrayOf*)(outputs[j].getDataPointer());
            arg[i] = ret[j];
        }
    }
    return outputs;
}
//=============================================================================
static ArrayOfVector
cellfun_uniformBuiltin(int nargout, const ArrayOfVector& argIn, Evaluator* eval,
    Dimensions& argdims, indexType argcount, FuncPtr fptr, FuncPtr fptrHandleError)
{
    ArrayOfVector outputs;
    for (int i = 0; i < argdims.getElementCount(); i++) {
        ArrayOfVector input;
        for (int j = 1; j < argcount; j++) {
            ArrayOf* arg = (ArrayOf*)(argIn[j].getDataPointer());
            input.push_back(arg[i]);
        }
        ArrayOfVector ret;
        if (fptrHandleError) {
            try {
                ret = fptr->evaluateFunction(eval, input, nargout);
            } catch (Exception& e) {
                ArrayOfVector in2;
                in2.push_back(ErrorToStruct(e));
                for (size_t k = 0; k < input.size(); k++) {
                    in2.push_back(input[k]);
                }
                ret = fptrHandleError->evaluateFunction(eval, in2, nargout);
            }
            if (ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        } else {
            ret = fptr->evaluateFunction(eval, input, nargout);
            if (ret.size() < nargout) {
                Error(_W("function returned fewer outputs than expected"));
            }
        }
        if (i == 0) {
            for (int j = 0; j < nargout; j++) {
                if (!ret[j].isScalar()) {
                    Error(_W("function returned non-scalar result"));
                }
                outputs.push_back(ret[j]);
                outputs[j].resize(argdims);
            }
        } else {
            for (int j = 0; j < nargout; j++) {
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
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    logical* matLogical
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isEmpty();
        }
    }
    retval.push_back(ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical));
    return retval;
}
//=============================================================================
static ArrayOfVector
islogical_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    logical* matLogical
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].isLogical();
        }
    }
    retval.push_back(ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical));
    return retval;
}
//=============================================================================
static ArrayOfVector
isreal_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    logical* matLogical
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matLogical[k] = arg[k].allReal();
        }
    }
    retval.push_back(ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical));
    return retval;
}
//=============================================================================
static ArrayOfVector
length_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    double* matDouble
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            double len = 0;
            Dimensions sze(arg[k].getDimensions());
            for (indexType i = 0; i < sze.getLength(); i++) {
                if ((double)sze[i] == 0) {
                    len = 0;
                    break;
                }
                if (i == 0) {
                    len = (double)sze[i];
                } else {
                    if ((double)sze[i] > len) {
                        len = (double)sze[i];
                    }
                }
            }
            matDouble[k] = len;
        }
    }
    retval.push_back(ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble));
    return retval;
}
//=============================================================================
static ArrayOfVector
ndims_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    double* matDouble
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            indexType len = arg[k].getDimensions().getLength();
            if (len < 2) {
                len = 2;
            }
            matDouble[k] = (double)len;
        }
    }
    retval.push_back(ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble));
    return retval;
}
//=============================================================================
static ArrayOfVector
prodofsize_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    double* matDouble
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            matDouble[k] = (double)arg[k].getDimensions().getElementCount();
        }
    }
    retval.push_back(ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble));
    return retval;
}
//=============================================================================
static ArrayOfVector
size_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param3 = argIn[2];
    indexType idx = param3.getContentAsScalarIndex(false);
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    double* matDouble
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            Dimensions sze(arg[k].getDimensions());
            if (idx - 1 >= maxDims) {
                matDouble[k] = (double)(1.0);
            } else {
                matDouble[k] = (double)(sze[idx - 1]);
            }
        }
    }
    retval.push_back(ArrayOf(NLS_DOUBLE, Cell.getDimensions(), matDouble));
    return retval;
}
//=============================================================================
static ArrayOfVector
isclass_cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param3 = argIn[2];
    std::wstring classExpected = param3.getContentAsWideString();
    ArrayOf Cell = argIn[1];
    indexType nbElements = Cell.getDimensions().getElementCount();
    logical* matLogical
        = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Cell.getDimensions().getElementCount());
    if (nbElements > 0) {
        ArrayOf* arg = (ArrayOf*)(Cell.getDataPointer());
        for (indexType k = 0; k < nbElements; k++) {
            std::wstring currentClass = L"";
            ClassName(arg[k], currentClass);
            if (boost::algorithm::contains(currentClass, classExpected)) {
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
    retval.push_back(ArrayOf(NLS_LOGICAL, Cell.getDimensions(), matLogical));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::cellfunBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int nbElementsInput = (int)argIn.size();
    if (nbElementsInput < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bHaveErrorHandlerArgs = false;
    bool bHaveUniformOutputArgs = false;
    bool isUniformOutput = true;
    function_handle errorFunc = 0;
    if (nbElementsInput - 2 > 0) {
        if (argIn[nbElementsInput - 2].isRowVectorCharacterArray()) {
            std::wstring argName = argIn[nbElementsInput - 2].getContentAsWideString();
            if (argName == L"UniformOutput") {
                if (argIn[nbElementsInput - 1].isLogical()) {
                    if (argIn[nbElementsInput - 1].isScalar()) {
                        bHaveUniformOutputArgs = true;
                        isUniformOutput
                            = (argIn[nbElementsInput - 1].getContentAsLogicalScalar() ? 1 : 0);
                    }
                } else {
                    Error(_W("Error wrong type expected."));
                }
            } else if (argName == L"ErrorHandler") {
                ArrayOf param = argIn[nbElementsInput - 1];
                if (param.isFunctionHandle()) {
                    errorFunc = param.getContentAsFunctionHandle();
                    bHaveErrorHandlerArgs = true;
                } else {
                    Error(StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_FUNCTION_HANDLE_EXPECTED.c_str(),
                        nbElementsInput));
                }
            }
        }
    }
    if (nbElementsInput - 4 > 0) {
        if (argIn[nbElementsInput - 4].isRowVectorCharacterArray()) {
            std::wstring argName = argIn[nbElementsInput - 4].getContentAsWideString();
            if (argName == L"UniformOutput") {
                if (argIn[nbElementsInput - 5].isLogical()) {
                    if (argIn[nbElementsInput - 5].isScalar()) {
                        if (bHaveUniformOutputArgs) {
                            Error(_W("Error already defined."));
                        } else {
                            bHaveUniformOutputArgs = true;
                            isUniformOutput
                                = (argIn[nbElementsInput - 5].getContentAsLogicalScalar() ? 1 : 0);
                        }
                    }
                } else {
                    Error(_W("Error wrong type expected."));
                }
            } else if (argName == L"ErrorHandler") {
                ArrayOf param = argIn[nbElementsInput - 3];
                if (param.isFunctionHandle()) {
                    if (bHaveErrorHandlerArgs) {
                        Error(_W("Error already defined."));
                    } else {
                        errorFunc = param.getContentAsFunctionHandle();
                        bHaveErrorHandlerArgs = true;
                    }
                } else {
                    Error(StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_FUNCTION_HANDLE_EXPECTED.c_str(),
                        nbElementsInput));
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
            if (!PathFuncManager::getInstance()->find(wstring_to_utf8(functionName), funcDef)) {
                if (!BuiltInFunctionDefManager::getInstance()->find(
                        wstring_to_utf8(functionName), funcDef)) {
                    Error(_W("A valid function name expected."));
                }
            }
        } else {
            function_handle fh = param1.getContentAsFunctionHandle();
            std::wstring functionName;
            if (PathFuncManager::getInstance()->find(fh, functionName)) {
                PathFuncManager::getInstance()->find(wstring_to_utf8(functionName), funcDef);
            } else {
                if (BuiltInFunctionDefManager::getInstance()->find(fh, functionName)) {
                    BuiltInFunctionDefManager::getInstance()->find(
                        wstring_to_utf8(functionName), funcDef);
                }
            }
            if (funcDef == nullptr) {
                Error(_W("A valid function name expected."));
            }
        }
    }
    Dimensions dimsCells;
    for (size_t k = 1; k < nbElementsInput; k++) {
        ArrayOf param = argIn[k];
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
    FuncPtr fptrHandleError = nullptr;
    if (errorFunc != 0) {
        std::wstring functionName;
        bool found = PathFuncManager::getInstance()->find(errorFunc, functionName);
        if (found) {
            PathFuncManager::getInstance()->find(wstring_to_utf8(functionName), fptrHandleError);
        } else {
            found = BuiltInFunctionDefManager::getInstance()->find(errorFunc, functionName);
            if (found) {
                BuiltInFunctionDefManager::getInstance()->find(
                    wstring_to_utf8(functionName), fptrHandleError);
            }
        }
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
