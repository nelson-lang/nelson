//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <functional>
#include "PythonObjectHandle.hpp"
#include "PythonLibraryWrapper.hpp"
#include "characters_encoding.hpp"
#include "PyObjectHelpers.hpp"
#include "i18n.hpp"
#include "PyObjectHelpers.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonEngine.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
PythonObjectHandle::invoke(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    if (isMainPythonInterpreter() && invokeFunction(methodName, inputs, nLhs, results)) {
        return true;
    }
    if (invokeCastMethod(methodName, results)) {
        return true;
    }
    if (invokeOperatorMethod(methodName, inputs, results)) {
        return true;
    }
    switch (inputs.size()) {
    case 0: {
        return invokeMethodNoArgument(methodName, inputs, nLhs, results);
    } break;
    case 1: {
        return invokeMethodOneArgument(methodName, inputs, nLhs, results);
    } break;
    default: {
        return invokeMethodMultipleArguments(methodName, inputs, nLhs, results);

    } break;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeFunction(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{

    PyObject* importModule = NLSPyImport_ImportModule(wstring_to_utf8(methodName).c_str());
    if (importModule) {
        bool needDeReference;
        results << PyObjectToArrayOf(importModule, needDeReference);
        return true;
    }
    PyObject* list_func
        = NLSPyDict_GetItemString(NLSPyEval_GetBuiltins(), wstring_to_utf8(methodName).c_str());

    if (list_func && NLSPyCallable_Check(list_func)) {
        bool needDeReference;
        results << PyObjectToArrayOf(list_func, needDeReference);
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethodNoArgument(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();

    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
    if (!method || !NLSPyCallable_Check(method)) {
        NLSPy_XDECREF(method);
        Error(_W("Method does not exist."));
        return false;
    }

    PyObject* pyObjectResult = NLSPyObject_CallNoArgs(method);
    NLSPy_DECREF(method);
    if (pyObjectResult) {
        bool needDecreaseReference;
        results << PyObjectToArrayOf(pyObjectResult, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(pyObjectResult);
        }
    } else {
        std::wstring errorMessage = _W("Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage);
    }
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethodOneArgument(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();

    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
    if (!method || !NLSPyCallable_Check(method)) {
        NLSPy_XDECREF(method);
        Error(_W("Method does not exist."));
        return false;
    }

    NLSPy_XDECREF(method);
    PyObject* arg1 = arrayOfToPyObject(inputs[0]);
    if (!arg1) {
        Error(_W("Failed to convert input argument to Python object."));
        return false;
    }
    PyObject* nameMethod = NLSPyUnicode_FromString(wstring_to_utf8(methodName).c_str());
    PyObject* pyObjectResult = NLSPyObject_CallMethodOneArg(pyObject, nameMethod, arg1);

    NLSPy_DECREF(nameMethod);
    NLSPy_DECREF(arg1);

    if (pyObjectResult) {
        bool needDecreaseReference;
        results << PyObjectToArrayOf(pyObjectResult, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(pyObjectResult);
        }
    } else {
        std::wstring errorMessage = _W("Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage);
    }
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethodMultipleArguments(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)

{
    PyObject* pyObject = (PyObject*)this->getPointer();

    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
    if (!method || !NLSPyCallable_Check(method)) {
        NLSPy_XDECREF(method);
        Error(_W("Method does not exist."));
        return false;
    }

    NLSPy_XDECREF(method);
    PyObject* nameMethod = NLSPyUnicode_FromString(wstring_to_utf8(methodName).c_str());
    PyObject** args = new PyObject*[inputs.size() + 1];
    args[0] = pyObject;
    for (size_t k = 0; k < inputs.size(); ++k) {
        args[k + 1] = arrayOfToPyObject(inputs[k]);
    }
    size_t nargsf = (inputs.size() + 1) | PY_VECTORCALL_ARGUMENTS_OFFSET;
    PyObject* pyObjectResult = NLSPyObject_VectorcallMethod(nameMethod, args, nargsf, NULL);
    NLSPy_XDECREF(nameMethod);
    for (size_t k = 0; k < inputs.size(); ++k) {
        NLSPy_DECREF(args[k + 1]);
    }
    delete[] args;
    if (pyObjectResult) {
        bool needDecreaseReference;
        results << PyObjectToArrayOf(pyObjectResult, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(pyObjectResult);
        }
    } else {
        std::wstring errorMessage = _W("Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage);
    }
    return true;
}
//=============================================================================
}
//=============================================================================
