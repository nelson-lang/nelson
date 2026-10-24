//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <functional>
#include "PythonObjectHandle.hpp"
#include "PythonLibraryWrapper.hpp"
#include "characters_encoding.hpp"
#include "PyObjectHelpers.hpp"
#include "i18n.hpp"
#include "PythonTypesWrapper.hpp"
#include "PythonEngine.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "PyKeyMatch.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isPyargs(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == "pyargs";
}
//=============================================================================
static bool
isKeywordName(const ArrayOf& value, std::string& name)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        name = wstring_to_utf8(value.asWideString());
        return !name.empty();
    }
    return false;
}
//=============================================================================
static void
appendPyargsKeywords(const ArrayOf& value, stringVector& keywordNames, ArrayOfVector& keywordValues)
{
    ArrayOf arguments = value.field("Arguments");
    stringVector names = arguments.fieldNames();
    for (const auto& name : names) {
        keywordNames.push_back(name);
        keywordValues.push_back(arguments.field(name));
    }
}
//=============================================================================
static bool
splitPythonArguments(const ArrayOfVector& inputs, ArrayOfVector& positionalValues,
    stringVector& keywordNames, ArrayOfVector& keywordValues, bool convertStringPairs)
{
    bool haveKeywords = false;
    for (size_t k = 0; k < inputs.size(); ++k) {
        const auto& input = inputs[k];
        if (isPyargs(input)) {
            appendPyargsKeywords(input, keywordNames, keywordValues);
            haveKeywords = true;
        } else if (convertStringPairs && k + 1 < inputs.size()) {
            std::string name;
            if (isKeywordName(input, name)) {
                keywordNames.push_back(name);
                keywordValues.push_back(inputs[k + 1]);
                haveKeywords = true;
                ++k;
            } else {
                positionalValues.push_back(input);
            }
        } else {
            positionalValues.push_back(input);
        }
    }
    return haveKeywords;
}
//=============================================================================
static bool
hasStringPairKeywords(const ArrayOfVector& inputs)
{
    for (size_t k = 0; k + 1 < inputs.size(); ++k) {
        std::string name;
        if (isKeywordName(inputs[k], name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static PyObject*
keywordNamesToDict(const stringVector& keywordNames, const ArrayOfVector& keywordValues)
{
    if (keywordNames.empty()) {
        return nullptr;
    }
    PyObject* dict = NLSPyDict_New();
    for (size_t k = 0; k < keywordNames.size(); ++k) {
        PyObject* value = arrayOfToPyObject(keywordValues[k]);
        NLSPyDict_SetItemString(dict, keywordNames[k].c_str(), value);
        NLSPy_DECREF(value);
    }
    return dict;
}
//=============================================================================
static PyObject*
valuesToTuple(const ArrayOfVector& values)
{
    PyObject* tuple = NLSPyTuple_New(static_cast<Py_ssize_t>(values.size()));
    for (size_t k = 0; k < values.size(); ++k) {
        NLSPyTuple_SetItem(tuple, static_cast<Py_ssize_t>(k), arrayOfToPyObject(values[k]));
    }
    return tuple;
}
//=============================================================================
static void
flushPythonOutput(Interface* io)
{
    if (io) {
        std::wstring outputMessage = getPythonStandardOutput();
        if (!outputMessage.empty()) {
            io->outputMessage(outputMessage);
        }
        std::wstring errorMessage = getPythonStandardError();
        if (!errorMessage.empty()) {
            io->outputMessage(errorMessage);
        }
    }
}
//=============================================================================
static void
appendPythonResult(Interface* io, PyObject* pyObjectResult, ArrayOfVector& results)
{
    if (pyObjectResult) {
        flushPythonOutput(io);
        bool needDecreaseReference;
        results << PyObjectToArrayOf(pyObjectResult, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(pyObjectResult);
        }
    } else {
        std::wstring errorMessage = resolveMessageTemplate(
            L"Nelson:python_engine:errorCallingMethod", L"Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(L"Nelson:python:invokeFailed", errorMessage);
    }
}
//=============================================================================
static PyObject*
callPythonMethodWithArguments(
    PyObject* method, const ArrayOfVector& inputs, bool convertStringPairs, bool& haveKeywords)
{
    ArrayOfVector positionalValues;
    stringVector keywordNames;
    ArrayOfVector keywordValues;
    haveKeywords = splitPythonArguments(
        inputs, positionalValues, keywordNames, keywordValues, convertStringPairs);

    PyObject* args = valuesToTuple(positionalValues);
    PyObject* kwargs = keywordNamesToDict(keywordNames, keywordValues);
    PyObject* pyObjectResult = NLSPyObject_Call(method, args, kwargs);
    NLSPy_XDECREF(kwargs);
    NLSPy_DECREF(args);
    return pyObjectResult;
}
//=============================================================================
// Tells a binding failure (wrong number or kind of positional arguments: a TypeError with
// no traceback, since no frame was entered) from an exception raised inside the callable
// body. Only a binding failure may trigger the name/value keyword retry: retrying after
// the body ran would hide the real exception. The pending exception is fetched and
// restored unchanged.
static bool
isPositionalBindingError()
{
    if (!NLSPyErr_Occurred()) {
        return false;
    }
    PyObject* type = nullptr;
    PyObject* value = nullptr;
    PyObject* traceback = nullptr;
    NLSPyErr_Fetch(&type, &value, &traceback);
    bool bindingError = (type != nullptr) && (traceback == nullptr)
        && (NLSPyErr_GivenExceptionMatches(type, NLSPyExc_TypeError()) != 0);
    NLSPyErr_Restore(type, value, traceback);
    return bindingError;
}
//=============================================================================
static PyObject*
callPythonMethodWithArguments(
    PyObject* pyObject, const std::wstring& methodName, const ArrayOfVector& inputs)
{
    PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
    if (!method || !NLSPyCallable_Check(method)) {
        NLSPy_XDECREF(method);
        Error(L"Nelson:Python:methodNotFound", L"Method does not exist.");
    }

    bool haveKeywords = false;
    PyObject* pyObjectResult = callPythonMethodWithArguments(method, inputs, false, haveKeywords);
    if (!pyObjectResult && !haveKeywords && hasStringPairKeywords(inputs)
        && isPositionalBindingError()) {
        NLSPyErr_Clear();
        pyObjectResult = callPythonMethodWithArguments(method, inputs, true, haveKeywords);
    }
    NLSPy_DECREF(method);
    return pyObjectResult;
}
//=============================================================================
bool
PythonObjectHandle::invoke(Interface* io, const std::wstring& methodName,
    const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
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
    if (methodName == L"keyHash") {
        uint64 hashValue = 0;
        if (invokeHashMethod(hashValue)) {
            results << ArrayOf::uint64Scalar(hashValue);
            return true;
        }
    }
    if (methodName == L"keyMatch") {
        PyObject* pyObjectA = (PyObject*)this->pointer();
        if (inputs.size() < 1) {
            Error("Nelson:notEnoughInputs", "Not enough input arguments.");
        }
        if (inputs.size() > 1) {
            Error("Nelson:tooManyInputs", "Too many input arguments.");
        }
        PythonObjectHandle* pyObjectB = (PythonObjectHandle*)inputs[0].asHandleScalar();
        PyObject* p2 = (PyObject*)pyObjectB->pointer();
        results << ArrayOf::logicalScalar(PyKeyMatch(pyObjectA, p2));
        return true;
    }

    switch (inputs.size()) {
    case 0: {
        return invokeMethodNoArgument(io, methodName, inputs, nLhs, results);
    } break;
    case 1: {
        return invokeMethodOneArgument(io, methodName, inputs, nLhs, results);
    } break;
    default: {
        return invokeMethodMultipleArguments(io, methodName, inputs, nLhs, results);

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
        // PyImport_ImportModule returns a new reference. PyObjectToArrayOf borrows it or
        // takes its own INCREF, so the reference owned here is released once.
        NLSPy_DECREF(importModule);
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
PythonObjectHandle::invokeMethodNoArgument(Interface* io, const std::wstring& methodName,
    const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->pointer();

    if (!pyObject) {
        Error(L"Nelson:Python:invalidObject", L"Invalid Python object.");
    }
    PyObject* pyObjectResult = callPythonMethodWithArguments(pyObject, methodName, inputs);
    appendPythonResult(io, pyObjectResult, results);
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethodOneArgument(Interface* io, const std::wstring& methodName,
    const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->pointer();

    if (!pyObject) {
        Error(L"Nelson:Python:invalidObject", L"Invalid Python object.");
    }
    appendPythonResult(io, callPythonMethodWithArguments(pyObject, methodName, inputs), results);
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethodMultipleArguments(Interface* io, const std::wstring& methodName,
    const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->pointer();

    if (!pyObject) {
        Error(L"Nelson:Python:invalidObject", L"Invalid Python object.");
    }
    appendPythonResult(io, callPythonMethodWithArguments(pyObject, methodName, inputs), results);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
