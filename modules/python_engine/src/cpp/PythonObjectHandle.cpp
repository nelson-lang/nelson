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
#include "PythonTypesWrapper.hpp"
#include "PythonEngine.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PythonObjectHandle::PythonObjectHandle(void* _ptr)
    : HandleGenericObject(NLS_HANDLE_PYOBJECT_CATEGORY_STR, _ptr, true)
{
    methodMap = { { L"cell",
                      std::bind(
                          &PythonObjectHandle::invokeCastCellMethod, this, std::placeholders::_1) },
        { L"struct",
            std::bind(&PythonObjectHandle::invokeCastStructMethod, this, std::placeholders::_1) },
        { L"numeric",
            std::bind(&PythonObjectHandle::invokeCastNumericMethod, this, std::placeholders::_1) },
        { L"char",
            std::bind(&PythonObjectHandle::invokeCastCharMethod, this, std::placeholders::_1) },
        { L"string",
            std::bind(&PythonObjectHandle::invokeCastStringMethod, this, std::placeholders::_1) },
        { L"double",
            std::bind(&PythonObjectHandle::invokeCastDoubleMethod, this, std::placeholders::_1) },
        { L"single",
            std::bind(&PythonObjectHandle::invokeCastSingleMethod, this, std::placeholders::_1) },
        { L"logical",
            std::bind(&PythonObjectHandle::invokeCastLogicalMethod, this, std::placeholders::_1) },
        { L"int8",
            std::bind(&PythonObjectHandle::invokeCastInt8Method, this, std::placeholders::_1) },
        { L"int16",
            std::bind(&PythonObjectHandle::invokeCastInt16Method, this, std::placeholders::_1) },
        { L"int32",
            std::bind(&PythonObjectHandle::invokeCastInt32Method, this, std::placeholders::_1) },
        { L"int64",
            std::bind(&PythonObjectHandle::invokeCastInt64Method, this, std::placeholders::_1) },
        { L"uint8",
            std::bind(&PythonObjectHandle::invokeCastUInt8Method, this, std::placeholders::_1) },
        { L"uint16",
            std::bind(&PythonObjectHandle::invokeCastUInt16Method, this, std::placeholders::_1) },
        { L"uint32",
            std::bind(&PythonObjectHandle::invokeCastUInt32Method, this, std::placeholders::_1) },
        { L"uint64",
            std::bind(&PythonObjectHandle::invokeCastUInt64Method, this, std::placeholders::_1) } };

    operatorMap = { { L"mtimes", L"__mul__" }, { L"mrdivide", L"__truediv__" },
        { L"mpower", L"__pow__" }, { L"gt", L"__gt__" }, { L"ge", L"__ge__" }, { L"le", L"__le__" },
        { L"ne", L"__ne__" }, { L"lt", L"__lt__" }, { L"plus", L"__add__" },
        { L"minus", L"__sub__" }, { L"eq", L"__eq__" }, { L"mod", L"__mod__" },
        { L"uplus", L"__pos__" }, { L"uminus", L"__neg__" } };
}
//=============================================================================
PythonObjectHandle::~PythonObjectHandle() { }
//=============================================================================
bool
PythonObjectHandle::isMainPythonInterpreter()
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return pyObject == (PyObject*)getPythonInterpreter();
}
//=============================================================================
void
PythonObjectHandle::display(Interface* io)
{
    NLSPyErr_Clear();
    std::wstring pyObjTypeName = getTypeName();
    std::wstring strFormat;

    PyObject* pyObject = (PyObject*)this->getPointer();
    switch (getPythonType(pyObject)) {
    case PY_DICT_TYPE:
    case PY_STR_TYPE:
    case PY_NONE_TYPE: {
        strFormat = _W("  Python %s with no properties.");
    } break;
    case PY_LIST_TYPE:
    case PY_LONG_TYPE: {
        strFormat = _W("  Python %s with values:");
    } break;
    case PY_FLOAT_TYPE:
    case PY_BOOL_TYPE:
    case PY_COMPLEX_TYPE:
    case PY_BYTES_TYPE:
    case PY_BYTE_ARRAY_TYPE:
    case PY_MEMORY_VIEW_TYPE:
    case PY_TUPLE_TYPE:
    case PY_ARRAY_ARRAY_TYPE:
    case PY_NUMPY_TYPE:
    case PY_NOT_MANAGED:
    default: {
        strFormat = _W("  Python %s:");
    } break;
    }

    std::wstring msg = fmt::sprintf(strFormat, pyObjTypeName);
    msg.append(L"\n");
    msg.append(L"\n");
    io->outputMessage(msg);
    std::wstring rep = PyObjectToStringRepresentation(pyObject);
    io->outputMessage(std::wstring(L"    ") + rep + L"\n");
}
//=============================================================================
std::wstring
PythonObjectHandle::getTypeName()
{
    return TypeName(((PyObject*)this->getPointer()));
}
//=============================================================================
std::string
PythonObjectHandle::getClassName()
{
    std::wstring _typename = getTypeName();
    if (!_typename.empty()) {
        return NLS_HANDLE_PYOBJECT_CATEGORY_STR + std::string(".") + wstring_to_utf8(_typename);
    }
    return NLS_HANDLE_PYOBJECT_CATEGORY_STR;
}
//=============================================================================
bool
PythonObjectHandle::isOperatorMethod(const std::wstring& methodName)
{
    wstringVector methodOperatorNames = getOperatorMethods();
    auto it = std::find(methodOperatorNames.begin(), methodOperatorNames.end(), methodName);
    return (it != methodOperatorNames.end());
}
//=============================================================================
bool
PythonObjectHandle::isMethod(const std::wstring& methodName)
{
    if (isMainPythonInterpreter()) {
        return true;
    }
    if (isCastMethod(methodName)) {
        return true;
    }
    if (isOperatorMethod(methodName)) {
        return true;
    }
    if (methodName == L"keyHash") {
        return true;
    }
    if (methodName == L"keyMatch") {
        return true;
    }
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
        NLSPyErr_Clear();
        if (!method) {
            return false;
        }
        bool callable = method && NLSPyCallable_Check(method);
        if (method) {
            NLSPy_DECREF(method);
        }
        return callable;
    }
    return false;
}
//=============================================================================
wstringVector
PythonObjectHandle::getOperatorMethods()
{
    if (!methodOperatorNames.empty()) {
        return methodOperatorNames;
    }

    PyObject* pyObject = (PyObject*)this->getPointer();
    wstringVector pythonMethodNames = getPyObjectMethods(pyObject, true);

    for (const auto& pair : operatorMap) {
        for (const auto& name : pythonMethodNames) {
            if (name == pair.second) {
                methodOperatorNames.push_back(pair.first);
            }
        }
    }
    return methodOperatorNames;
}
//=============================================================================
wstringVector
PythonObjectHandle::getCastMethods()
{
    if (!methodCastNames.empty()) {
        return methodCastNames;
    }

    methodCastNames.push_back(L"char");

    PyObject* pyObject = (PyObject*)this->getPointer();
    switch (getPythonType(pyObject)) {
    case PY_STR_TYPE: {
        methodCastNames.push_back(L"string");
        return methodCastNames;

    } break;
    case PY_BYTES_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"double", L"single", L"logical", L"int8", L"uint8", L"int16", L"uint16", L"int32",
                L"uint32", L"int64", L"uint64" });
        return methodCastNames;

    } break;

    case PY_MEMORY_VIEW_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"double", L"cell", L"single", L"logical", L"int8", L"uint8", L"int16", L"uint16",
                L"int32", L"uint32", L"int64", L"uint64" });
        return methodCastNames;

    } break;
    case PY_LIST_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"string", L"cell", L"double", L"single", L"logical", L"int8", L"uint8", L"int16",
                L"uint16", L"int32", L"uint32", L"int64", L"uint64" });
        return methodCastNames;
    } break;
    case PY_TUPLE_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"string", L"cell", L"double", L"single", L"logical", L"int8", L"uint8", L"int16",
                L"uint16", L"int32", L"uint32", L"int64", L"uint64" });
        return methodCastNames;

    } break;
    case PY_DICT_TYPE: {
        methodCastNames.push_back(L"struct");
        return methodCastNames;
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"numeric", L"string", L"cell", L"double", L"single", L"logical", L"int8", L"uint8",
                L"int16", L"uint16", L"int32", L"uint32", L"int64", L"uint64" });
        return methodCastNames;
    } break;
    case PY_LONG_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"double", L"single", L"logical", L"int8", L"uint8", L"int16", L"uint16", L"int32",
                L"uint32", L"int64", L"uint64" });
        return methodCastNames;
    } break;
    case PY_BYTE_ARRAY_TYPE:
    case PY_NUMPY_TYPE: {
        methodCastNames.insert(methodCastNames.end(),
            { L"numeric", L"double", L"single", L"logical", L"int8", L"uint8", L"int16", L"uint16",
                L"int32", L"uint32", L"int64", L"uint64" });
        return methodCastNames;
    } break;
    case PY_NONE_TYPE:
    case PY_FLOAT_TYPE:
    case PY_BOOL_TYPE:
    case PY_COMPLEX_TYPE:
    case PY_NOT_MANAGED:
    default: {
    } break;
    }
    return methodCastNames;
}
//=============================================================================
bool
PythonObjectHandle::isCastMethod(const std::wstring& methodName)
{
    wstringVector methodCastNames = getCastMethods();

    auto it = std::find(methodCastNames.begin(), methodCastNames.end(), methodName);
    return (it != methodCastNames.end());
}
//=============================================================================
bool
PythonObjectHandle::isPyObjectMethod(const std::wstring& methodName, bool withUnderscore)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    wstringVector pythonMethodNames = getPyObjectMethods(pyObject, withUnderscore);
    auto it = std::find(pythonMethodNames.begin(), pythonMethodNames.end(), methodName);
    return (it != pythonMethodNames.end());
}
//=============================================================================
wstringVector
PythonObjectHandle::getMethods()
{
    if (isMainPythonInterpreter()) {
        return {};
    }
    wstringVector methodCastNames = getCastMethods();
    wstringVector methodOperatorNames = getOperatorMethods();
    methodCastNames.insert(
        methodCastNames.end(), methodOperatorNames.begin(), methodOperatorNames.end());
    PyObject* pyObject = (PyObject*)this->getPointer();
    wstringVector pythonMethodNames = getPyObjectMethods(pyObject, false);
    methodCastNames.insert(
        methodCastNames.end(), pythonMethodNames.begin(), pythonMethodNames.end());
    methodCastNames.push_back(L"keyHash");
    methodCastNames.push_back(L"keyMatch");
    std::sort(methodCastNames.begin(), methodCastNames.end());
    return methodCastNames;
}
//=============================================================================
bool
PythonObjectHandle::isProperty(const std::wstring& propertyName)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method
            = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(propertyName).c_str());
        bool callable = method && !NLSPyCallable_Check(method);
        NLSPy_DECREF(method);
        return callable;
    }
    return false;
}
//=============================================================================
wstringVector
PythonObjectHandle::getProperties()
{
    if (isMainPythonInterpreter()) {
        return {};
    }
    PyObject* pyObject = (PyObject*)this->getPointer();
    return getPyObjectProperties(pyObject, false);
}
//=============================================================================
bool
PythonObjectHandle::get(const std::wstring& propertyName, ArrayOf& result)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    PyObject* propertyObj
        = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(propertyName).c_str());
    if (!propertyObj || NLSPyCallable_Check(propertyObj)) {
        return false;
    }
    if (propertyObj) {
        bool needDecreaseReference;
        result = PyObjectToArrayOf(propertyObj, needDecreaseReference);
        if (needDecreaseReference) {
            NLSPy_DECREF(propertyObj);
        }
    } else {
        NLSPy_DECREF(propertyObj);
        std::wstring errorMessage = _W("Error calling property.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage, L"Nelson:Python:PyException");
    }
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeMethod(Interface* io, const ArrayOfVector& argIn, int nLhs,
    const std::string& methodName, ArrayOfVector& results)
{
    ArrayOfVector params = argIn;
    params.pop_front();
    return invoke(io, utf8_to_wstring(methodName), params, nLhs, results);
}
//=============================================================================
static bool
handleUnaryOperator(
    PyObject* pyObject, const std::wstring& pythonOperatorName, ArrayOfVector& results)
{
    std::string utf8OperatorName = wstring_to_utf8(pythonOperatorName);

    PyObject* nameMethod = NLSPyUnicode_FromString(utf8OperatorName.c_str());
    if (!nameMethod) {
        Error(_W("Failed to create Python method name object."), L"Nelson:Python:PyException");
    }

    PyObject* pyObjectResult = NLSPyObject_VectorcallMethod(
        nameMethod, &pyObject, 1 | PY_VECTORCALL_ARGUMENTS_OFFSET, nullptr);
    NLSPy_DECREF(nameMethod);

    if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    if (!pyObjectResult) {
        if (pythonOperatorName == L"__neg__") {
            pyObjectResult = NLSPyNumber_Negative(pyObject);
        } else if (pythonOperatorName == L"__pos__") {
            pyObjectResult = NLSPyNumber_Positive(pyObject);
        }
    }
    if (!pyObjectResult) {
        std::wstring errorMessage = _W("Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage, L"Nelson:Python:PyException");
    }
    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(pyObjectResult);
    results << ArrayOf::handleConstructor(pythonObjectHandle);
    return true;
}
//=============================================================================
static bool
handleBinaryOperator(PyObject* pyObject, const std::wstring& pythonOperatorName,
    const ArrayOfVector& inputs, ArrayOfVector& results)
{
    PyObject* arg1 = arrayOfToPyObject(inputs[0]);

    PyObject* p1 = pyObject;
    PyObject* p2 = arg1;

    PyObject* arg = NLSPy_BuildValue("(O)", p2);
    if (!arg) {
        Error(_W("Failed to create Python argument tuple."), L"Nelson:Python:PyException");
        return false;
    }

    PyObject* nameMethod = NLSPyUnicode_FromString(wstring_to_utf8(pythonOperatorName).c_str());
    if (!nameMethod) {
        Error(_W("Failed to create Python method name object."), L"Nelson:Python:PyException");
    }

    PyObject* args[2] = { p1, p2 };
    size_t nargsf = 2 | PY_VECTORCALL_ARGUMENTS_OFFSET;
    PyObject* pyObjectResult = NLSPyObject_VectorcallMethod(nameMethod, args, nargsf, NULL);
    if (!pyObjectResult) {
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
        }
    }
    if (!pyObjectResult || (TypeName(pyObjectResult) == L"NotImplementedType")) {
        if (pythonOperatorName == L"__add__") {
            pyObjectResult = NLSPyNumber_Add(p1, p2);
        }
        if (pythonOperatorName == L"__sub__") {
            pyObjectResult = NLSPyNumber_Subtract(p1, p2);
        }
        if (pythonOperatorName == L"__mod__") {
            pyObjectResult = NLSPyNumber_Remainder(p1, p2);
        }
        if (pythonOperatorName == L"__pow__") {
            pyObjectResult = NLSPyNumber_Power(p1, p2, Py_None);
        }
        if (pythonOperatorName == L"__mul__") {
            pyObjectResult = NLSPyNumber_Multiply(p1, p2);
        }
        if (pythonOperatorName == L"__truediv__") {
            pyObjectResult = NLSPyNumber_TrueDivide(p1, p2);
        }
    }

    if (!pyObjectResult) {
        std::wstring errorMessage = _W("Error calling method.");
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Print();
            errorMessage = getPythonStandardError();
            NLSPyErr_Clear();
        }
        Error(errorMessage, L"Nelson:Python:PyException");
    }

    bool needToDecreaseReference;
    results << PyObjectToArrayOf(pyObjectResult, needToDecreaseReference);
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeOperatorMethod(
    const std::wstring& methodName, const ArrayOfVector& inputs, ArrayOfVector& results)
{
    auto it = operatorMap.find(methodName);
    std::wstring pythonOperatorName;
    if (it == operatorMap.end()) {
        return false;
    }
    pythonOperatorName = it->second;

    PyObject* pyObject = (PyObject*)this->getPointer();

    if (methodName == L"uminus" || methodName == L"uplus") {
        return handleUnaryOperator(pyObject, pythonOperatorName, results);
    }
    return handleBinaryOperator(pyObject, pythonOperatorName, inputs, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastMethod(const std::wstring& methodName, ArrayOfVector& results)
{
    auto it = methodMap.find(methodName);
    if (it != methodMap.end()) {
        return it->second(results);
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeHashMethod(uint64& hashValue)
{
    PyObject* pyObject = (PyObject*)this->getPointer();

    if (!pyObject) {
        Error(_W("Invalid Python object."), L"Nelson:Python:PyException");
    }
    hashValue = PyGetHashValue(pyObject);
    return true;
}
//=============================================================================
bool
PythonObjectHandle::isEqual(PythonObjectHandle& pythonObjectHandle)
{
    PyObject* pyObjectA = (PyObject*)this->getPointer();
    PyObject* pyObjectB = (PyObject*)pythonObjectHandle.getPointer();

    return PyIsEqual(pyObjectA, pyObjectB);
}
//=============================================================================
}
//=============================================================================
