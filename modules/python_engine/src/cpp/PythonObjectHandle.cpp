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
    std::wstring pyObjTypeName = getTypeName();
    std::wstring strFormat;

    if ((pyObjTypeName == L"dict") || (pyObjTypeName == L"str") || (pyObjTypeName == L"NoneType")) {
        strFormat = _W("  Python %s with no properties.");
    } else if ((pyObjTypeName == L"list") || (pyObjTypeName == L"int")) {
        strFormat = _W("  Python %s with values:");
    } else {
        strFormat = _W("  Python %s:");
    }

    std::wstring msg = fmt::sprintf(strFormat, pyObjTypeName);
    msg.append(L"\n");
    msg.append(L"\n");
    io->outputMessage(msg);
    std::wstring rep = PyObjectToStringRepresentation(((PyObject*)this->getPointer()));
    io->outputMessage(std::wstring(L"    ") + rep + L"\n");
}
//=============================================================================
std::wstring
PythonObjectHandle::getTypeName()
{
    return TypeName(((PyObject*)this->getPointer()));
}
//=============================================================================
std::wstring
PythonObjectHandle::getClassName()
{
    std::wstring _typename = getTypeName();
    if (!_typename.empty()) {
        return utf8_to_wstring(NLS_HANDLE_PYOBJECT_CATEGORY_STR) + L"." + _typename;
    }
    return utf8_to_wstring(NLS_HANDLE_PYOBJECT_CATEGORY_STR);
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
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
        bool callable = method && NLSPyCallable_Check(method);
        NLSPy_DECREF(method);
        return callable;
    }
    return false;
}
//=============================================================================

wstringVector
PythonObjectHandle::getCastMethods()
{
    wstringVector methodsList;

    /*
    methodsList.push_back(L"char");
    methodsList.push_back(L"string");
    methodsList.push_back(L"cell");
    methodsList.push_back(L"struct");
    methodsList.push_back(L"double");
    methodsList.push_back(L"single");
    methodsList.push_back(L"logical");
    methodsList.push_back(L"int8");
    methodsList.push_back(L"uint8");
    methodsList.push_back(L"int16");
    methodsList.push_back(L"uint16");
    methodsList.push_back(L"int32");
    methodsList.push_back(L"uint32");
    methodsList.push_back(L"int64");
    methodsList.push_back(L"uint64");
    */
    if (getTypeName() == L"NoneType") {
        methodsList.push_back(L"char");
    }
    if (getTypeName() == L"str") {
        methodsList.push_back(L"numeric");
        methodsList.push_back(L"char");
        methodsList.push_back(L"string");
        methodsList.push_back(L"cell");
    }

    if (getTypeName() == L"memoryview") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"double");
    }

    if (getTypeName() == L"array.array") {
        methodsList.push_back(L"numeric");
        methodsList.push_back(L"char");
        methodsList.push_back(L"string");
        methodsList.push_back(L"cell");
        methodsList.push_back(L"double");
        methodsList.push_back(L"single");
        methodsList.push_back(L"logical");
        methodsList.push_back(L"int8");
        methodsList.push_back(L"uint8");
        methodsList.push_back(L"int16");
        methodsList.push_back(L"uint16");
        methodsList.push_back(L"int32");
        methodsList.push_back(L"uint32");
        methodsList.push_back(L"int64");
        methodsList.push_back(L"uint64");
    }
    if (getTypeName() == L"dict") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"struct");
    }
    if (getTypeName() == L"list") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"string");
        methodsList.push_back(L"cell");
        methodsList.push_back(L"double");
        methodsList.push_back(L"single");
        methodsList.push_back(L"logical");
        methodsList.push_back(L"int8");
        methodsList.push_back(L"uint8");
        methodsList.push_back(L"int16");
        methodsList.push_back(L"uint16");
        methodsList.push_back(L"int32");
        methodsList.push_back(L"uint32");
        methodsList.push_back(L"int64");
        methodsList.push_back(L"uint64");
    }
    if (getTypeName() == L"int") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"double");
        methodsList.push_back(L"single");
        methodsList.push_back(L"logical");
        methodsList.push_back(L"int8");
        methodsList.push_back(L"uint8");
        methodsList.push_back(L"int16");
        methodsList.push_back(L"uint16");
        methodsList.push_back(L"int32");
        methodsList.push_back(L"uint32");
        methodsList.push_back(L"int64");
        methodsList.push_back(L"uint64");
    }
    if (getTypeName() == L"tuple") {
        methodsList.push_back(L"char");
        methodsList.push_back(L"string");
        methodsList.push_back(L"cell");
        methodsList.push_back(L"double");
        methodsList.push_back(L"single");
        methodsList.push_back(L"logical");
        methodsList.push_back(L"int8");
        methodsList.push_back(L"uint8");
        methodsList.push_back(L"int16");
        methodsList.push_back(L"uint16");
        methodsList.push_back(L"int32");
        methodsList.push_back(L"uint32");
        methodsList.push_back(L"int64");
        methodsList.push_back(L"uint64");
    }
    return methodsList;
}
//=============================================================================
bool
PythonObjectHandle::isCastMethod(const std::wstring& methodName)
{
    wstringVector methodsList = getCastMethods();

    auto it = std::find(methodsList.begin(), methodsList.end(), methodName);
    return (it != methodsList.end());
}
//=============================================================================
wstringVector
PythonObjectHandle::getMethods()
{
    if (isMainPythonInterpreter()) {
        return {};
    }
    wstringVector methodsList = getCastMethods();
    PyObject* pyObject = (PyObject*)this->getPointer();
    wstringVector pythonMethodNames = getPyObjectMethods(pyObject, false);
    methodsList.insert(methodsList.end(), pythonMethodNames.begin(), pythonMethodNames.end());

    return methodsList;
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
        Error(errorMessage);
    }
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastCellMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    if (getTypeName() == L"tuple") {
        Py_ssize_t sz = NLSPyTuple_Size(pyObject);

        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, sz);
        Dimensions dims(1, sz);
        ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = NLSPyTuple_GetItem(pyObject, i);
            if (item != NULL) {
                bool needDecreaseReference;
                elements[i] = PyObjectToArrayOf(item, needDecreaseReference);
            } else {
                Error(_W("Cannot convert to ") + L"cell");
            }
        }
        results << res;
        return true;
    }
    Error(_W("Cannot convert to ") + L"cell");
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastStructMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    if (getTypeName() == L"dict") {
        PyObject* keys = NLSPyDict_Keys(pyObject);
        Py_ssize_t size = NLSPyList_Size(keys);
        stringVector names;
        for (Py_ssize_t i = 0; i < size; i++) {
            PyObject* key = NLSPyList_GetItem(keys, i);
            PyObject* str = NLSPyObject_Str(key);
            names.push_back(NLSPyUnicode_AsUTF8(str));
            NLSPy_DECREF(str);
        }
        NLSPy_DECREF(keys);

        ArrayOfVector values;
        for (auto name : names) {
            PyObject* value = NLSPyDict_GetItemString(pyObject, name.c_str());
            bool needToDecreaseReference;
            values.push_back(PyObjectToArrayOf(value, needToDecreaseReference));
        }
        results << ArrayOf::structScalarConstructor(names, values);
        return true;
    }
    Error(_W("Cannot convert to ") + L"struct");
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastNumericMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    if (getTypeName() == L"memoryview") {
        results << PyMemoryViewToArrayOf(pyObject);
        return true;
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        results << PyArrayArrayToArrayOf(pyObject);
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastCharMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    if (getTypeName() == L"str") {
        std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
        results << ArrayOf::characterArrayConstructor(str);
        return true;
    }

    results << ArrayOf::characterArrayConstructor(PyObjectToStringRepresentation(pyObject));
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastStringMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    if (getTypeName() == L"str") {
        std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
        results << ArrayOf::stringArrayConstructor(str);
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastDoubleMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        if (res.isComplex()) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
        results << res;
        return true;
    }
    if (getTypeName() == L"tuple") {
        Py_ssize_t sz = NLSPyTuple_Size(pyObject);
        double* elements = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, sz);
        Dimensions dims(1, sz);
        ArrayOf res = ArrayOf(NLS_DCOMPLEX, dims, elements);
        indexType k = 0;
        bool allReal = true;
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = NLSPyTuple_GetItem(pyObject, i);
            if (item != NULL) {
                bool needDecreaseReference;
                ArrayOf r = PyObjectToArrayOf(item, needDecreaseReference);
                if (r.isNumeric()) {
                    if (r.isComplex()) {
                        std::complex<double> c = r.getContentAsDoubleComplexScalar();
                        elements[k] = c.real();
                        elements[k + 1] = c.imag();
                        allReal = false;
                    } else {
                        elements[k] = r.getContentAsDoubleScalar();
                        elements[k + 1] = 0;
                    }
                    k = k + 2;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        if (allReal) {
            res.promoteType(NLS_DOUBLE);
        }
        results << res;
        return true;
    }

    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        if (res.isComplex()) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastSingleMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        if (res.isComplex()) {
            res.promoteType(NLS_SCOMPLEX);
        } else {
            res.promoteType(NLS_SINGLE);
        }
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        if (res.isComplex()) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_DOUBLE);
        }
        results << res;
        return true;
    }
    if (getTypeName() == L"tuple") {
        Py_ssize_t sz = NLSPyTuple_Size(pyObject);
        double* elements = (double*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, sz);
        Dimensions dims(1, sz);
        ArrayOf res = ArrayOf(NLS_SCOMPLEX, dims, elements);
        indexType k = 0;
        bool allReal = true;
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = NLSPyTuple_GetItem(pyObject, i);
            if (item != NULL) {
                bool needDecreaseReference;
                ArrayOf r = PyObjectToArrayOf(item, needDecreaseReference);
                if (r.isNumeric()) {
                    if (r.isComplex()) {
                        std::complex<single> c = r.getContentAsSingleComplexScalar();
                        elements[k] = c.real();
                        elements[k + 1] = c.imag();
                        allReal = false;
                    } else {
                        elements[k] = r.getContentAsSingleScalar();
                        elements[k + 1] = 0;
                    }
                    k = k + 2;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        if (allReal) {
            res.promoteType(NLS_SINGLE);
        }
        results << res;
        return true;
    }

    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastLogicalMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_LOGICAL);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_LOGICAL);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt8Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_INT8);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_INT8);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt16Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_INT16);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_INT16);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt32Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_INT32);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_INT32);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt64Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_INT64);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_INT64);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt8Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_UINT8);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_UINT8);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt16Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_UINT16);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_UINT16);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt32Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_UINT32);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_UINT32);
        results << res;
        return true;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt64Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    const char* typeCode = getArrayArrayTypeCode(pyObject);
    if (typeCode) {
        ArrayOf res = PyArrayArrayToArrayOf(pyObject);
        res.promoteType(NLS_UINT64);
        results << res;
        return true;
    }
    if (getTypeName() == L"memoryview") {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(NLS_UINT64);
        results << res;
        return true;
    }
    return false;
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
PythonObjectHandle::invoke(
    const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    if (isMainPythonInterpreter() && invokeFunction(methodName, inputs, nLhs, results)) {
        return true;
    }
    if (invokeCastMethod(methodName, results)) {
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
