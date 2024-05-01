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

    operatorMap = { { L"mtimes", L"" }, { L"mrdivide", L"" }, { L"mpower", L"" },
        { L"gt", L"__gt__" }, { L"ge", L"__ge__" }, { L"le", L"__le__" }, { L"ne", L"__ne__" },
        { L"lt", L"__lt__" }, { L"plus", L"__add__" }, { L"minus", L"__sub__" },
        { L"eq", L"__eq__" }, { L"mod", L"__mod__" }, { L"uplus", L"__pos__" },
        { L"uminus", L"__neg__" } };
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
ArrayOfVector
PythonObjectHandle::invokeMethod(
    const ArrayOfVector& argIn, int nLhs, const std::string& methodName)
{
    ArrayOfVector params = argIn;
    params.pop_front();
    ArrayOfVector results;
    invoke(utf8_to_wstring(methodName), params, nLhs, results);
    return results;
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
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (pyObject) {
        PyObject* method = NLSPyObject_GetAttrString(pyObject, wstring_to_utf8(methodName).c_str());
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

    for (const auto& pair : operatorMap) {
        if (isPyObjectMethod(pair.second, true)) {
            methodOperatorNames.push_back(pair.first);
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
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;

    } break;

    case PY_MEMORY_VIEW_TYPE: {
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"cell");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;

    } break;
    case PY_LIST_TYPE: {
        methodCastNames.push_back(L"string");
        methodCastNames.push_back(L"cell");
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;
    } break;
    case PY_TUPLE_TYPE: {
        methodCastNames.push_back(L"string");
        methodCastNames.push_back(L"cell");
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;

    } break;
    case PY_DICT_TYPE: {
        methodCastNames.push_back(L"struct");
        return methodCastNames;
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        methodCastNames.push_back(L"numeric");
        methodCastNames.push_back(L"string");
        methodCastNames.push_back(L"cell");
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;
    } break;
    case PY_LONG_TYPE: {
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
        return methodCastNames;
    } break;
    case PY_BYTE_ARRAY_TYPE:
    case PY_NUMPY_TYPE: {
        methodCastNames.push_back(L"numeric");
        methodCastNames.push_back(L"double");
        methodCastNames.push_back(L"single");
        methodCastNames.push_back(L"logical");
        methodCastNames.push_back(L"int8");
        methodCastNames.push_back(L"uint8");
        methodCastNames.push_back(L"int16");
        methodCastNames.push_back(L"uint16");
        methodCastNames.push_back(L"int32");
        methodCastNames.push_back(L"uint32");
        methodCastNames.push_back(L"int64");
        methodCastNames.push_back(L"uint64");
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

    switch (getPythonType(pyObject)) {
    case PY_LIST_TYPE: {
        Py_ssize_t sz = NLSPyList_Size(pyObject);

        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, sz);
        Dimensions dims(1, sz);
        ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = NLSPyList_GetItem(pyObject, i);
            if (item != NULL) {
                bool needDecreaseReference;
                elements[i] = PyObjectToArrayOf(item, needDecreaseReference);
            } else {
                Error(_W("Cannot convert to ") + L"cell");
            }
        }
        results << res;
        return true;
    } break;
    case PY_TUPLE_TYPE: {
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
    } break;
    default: {
        Error(_W("Cannot convert to ") + L"cell");
    } break;
    }
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

    switch (getPythonType(pyObject)) {
    case PY_DICT_TYPE: {
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
    } break;
    default: {
        Error(_W("Cannot convert to ") + L"struct");
    } break;
    }
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
    switch (getPythonType(pyObject)) {
    case PY_MEMORY_VIEW_TYPE: {
        results << PyMemoryViewToArrayOf(pyObject);
        return true;
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        const char* typeCode = getArrayArrayTypeCode(pyObject);
        if (typeCode) {
            results << PyArrayArrayToArrayOf(pyObject);
            return true;
        }
    } break;
    case PY_BYTE_ARRAY_TYPE: {
        char* buffer = NLSPyByteArray_AsString(pyObject);
        size_t len = strlen(buffer);
        uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, len);
        ArrayOf res(NLS_UINT8, Dimensions(1, len), ptr);
        for (size_t k = 0; k < len; ++k) {
            ptr[k] = (uint8)buffer[k];
        }
        results << res;
        return true;
    } break;
    case PY_NUMPY_TYPE: {
        PyObject* data = NLSPyObject_GetAttrString(pyObject, "data");
        if (data) {
            results << PyMemoryViewToArrayOf(pyObject);
            return true;
        }
    } break;
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
    switch (getPythonType(pyObject)) {
    case PY_STR_TYPE: {
        std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
        results << ArrayOf::characterArrayConstructor(str);
        return true;
    } break;
    default: {
        results << ArrayOf::characterArrayConstructor(PyObjectToStringRepresentation(pyObject));
        return true;
    } break;
    }
    return false;
}
//=============================================================================
template <typename GetSizeFunc, typename GetItemFunc>
bool
handleListOrTupleToString(
    PyObject* pyObject, GetSizeFunc getSizeFunc, GetItemFunc getItemFunc, ArrayOfVector& results)
{
    Py_ssize_t sz = getSizeFunc(pyObject);
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, sz);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, Dimensions(1, sz), elements);

    for (Py_ssize_t i = 0; i < sz; ++i) {
        PyObject* item = getItemFunc(pyObject, i);
        if (item != NULL) {
            if (getPythonType(item) == PY_STR_TYPE) {
                std::string str = std::string(NLSPyUnicode_AsUTF8(item));
                elements[i] = ArrayOf::characterArrayConstructor(str);
            } else {
                elements[i]
                    = ArrayOf::characterArrayConstructor(PyObjectToStringRepresentation(item));
            }
        } else {
            Error(_W("All Python elements must be convertible as scalar to the requested type."));
            return false;
        }
    }

    results << res;
    return true;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastStringMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    if (!pyObject) {
        Error(_W("Invalid Python object."));
        return false;
    }

    switch (getPythonType(pyObject)) {
    case PY_STR_TYPE: {
        std::string str = std::string(NLSPyUnicode_AsUTF8(pyObject));
        results << ArrayOf::stringArrayConstructor(str);
        return true;
    } break;
    case PY_LIST_TYPE: {
        auto getSizeFunc = [](PyObject* obj) { return NLSPyList_Size(obj); };
        auto getItemFunc = [](PyObject* obj, Py_ssize_t i) { return NLSPyList_GetItem(obj, i); };
        return handleListOrTupleToString(pyObject, getSizeFunc, getItemFunc, results);
    } break;
    case PY_TUPLE_TYPE: {
        auto getSizeFunc = [](PyObject* obj) { return NLSPyTuple_Size(obj); };
        auto getItemFunc = [](PyObject* obj, Py_ssize_t i) { return NLSPyTuple_GetItem(obj, i); };
        return handleListOrTupleToString(pyObject, getSizeFunc, getItemFunc, results);
    } break;
    }
    return false;
}
//=============================================================================
template <class T>
bool
castRealMethod(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }

    PythonType pyType = getPythonType(pyObject);
    switch (pyType) {
    case PY_NUMPY_TYPE: {
        PyObject* data = NLSPyObject_GetAttrString(pyObject, "data");
        if (data) {
            bool res = castRealMethod<T>(data, nelsonType, results);
            NLSPy_DECREF(data);
            return res;
        }
    } break;
    case PY_FLOAT_TYPE: {
        double result = NLSPyFloat_AsDouble(pyObject);
        ArrayOf res = ArrayOf::doubleConstructor(result);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_BOOL_TYPE: {
        logical value = (logical)NLSPyObject_IsTrue(pyObject);
        ArrayOf res = ArrayOf::logicalConstructor(value);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_COMPLEX_TYPE: {
        std::complex<double> value;
        Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
        ArrayOf res = ArrayOf::dcomplexConstructor(pyCplx.real, pyCplx.imag);
        if (nelsonType == NLS_DOUBLE) {
            res.promoteType(NLS_DCOMPLEX);
        } else {
            res.promoteType(NLS_SCOMPLEX);
        }
        results << res;
        return true;
    } break;
    case PY_LONG_TYPE: {
        double value = NLSPyLong_AsDouble(pyObject);
        ArrayOf res = ArrayOf::doubleConstructor(value);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_BYTES_TYPE: {
        char* buffer;
        Py_ssize_t size;
        if (NLSPyBytes_AsStringAndSize(pyObject, &buffer, &size) == 0) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, size);
            ArrayOf res(nelsonType, Dimensions(1, size), ptr);
            for (size_t k = 0; k < (size_t)size; ++k) {
                ptr[k] = (T)buffer[k];
            }
            results << res;
            return true;
        }
    } break;
    case PY_BYTE_ARRAY_TYPE: {
        char* buffer = NLSPyByteArray_AsString(pyObject);
        size_t len = strlen(buffer);
        T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, len);
        ArrayOf res(nelsonType, Dimensions(1, len), ptr);
        for (size_t k = 0; k < len; ++k) {
            ptr[k] = (T)buffer[k];
        }
        results << res;
        return true;
    } break;
    case PY_MEMORY_VIEW_TYPE: {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        if (res.isComplex()) {
            if (nelsonType == NLS_DOUBLE) {
                res.promoteType(NLS_DCOMPLEX);
            } else {
                res.promoteType(NLS_SCOMPLEX);
            }
        } else {
            res.promoteType(nelsonType);
        }
        results << res;
        return true;
    } break;
    case PY_LIST_TYPE:
    case PY_TUPLE_TYPE: {
        Py_ssize_t sz
            = (pyType == PY_LIST_TYPE) ? NLSPyList_Size(pyObject) : NLSPyTuple_Size(pyObject);
        T* elements = (T*)ArrayOf::allocateArrayOf(
            nelsonType == NLS_DOUBLE ? NLS_DCOMPLEX : NLS_SCOMPLEX, sz);
        Dimensions dims(1, sz);
        ArrayOf res
            = ArrayOf(nelsonType == NLS_DOUBLE ? NLS_DCOMPLEX : NLS_SCOMPLEX, dims, elements);
        indexType k = 0;
        bool allReal = true;
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = (pyType == PY_LIST_TYPE) ? NLSPyList_GetItem(pyObject, i)
                                                      : NLSPyTuple_GetItem(pyObject, i);
            if (item != NULL) {
                ArrayOfVector rr;
                if (castRealMethod<double>(item, NLS_DOUBLE, rr)) {
                    ArrayOf r = rr[0];
                    if (r.isNumeric()) {
                        if (r.isComplex()) {
                            std::complex<double> c = r.getContentAsDoubleComplexScalar();
                            elements[k] = (T)c.real();
                            elements[k + 1] = (T)c.imag();
                            allReal = false;
                        } else {
                            elements[k] = (T)r.getContentAsDoubleScalar();
                            elements[k + 1] = 0;
                        }
                        k = k + 2;
                    } else {
                        Error(_W("All Python elements must be convertible as scalar to the "
                                 "requested "
                                 "type."));
                        return false;
                    }
                } else {
                    Error(_W("All Python elements must be convertible as scalar to the requested "
                             "type."));
                    return false;
                }
            } else {
                Error(_W("All Python elements must be convertible as scalar to the requested "
                         "type."));
                return false;
            }
        }
        if (allReal) {
            res.promoteType(nelsonType);
        }
        results << res;
        return true;
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        const char* typeCode = getArrayArrayTypeCode(pyObject);
        if (typeCode) {
            ArrayOf res = PyArrayArrayToArrayOf(pyObject);
            if (res.isComplex()) {
                if (nelsonType == NLS_DOUBLE) {
                    res.promoteType(NLS_DCOMPLEX);
                } else {
                    res.promoteType(NLS_SCOMPLEX);
                }
            } else {
                res.promoteType(nelsonType);
            }
            results << res;
            return true;
        }
    } break;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastDoubleMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castRealMethod<double>(pyObject, NLS_DOUBLE, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastSingleMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castRealMethod<single>(pyObject, NLS_SINGLE, results);
}
//=============================================================================
template <class T>
bool
castIntegerMethod(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    if (!pyObject) {
        Error(_W("Invalid Python object."));
    }
    PythonType pyType = getPythonType(pyObject);
    switch (pyType) {
    case PY_NUMPY_TYPE: {
        PyObject* data = NLSPyObject_GetAttrString(pyObject, "data");
        if (data) {
            bool res = castIntegerMethod<T>(data, nelsonType, results);
            NLSPy_DECREF(data);
            return res;
        }
    } break;
    case PY_FLOAT_TYPE: {
        double result = NLSPyFloat_AsDouble(pyObject);
        ArrayOf res = ArrayOf::doubleConstructor(result);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_BOOL_TYPE: {
        logical value = (logical)NLSPyObject_IsTrue(pyObject);
        ArrayOf res = ArrayOf::logicalConstructor(value);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_COMPLEX_TYPE: {
        std::complex<double> value;
        Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
        ArrayOf res = ArrayOf::dcomplexConstructor(pyCplx.real, pyCplx.imag);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_LONG_TYPE: {
        double value = NLSPyLong_AsDouble(pyObject);
        ArrayOf res = ArrayOf::doubleConstructor(value);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    case PY_BYTES_TYPE: {
        char* buffer;
        Py_ssize_t size;
        if (NLSPyBytes_AsStringAndSize(pyObject, &buffer, &size) == 0) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, size);
            ArrayOf res(nelsonType, Dimensions(1, size), ptr);
            for (size_t k = 0; k < (size_t)size; ++k) {
                ptr[k] = (T)buffer[k];
            }
            results << res;
            return true;
        }
    } break;
    case PY_BYTE_ARRAY_TYPE: {
        char* buffer = NLSPyByteArray_AsString(pyObject);
        size_t len = strlen(buffer);
        T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, len);
        ArrayOf res(nelsonType, Dimensions(1, len), ptr);
        for (size_t k = 0; k < len; ++k) {
            ptr[k] = (T)buffer[k];
        }
        results << res;
        return true;
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        const char* typeCode = getArrayArrayTypeCode(pyObject);
        if (typeCode) {
            ArrayOf res = PyArrayArrayToArrayOf(pyObject);
            res.promoteType(nelsonType);
            results << res;
            return true;
        }
    } break;
    case PY_LIST_TYPE:
    case PY_TUPLE_TYPE: {
        Py_ssize_t sz
            = (pyType == PY_LIST_TYPE) ? NLSPyList_Size(pyObject) : NLSPyTuple_Size(pyObject);
        T* values = (T*)ArrayOf::allocateArrayOf(nelsonType, sz);
        ArrayOf res = ArrayOf(nelsonType, Dimensions(1, sz), values);
        for (Py_ssize_t i = 0; i < sz; ++i) {
            PyObject* item = (pyType == PY_LIST_TYPE) ? NLSPyList_GetItem(pyObject, i)
                                                      : NLSPyTuple_GetItem(pyObject, i);
            if (item != NULL) {
                ArrayOfVector rr;
                if (castIntegerMethod<T>(item, nelsonType, rr)) {
                    T* ptr = (T*)rr[0].getDataPointer();
                    values[i] = ptr[0];
                } else {
                    Error(_W("All Python elements must be convertible as scalar to the "
                             "requested "
                             "type."));
                    return false;
                }
            } else {
                Error(_W("All Python elements must be convertible as scalar to the "
                         "requested "
                         "type."));
                return false;
            }
        }
        results << res;
        return true;
    } break;
    case PY_MEMORY_VIEW_TYPE: {
        ArrayOf res = PyMemoryViewToArrayOf(pyObject);
        res.promoteType(nelsonType);
        results << res;
        return true;
    } break;
    }
    return false;
}
//=============================================================================
bool
PythonObjectHandle::invokeCastLogicalMethod(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<logical>(pyObject, NLS_LOGICAL, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt8Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<int8>(pyObject, NLS_INT8, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt16Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<int16>(pyObject, NLS_INT16, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt32Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<int32>(pyObject, NLS_INT32, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastInt64Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<int64>(pyObject, NLS_INT64, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt8Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<uint8>(pyObject, NLS_UINT8, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt16Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<uint16>(pyObject, NLS_UINT16, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt32Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<uint32>(pyObject, NLS_UINT32, results);
}
//=============================================================================
bool
PythonObjectHandle::invokeCastUInt64Method(ArrayOfVector& results)
{
    PyObject* pyObject = (PyObject*)this->getPointer();
    return castIntegerMethod<uint64>(pyObject, NLS_UINT64, results);
}
//=============================================================================
static bool
handleUnaryOperator(
    PyObject* pyObject, const std::wstring& pythonOperatorName, ArrayOfVector& results)
{
    std::string utf8OperatorName = wstring_to_utf8(pythonOperatorName);

    PyObject* nameMethod = NLSPyUnicode_FromString(utf8OperatorName.c_str());
    if (!nameMethod) {
        Error(_W("Failed to create Python method name object."));
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
        Error(_W("Error calling method."));
        return false;
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
    PyObject* p1 = pyObject;
    PyObject* arg1 = arrayOfToPyObject(inputs[0]);
    PyObject* p2 = arg1;

    PyObject* arg = NLSPy_BuildValue("(O)", p2);
    if (!arg) {
        Error(_W("Failed to create Python argument tuple."));
        return false;
    }

    PyObject* nameMethod = NLSPyUnicode_FromString(wstring_to_utf8(pythonOperatorName).c_str());
    if (!nameMethod) {
        Error(_W("Failed to create Python method name object."));
    }

    PyObject* args[2] = { p1, p2 };
    size_t nargsf = 2 | PY_VECTORCALL_ARGUMENTS_OFFSET;
    PyObject* pyObjectResult = NLSPyObject_VectorcallMethod(nameMethod, args, nargsf, NULL);
    if (!pyObjectResult) {
        std::wstring errorMessage;
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
        }
    }
    std::wstring typeResult = TypeName(pyObjectResult);
    if (typeResult == L"NotImplementedType") {
        if (pythonOperatorName == L"__add__") {
            pyObjectResult = NLSPyNumber_Add(p1, p2);
        }
        if (pythonOperatorName == L"__sub__") {
            pyObjectResult = NLSPyNumber_Subtract(p1, p2);
        }
        // L"mtimes", L"" },
        //  { L"mrdivide", L"" },
        //  { L"mpower", L"" },
        //  { L"gt", L"__gt__" },
        //  { L"ge", L"__ge__" },
        // { L"le", L"__le__" },
        // { L"ne", L"__ne__" },
        // { L"lt", L"__lt__" },
        // { L"eq", L"__eq__" },
        //{ L"mod", L"__mod__" },
    }

    if (!pyObjectResult) {
        Error(_W("Error calling method."));
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
