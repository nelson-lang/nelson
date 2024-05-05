//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "PyObjectHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PythonType
getPythonType(PyObject* po)
{
    if (po == nullptr) {
        return PythonType::PY_NOT_MANAGED;
    }

    PyTypeObject* pyTypeObject = Py_TYPE(po);

    if (pyTypeObject == PyFloat_TypePtr) {
        return PythonType::PY_FLOAT_TYPE;
    }
    if (pyTypeObject == PyBool_TypePtr) {
        return PythonType::PY_BOOL_TYPE;
    }
    if (pyTypeObject == PyComplex_TypePtr) {
        return PythonType::PY_COMPLEX_TYPE;
    }
    if (pyTypeObject == PyLong_TypePtr) {
        return PythonType::PY_LONG_TYPE;
    }
    if (pyTypeObject == PyBytes_TypePtr) {
        return PythonType::PY_BYTES_TYPE;
    }
    if (pyTypeObject == PyByteArray_TypePtr) {
        return PythonType::PY_BYTE_ARRAY_TYPE;
    }

    std::wstring typeName = TypeName(po);
    if (typeName == L"NoneType") {
        return PythonType::PY_NONE_TYPE;
    }
    if (typeName == L"memoryview") {
        return PythonType::PY_MEMORY_VIEW_TYPE;
    }
    if (typeName == L"list") {
        return PythonType::PY_LIST_TYPE;
    }
    if (typeName == L"tuple") {
        return PythonType::PY_TUPLE_TYPE;
    }
    if (typeName == L"dict") {
        return PythonType::PY_DICT_TYPE;
    }
    if (typeName == L"str") {
        return PythonType::PY_STR_TYPE;
    }

    std::wstring numpyPrefix = L"numpy.";
    if (typeName.compare(0, numpyPrefix.length(), numpyPrefix) == 0) {
        return PythonType::PY_NUMPY_TYPE;
    }

    PyObject* arrayModule = NLSPyImport_ImportModule("array");
    if (arrayModule) {
        PyObject* arrayType = NLSPyObject_GetAttrString(arrayModule, "array");
        if (arrayType) {
            if (NLSPyObject_IsInstance(po, arrayType)) {
                NLSPy_XDECREF(arrayType);
                NLSPy_XDECREF(arrayModule);
                return PythonType::PY_ARRAY_ARRAY_TYPE;
            }
            NLSPy_XDECREF(arrayType);
        }
        NLSPy_XDECREF(arrayModule);
    }

    return PythonType::PY_NOT_MANAGED;
}
//=============================================================================
std::wstring
PyObjectToStringRepresentation(PyObject* po)
{
    std::wstring rep;
    if (po) {
        PyObject* poRep = NLSPyObject_Repr(po);
        if (poRep) {
            const char* c_str = NLSPyUnicode_AsUTF8(poRep);
            rep = utf8_to_wstring(c_str);
            NLSPy_DECREF(poRep);
        }
    }
    return rep;
}
//=============================================================================
std::wstring
TypeName(PyObject* po)
{
    std::wstring res;
    if (po->ob_type && po->ob_type->tp_name) {
        const char* typeName = po->ob_type->tp_name;
        res = utf8_to_wstring(typeName);
    }
    return res;
}
//=============================================================================
wstringVector
getPyObjectMethods(PyObject* po, bool withUnderscoreMethods)
{
    wstringVector methodNames;
    PyObject* dir_result = NLSPyObject_Dir(po);
    if (dir_result && TypeName(dir_result) == L"list") {
        Py_ssize_t size = NLSPyList_Size(dir_result);
        for (Py_ssize_t i = 0; i < size; ++i) {
            PyObject* item = NLSPyList_GetItem(dir_result, i);
            if (item != NULL) {
                const char* attr_name = NLSPyUnicode_AsUTF8(item);
                if (attr_name) {
                    PyObject* method = NLSPyObject_GetAttrString(po, attr_name);
                    bool callable = method && NLSPyCallable_Check(method);
                    NLSPy_DECREF(method);
                    if (callable) {
                        std::string attributName = std::string(attr_name);
                        bool startWithUnderscore = (attributName.rfind("_", 0) == 0);
                        if (withUnderscoreMethods) {
                            methodNames.push_back(utf8_to_wstring(attributName));
                        } else {
                            if (!startWithUnderscore) {
                                methodNames.push_back(utf8_to_wstring(attributName));
                            }
                        }
                    }
                }
            }
        }
        NLSPy_DECREF(dir_result);
    }
    return methodNames;
}
//=============================================================================
wstringVector
getPyObjectProperties(PyObject* po, bool withUnderscoreMethods)
{
    wstringVector propertiesNames;
    PyObject* dir_result = NLSPyObject_Dir(po);
    if (dir_result && TypeName(dir_result) == L"list") {
        Py_ssize_t size = NLSPyList_Size(dir_result);
        for (Py_ssize_t i = 0; i < size; ++i) {
            PyObject* item = NLSPyList_GetItem(dir_result, i);
            if (item != NULL) {

                const char* attr_name = NLSPyUnicode_AsUTF8(item);
                if (attr_name) {
                    PyObject* method = NLSPyObject_GetAttrString(po, attr_name);
                    bool callable = method && NLSPyCallable_Check(method);
                    NLSPy_DECREF(method);
                    if (!callable) {
                        std::string attributName = std::string(attr_name);
                        bool startWithUnderscore = (attributName.rfind("_", 0) == 0);
                        if (withUnderscoreMethods) {
                            propertiesNames.push_back(utf8_to_wstring(attributName));
                        } else {
                            if (!startWithUnderscore) {
                                propertiesNames.push_back(utf8_to_wstring(attributName));
                            }
                        }
                    }
                }
            }
        }
        NLSPy_DECREF(dir_result);
    }
    return propertiesNames;
}
//=============================================================================
const char*
getArrayArrayTypeCode(PyObject* pyObject)
{
    PyObject* typeChar = NLSPyObject_GetAttrString(pyObject, "typecode");
    if (typeChar) {
        PyObject* typeStr = NLSPyUnicode_AsUTF8String(typeChar);
        if (typeStr) {
            const char* typeCode = NLSPyBytes_AsString(typeStr);
            NLSPy_DECREF(typeStr);
            NLSPy_XDECREF(typeChar);
            return typeCode;
        }
    }
    NLSPy_XDECREF(typeChar);

    return nullptr;
}
//=============================================================================
bool
isArrayArrayOfTypeCode(const char* typeName, PyObject* pyObject)
{
    const char* typecode = getArrayArrayTypeCode(pyObject);
    if (typecode) {
        return (strcmp(typecode, typeName) == 0);
    }
    return false;
}
//=============================================================================
char*
nelsonTypeToTypeCode(NelsonType nelsonType)
{
    switch (nelsonType) {
    case NLS_DOUBLE: {
        return (char*)"d";
    } break;
    case NLS_SINGLE: {
        return (char*)"f";
    } break;
    case NLS_DCOMPLEX: {
        return (char*)"Zd";
    } break;
    case NLS_SCOMPLEX: {
        return (char*)"Zf";
    } break;
    case NLS_INT8: {
        return (char*)"b";
    } break;
    case NLS_INT16: {
        return (char*)"h";
    } break;
    case NLS_INT32: {
        return (char*)"i";
    } break;
    case NLS_INT64: {
        return (char*)"q";
    } break;
    case NLS_UINT8: {
        return (char*)"B";
    } break;
    case NLS_UINT16: {
        return (char*)"H";
    } break;
    case NLS_UINT32: {
        return (char*)"I";
    } break;
    case NLS_UINT64: {
        return (char*)"Q";
    } break;
    case NLS_LOGICAL: {
        return (char*)"?";
    } break;
    default: {
        return (char*)"";
    }
    }
    return (char*)"";
}
//=============================================================================
NelsonType
PyTypecodeToNelsonType(const char* memoryViewType)
{
    if (strcmp(memoryViewType, "?") == 0) {
        return NLS_LOGICAL;
    }
    if (strcmp(memoryViewType, "d") == 0) {
        return NLS_DOUBLE;
    }
    if (strcmp(memoryViewType, "f") == 0) {
        return NLS_SINGLE;
    }
    if (strcmp(memoryViewType, "Zd") == 0) {
        return NLS_DCOMPLEX;
    }
    if (strcmp(memoryViewType, "Zf") == 0) {
        return NLS_SCOMPLEX;
    }
    if (strcmp(memoryViewType, "q") == 0) {
        return NLS_INT64;
    }
    if (strcmp(memoryViewType, "Q") == 0) {
        return NLS_UINT64;
    }
    if (strcmp(memoryViewType, "i") == 0) {
        return NLS_INT32;
    }
    if (strcmp(memoryViewType, "I") == 0) {
        return NLS_UINT32;
    }
    if (strcmp(memoryViewType, "l") == 0) {
        return NLS_INT32;
    }
    if (strcmp(memoryViewType, "L") == 0) {
        return NLS_UINT32;
    }
    if (strcmp(memoryViewType, "h") == 0) {
        return NLS_INT16;
    }
    if (strcmp(memoryViewType, "H") == 0) {
        return NLS_UINT16;
    }
    if (strcmp(memoryViewType, "b") == 0) {
        return NLS_INT8;
    }
    if (strcmp(memoryViewType, "B") == 0) {
        return NLS_UINT8;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
bool
PyIsEqual(PyObject* pyObjectA, PyObject* pyObjectB)
{
    return (NLSPyObject_RichCompareBool(pyObjectA, pyObjectB, Py_EQ) == 1);
}
//=============================================================================
}
//=============================================================================
