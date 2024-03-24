//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PyObjectHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
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
PyObject*
deepCopyPyObject(PyObject* obj)
{
    PyObject* copy_module = NLSPyImport_ImportModule("copy");
    if (copy_module == nullptr) {
        return nullptr;
    }

    PyObject* deepcopy_func = NLSPyObject_GetAttrString(copy_module, "deepcopy");
    if (deepcopy_func == nullptr) {
        NLSPy_DECREF(copy_module);
        return nullptr;
    }
    PyObject* copied_object = NLSPyObject_CallObject(deepcopy_func, (PyObject*)obj);
    NLSPy_DECREF(copy_module);
    NLSPy_DECREF(deepcopy_func);
    return copied_object;
}
//=============================================================================
const char*
getArrayArrayTypeCode(PyObject* pyObject)
{
    PyObject* arrayModule = NLSPyImport_ImportModule("array");
    if (!arrayModule) {
        return nullptr;
    }
    PyObject* arrayType = NLSPyObject_GetAttrString(arrayModule, "array");
    if (!arrayType) {
        NLSPy_XDECREF(arrayModule);
        return nullptr;
    }
    bool isInstanceArrayArray = NLSPyObject_IsInstance(pyObject, arrayType);
    if (!isInstanceArrayArray) {
        NLSPy_XDECREF(arrayType);
        NLSPy_XDECREF(arrayModule);
        return nullptr;
    }

    PyObject* typeChar = NLSPyObject_GetAttrString(pyObject, "typecode");
    if (typeChar) {
        PyObject* typeStr = NLSPyUnicode_AsUTF8String(typeChar);
        if (typeStr) {
            const char* typeCode = NLSPyBytes_AsString(typeStr);
            NLSPy_DECREF(typeStr);
            return typeCode;
        }
    }
    NLSPy_XDECREF(typeChar);
    NLSPy_XDECREF(arrayType);
    NLSPy_XDECREF(arrayModule);

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
}
//=============================================================================
