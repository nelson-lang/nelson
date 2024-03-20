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
        PyObject* poRep = NLSPyObject_Str(po);
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
                        bool startWithUnderscore = (attributName.rfind("__", 0) == 0);
                        bool addName = true;
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
                        bool startWithUnderscore = (attributName.rfind("__", 0) == 0);
                        bool addName = true;
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
}
//=============================================================================
