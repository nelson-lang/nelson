//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonLibraryWrapper.hpp"
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
//=============================================================================
PyTypeObject* PyBool_TypePtr = nullptr;
PyTypeObject* PyComplex_TypePtr = nullptr;
PyTypeObject* PyFloat_TypePtr = nullptr;
PyTypeObject* PyLong_TypePtr = nullptr;
PyTypeObject* PyMemoryView_TypePtr = nullptr;
PyTypeObject* PyUnicode_TypePtr = nullptr;
PyTypeObject* PyDict_TypePtr = nullptr;
//=============================================================================
PyObject* _Py_NoneStructPtr = nullptr;
//=============================================================================
static bool pythonLibraryLoaded = false;
static Nelson::library_handle python3XX_handle = nullptr;
//=============================================================================
static bool
loadPythonSymbols();
//=============================================================================
using PROC_Py_Initialize = void (*)();
PROC_Py_Initialize Py_InitializePtr = nullptr;
//=============================================================================
using PROC_Py_Finalize = void (*)();
PROC_Py_Finalize Py_FinalizePtr = nullptr;
//=============================================================================
using PROC_Py_IsInitialized = int (*)();
PROC_Py_IsInitialized Py_IsInitializedPtr = nullptr;
//=============================================================================
using PROC_PyImport_AddModule = PyObject* (*)(const char* name);
PROC_PyImport_AddModule PyImport_AddModulePtr = nullptr;
//=============================================================================
using PROC_PyRun_SimpleStringFlags = int (*)(const char* command, PyCompilerFlags* flags);
PROC_PyRun_SimpleStringFlags PyRun_SimpleStringFlagsPtr = nullptr;
//=============================================================================
using PROC_PyObject_GetAttrString = PyObject* (*)(PyObject* o, const char* attr_name);
PROC_PyObject_GetAttrString PyObject_GetAttrStringPtr = nullptr;
//=============================================================================
using PROC_PyUnicode_AsUTF8String = PyObject* (*)(PyObject* unicode);
PROC_PyUnicode_AsUTF8String PyUnicode_AsUTF8StringPtr = nullptr;
//=============================================================================
using PROC_PyUnicode_AsUTF8 = const char* (*)(PyObject* unicode);
PROC_PyUnicode_AsUTF8 PyUnicode_AsUTF8Ptr = nullptr;
//=============================================================================
using PROC_PyEval_SaveThread = PyThreadState* (*)();
PROC_PyEval_SaveThread PyEval_SaveThreadPtr = nullptr;
//=============================================================================
using PROC_PyEval_RestoreThread = void (*)(PyThreadState* tstate);
PROC_PyEval_RestoreThread PyEval_RestoreThreadPtr = nullptr;
//=============================================================================
using PROC_PyModule_GetDict = PyObject* (*)(PyObject* moduleo);
PROC_PyModule_GetDict PyModule_GetDictPtr = nullptr;
//=============================================================================
using PROC_PyGILState_Ensure = PyGILState_STATE (*)();
PROC_PyGILState_Ensure PyGILState_EnsurePtr = nullptr;
//=============================================================================
using PROC_PyErr_Clear = void (*)();
PROC_PyErr_Clear PyErr_ClearPtr = nullptr;
//=============================================================================
using PROC_PyGILState_Release = void (*)(PyGILState_STATE state);
PROC_PyGILState_Release PyGILState_ReleasePtr = nullptr;
//=============================================================================
using PROC_PyObject_SetAttrString = int (*)(PyObject* o, const char* attr_name, PyObject* v);
PROC_PyObject_SetAttrString PyObject_SetAttrStringPtr = nullptr;
//=============================================================================
using PROC_PyRun_SimpleFileExFlags
    = int (*)(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags);
PROC_PyRun_SimpleFileExFlags PyRun_SimpleFileExFlagsPtr = nullptr;
//=============================================================================
using PROC_PyObject_IsTrue = int (*)(PyObject* ob);
PROC_PyObject_IsTrue PyObject_IsTruePtr = nullptr;
//=============================================================================
using PROC_Py_DECREF = void (*)(PyObject* op);
PROC_Py_DECREF Py_DECREFPtr = nullptr;
//=============================================================================
using PROC_PyFloat_AsDouble = double (*)(PyObject* pyfloat);
PROC_PyFloat_AsDouble PyFloat_AsDoublePtr = nullptr;
//=============================================================================
using PROC_PyLong_AsDouble = double (*)(PyObject* pylong);
PROC_PyLong_AsDouble PyLong_AsDoublePtr = nullptr;
//=============================================================================
using PROC_PyErr_Occurred = PyObject* (*)(void);
PROC_PyErr_Occurred PyErr_OccurredPtr = nullptr;
//=============================================================================
using PROC_PyComplex_AsCComplex = Py_complex (*)(PyObject* op);
PROC_PyComplex_AsCComplex PyComplex_AsCComplexPtr = nullptr;
//=============================================================================
using PROC_PyObject_Str = PyObject* (*)(PyObject* o);
PROC_PyObject_Str PyObject_StrPtr = nullptr;
//=============================================================================
using PROC_PyObject_Dir = PyObject* (*)(PyObject* o);
PROC_PyObject_Dir PyObject_DirPtr = nullptr;
//=============================================================================
using PROC_PyList_Size = Py_ssize_t (*)(PyObject* o);
PROC_PyList_Size PyList_SizePtr = nullptr;
//=============================================================================
using PROC_PyList_GetItem = PyObject* (*)(PyObject* o, Py_ssize_t s);
PROC_PyList_GetItem PyList_GetItemPtr = nullptr;
//=============================================================================
using PROC_PyCallable_Check = int (*)(PyObject* o);
PROC_PyCallable_Check PyCallable_CheckPtr = nullptr;
//=============================================================================
using PROC_PyObject_CallNoArgs = PyObject* (*)(PyObject* func);
PROC_PyObject_CallNoArgs PyObject_CallNoArgsPtr = nullptr;
//=============================================================================
using PROC_PyErr_Print = void (*)();
PROC_PyErr_Print PyErr_PrintPtr = nullptr;
//=============================================================================
using PROC_PyImport_ImportModule = PyObject* (*)(const char* name);
PROC_PyImport_ImportModule PyImport_ImportModulePtr = nullptr;
//=============================================================================
using PROC_PyObject_CallObject = PyObject* (*)(PyObject* callable, PyObject* args);
PROC_PyObject_CallObject PyObject_CallObjectPtr = nullptr;
//=============================================================================
using PROC_PyUnicode_FromString = PyObject* (*)(const char* str);
PROC_PyUnicode_FromString PyUnicode_FromStringPtr = nullptr;
//=============================================================================
using PROC_PyDict_New = PyObject* (*)();
PROC_PyDict_New PyDict_NewPtr = nullptr;
//=============================================================================
bool
isPythonLibraryLoaded()
{
    return pythonLibraryLoaded;
}
//=============================================================================
static bool
loadPythonSymbols()
{
    //=============================================================================
    // Types
    //=============================================================================
    PyBool_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyBool_Type"));
    if (!PyBool_TypePtr) {
        return false;
    }

    PyComplex_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyComplex_Type"));
    if (!PyComplex_TypePtr) {
        return false;
    }

    PyFloat_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyFloat_Type"));
    if (!PyFloat_TypePtr) {
        return false;
    }

    PyLong_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyLong_Type"));
    if (!PyLong_TypePtr) {
        return false;
    }
    PyMemoryView_TypePtr = reinterpret_cast<PyTypeObject*>(
        Nelson::get_function(python3XX_handle, "PyMemoryView_Type"));
    if (!PyMemoryView_TypePtr) {
        return false;
    }
    PyUnicode_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyUnicode_Type"));
    if (!PyUnicode_TypePtr) {
        return false;
    }
    PyDict_TypePtr
        = reinterpret_cast<PyTypeObject*>(Nelson::get_function(python3XX_handle, "PyDict_Type"));
    if (!PyDict_TypePtr) {
        return false;
    }

    _Py_NoneStructPtr
        = reinterpret_cast<PyObject*>(Nelson::get_function(python3XX_handle, "_Py_NoneStruct"));
    if (!_Py_NoneStructPtr) {
        return false;
    }

    //=============================================================================
    Py_InitializePtr = reinterpret_cast<PROC_Py_Initialize>(
        Nelson::get_function(python3XX_handle, "Py_Initialize"));
    if (!Py_InitializePtr) {
        return false;
    }

    Py_FinalizePtr
        = reinterpret_cast<PROC_Py_Finalize>(Nelson::get_function(python3XX_handle, "Py_Finalize"));
    if (!Py_FinalizePtr) {
        return false;
    }

    Py_IsInitializedPtr = reinterpret_cast<PROC_Py_IsInitialized>(
        Nelson::get_function(python3XX_handle, "Py_IsInitialized"));
    if (!Py_IsInitializedPtr) {
        return false;
    }

    PyImport_AddModulePtr = reinterpret_cast<PROC_PyImport_AddModule>(
        Nelson::get_function(python3XX_handle, "PyImport_AddModule"));
    if (!PyImport_AddModulePtr) {
        return false;
    }

    PyRun_SimpleStringFlagsPtr = reinterpret_cast<PROC_PyRun_SimpleStringFlags>(
        Nelson::get_function(python3XX_handle, "PyRun_SimpleStringFlags"));
    if (!PyRun_SimpleStringFlagsPtr) {
        return false;
    }

    PyObject_GetAttrStringPtr = reinterpret_cast<PROC_PyObject_GetAttrString>(
        Nelson::get_function(python3XX_handle, "PyObject_GetAttrString"));
    if (!PyObject_GetAttrStringPtr) {
        return false;
    }

    PyUnicode_AsUTF8StringPtr = reinterpret_cast<PROC_PyUnicode_AsUTF8String>(
        Nelson::get_function(python3XX_handle, "PyUnicode_AsUTF8String"));
    if (!PyUnicode_AsUTF8StringPtr) {
        return false;
    }

    PyUnicode_AsUTF8Ptr = reinterpret_cast<PROC_PyUnicode_AsUTF8>(
        Nelson::get_function(python3XX_handle, "PyUnicode_AsUTF8"));
    if (!PyUnicode_AsUTF8Ptr) {
        return false;
    }

    PyEval_SaveThreadPtr = reinterpret_cast<PROC_PyEval_SaveThread>(
        Nelson::get_function(python3XX_handle, "PyEval_SaveThread"));
    if (!PyEval_SaveThreadPtr) {
        return false;
    }

    PyEval_RestoreThreadPtr = reinterpret_cast<PROC_PyEval_RestoreThread>(
        Nelson::get_function(python3XX_handle, "PyEval_RestoreThread"));
    if (!PyEval_RestoreThreadPtr) {
        return false;
    }

    PyModule_GetDictPtr = reinterpret_cast<PROC_PyModule_GetDict>(
        Nelson::get_function(python3XX_handle, "PyModule_GetDict"));
    if (!PyModule_GetDictPtr) {
        return false;
    }

    PyGILState_EnsurePtr = reinterpret_cast<PROC_PyGILState_Ensure>(
        Nelson::get_function(python3XX_handle, "PyGILState_Ensure"));
    if (!PyGILState_EnsurePtr) {
        return false;
    }

    PyErr_ClearPtr
        = reinterpret_cast<PROC_PyErr_Clear>(Nelson::get_function(python3XX_handle, "PyErr_Clear"));
    if (!PyErr_ClearPtr) {
        return false;
    }

    PyGILState_ReleasePtr = reinterpret_cast<PROC_PyGILState_Release>(
        Nelson::get_function(python3XX_handle, "PyGILState_Release"));
    if (!PyGILState_ReleasePtr) {
        return false;
    }

    PyObject_SetAttrStringPtr = reinterpret_cast<PROC_PyObject_SetAttrString>(
        Nelson::get_function(python3XX_handle, "PyObject_SetAttrString"));
    if (!PyObject_SetAttrStringPtr) {
        return false;
    }

    PyRun_SimpleFileExFlagsPtr = reinterpret_cast<PROC_PyRun_SimpleFileExFlags>(
        Nelson::get_function(python3XX_handle, "PyRun_SimpleFileExFlags"));
    if (!PyRun_SimpleFileExFlagsPtr) {
        return false;
    }

    PyObject_IsTruePtr = reinterpret_cast<PROC_PyObject_IsTrue>(
        Nelson::get_function(python3XX_handle, "PyObject_IsTrue"));
    if (!PyObject_IsTruePtr) {
        return false;
    }
    Py_DECREFPtr
        = reinterpret_cast<PROC_Py_DECREF>(Nelson::get_function(python3XX_handle, "_Py_DecRef"));
    if (!Py_DECREFPtr) {
        return false;
    }

    PyFloat_AsDoublePtr = reinterpret_cast<PROC_PyFloat_AsDouble>(
        Nelson::get_function(python3XX_handle, "PyFloat_AsDouble"));
    if (!PyFloat_AsDoublePtr) {
        return false;
    }

    PyLong_AsDoublePtr = reinterpret_cast<PROC_PyLong_AsDouble>(
        Nelson::get_function(python3XX_handle, "PyFloat_AsDouble"));
    if (!PyLong_AsDoublePtr) {
        return false;
    }

    PyErr_OccurredPtr = reinterpret_cast<PROC_PyErr_Occurred>(
        Nelson::get_function(python3XX_handle, "PyErr_Occurred"));
    if (!PyErr_OccurredPtr) {
        return false;
    }

    PyComplex_AsCComplexPtr = reinterpret_cast<PROC_PyComplex_AsCComplex>(
        Nelson::get_function(python3XX_handle, "PyComplex_AsCComplex"));
    if (!PyComplex_AsCComplexPtr) {
        return false;
    }

    PyObject_StrPtr = reinterpret_cast<PROC_PyObject_Str>(
        Nelson::get_function(python3XX_handle, "PyObject_Str"));
    if (!PyObject_StrPtr) {
        return false;
    }

    PyObject_DirPtr = reinterpret_cast<PROC_PyObject_Dir>(
        Nelson::get_function(python3XX_handle, "PyObject_Dir"));
    if (!PyObject_DirPtr) {
        return false;
    }

    PyList_SizePtr
        = reinterpret_cast<PROC_PyList_Size>(Nelson::get_function(python3XX_handle, "PyList_Size"));
    if (!PyList_SizePtr) {
        return false;
    }

    PyList_GetItemPtr = reinterpret_cast<PROC_PyList_GetItem>(
        Nelson::get_function(python3XX_handle, "PyList_GetItem"));
    if (!PyList_GetItemPtr) {
        return false;
    }

    PyCallable_CheckPtr = reinterpret_cast<PROC_PyCallable_Check>(
        Nelson::get_function(python3XX_handle, "PyCallable_Check"));
    if (!PyCallable_CheckPtr) {
        return false;
    }

    PyObject_CallNoArgsPtr = reinterpret_cast<PROC_PyObject_CallNoArgs>(
        Nelson::get_function(python3XX_handle, "PyObject_CallNoArgs"));
    if (!PyObject_CallNoArgsPtr) {
        return false;
    }

    PyErr_PrintPtr
        = reinterpret_cast<PROC_PyErr_Print>(Nelson::get_function(python3XX_handle, "PyErr_Print"));
    if (!PyErr_PrintPtr) {
        return false;
    }

    PyImport_ImportModulePtr = reinterpret_cast<PROC_PyImport_ImportModule>(
        Nelson::get_function(python3XX_handle, "PyImport_ImportModule"));
    if (!PyImport_ImportModulePtr) {
        return false;
    }

    PyObject_CallObjectPtr = reinterpret_cast<PROC_PyObject_CallObject>(
        Nelson::get_function(python3XX_handle, "PyObject_CallObject"));
    if (!PyObject_CallObjectPtr) {
        return false;
    }

    PyUnicode_FromStringPtr = reinterpret_cast<PROC_PyUnicode_FromString>(
        Nelson::get_function(python3XX_handle, "PyUnicode_FromString"));
    if (!PyUnicode_FromStringPtr) {
        return false;
    }

    PyDict_NewPtr
        = reinterpret_cast<PROC_PyDict_New>(Nelson::get_function(python3XX_handle, "PyDict_New"));
    if (!PyDict_NewPtr) {
        return false;
    }

    return true;
}
//=============================================================================
bool
loadPythonLibrary(const std::wstring& python3XXFullLibraryPathName)
{
    if (pythonLibraryLoaded) {
        return true;
    }
#ifdef _MSC_VER
    python3XX_handle = Nelson::load_dynamic_libraryW(python3XXFullLibraryPathName);
#else
    python3XX_handle
        = Nelson::load_dynamic_library(Nelson::wstring_to_utf8(python3XXFullLibraryPathName));
#endif
    if (!python3XX_handle) {
        return false;
    }
    pythonLibraryLoaded = loadPythonSymbols();
    return pythonLibraryLoaded;
}
//=============================================================================
bool
unloadPythonLibrary()
{
    Py_InitializePtr = nullptr;
    Py_FinalizePtr = nullptr;
    Py_IsInitializedPtr = nullptr;
    PyImport_AddModulePtr = nullptr;
    PyRun_SimpleStringFlagsPtr = nullptr;
    PyObject_GetAttrStringPtr = nullptr;
    PyUnicode_AsUTF8StringPtr = nullptr;
    PyUnicode_AsUTF8Ptr = nullptr;
    PyEval_SaveThreadPtr = nullptr;
    PyEval_RestoreThreadPtr = nullptr;
    PyModule_GetDictPtr = nullptr;
    PyGILState_EnsurePtr = nullptr;
    PyErr_ClearPtr = nullptr;
    PyGILState_ReleasePtr = nullptr;
    PyObject_SetAttrStringPtr = nullptr;
    PyRun_SimpleFileExFlagsPtr = nullptr;
    PyObject_IsTruePtr = nullptr;
    Py_DECREFPtr = nullptr;
    PyFloat_AsDoublePtr = nullptr;
    PyLong_AsDoublePtr = nullptr;
    PyErr_OccurredPtr = nullptr;
    PyComplex_AsCComplexPtr = nullptr;
    PyObject_StrPtr = nullptr;
    PyObject_DirPtr = nullptr;
    PyList_SizePtr = nullptr;
    PyList_GetItemPtr = nullptr;
    PyCallable_CheckPtr = nullptr;
    PyObject_CallNoArgsPtr = nullptr;
    PyErr_PrintPtr = nullptr;
    PyImport_ImportModulePtr = nullptr;
    PyObject_CallObjectPtr = nullptr;
    PyUnicode_FromStringPtr = nullptr;
    PyDict_NewPtr = nullptr;

    PyBool_TypePtr = nullptr;
    PyComplex_TypePtr = nullptr;
    PyFloat_TypePtr = nullptr;
    PyLong_TypePtr = nullptr;
    PyMemoryView_TypePtr = nullptr;
    PyUnicode_TypePtr = nullptr;
    _Py_NoneStructPtr = nullptr;

    pythonLibraryLoaded = false;
    return true;
}
//=============================================================================
void
NLSPy_Initialize()
{
    Py_InitializePtr();
}
//=============================================================================
void
NLSPy_Finalize()
{
    Py_FinalizePtr();
}
//=============================================================================
int
NLSPy_IsInitialized()
{
    return Py_IsInitializedPtr();
}
//=============================================================================
PyObject*
NLSPyImport_AddModule(const char* name)
{
    return PyImport_AddModulePtr(name);
}
//=============================================================================
int
NLSPyRun_SimpleStringFlags(const char* command, PyCompilerFlags* flags)
{
    return PyRun_SimpleStringFlagsPtr(command, flags);
}
//=============================================================================
PyObject*
NLSPyObject_GetAttrString(PyObject* o, const char* attr_name)
{
    return PyObject_GetAttrStringPtr(o, attr_name);
}
//=============================================================================
PyObject*
NLSPyUnicode_AsUTF8String(PyObject* unicode)
{
    return PyUnicode_AsUTF8StringPtr(unicode);
}
//=============================================================================
const char*
NLSPyUnicode_AsUTF8(PyObject* unicode)
{
    return PyUnicode_AsUTF8Ptr(unicode);
}
//=============================================================================
PyThreadState*
NLSPyEval_SaveThread()
{
    return PyEval_SaveThreadPtr();
}
//=============================================================================
void
NLSPyEval_RestoreThread(PyThreadState* tstate)
{
    PyEval_RestoreThreadPtr(tstate);
}
//=============================================================================
PyObject*
NLSPyModule_GetDict(PyObject* moduleo)
{
    return PyModule_GetDictPtr(moduleo);
}
//=============================================================================
PyGILState_STATE
NLSPyGILState_Ensure()
{
    return PyGILState_EnsurePtr();
}
//=============================================================================
void
NLSPyErr_Clear()
{
    PyErr_ClearPtr();
}
//=============================================================================
void
NLSPyGILState_Release(PyGILState_STATE state)
{
    PyGILState_ReleasePtr(state);
}
//=============================================================================
int
NLSPyObject_SetAttrString(PyObject* o, const char* attr_name, PyObject* v)
{
    return PyObject_SetAttrStringPtr(o, attr_name, v);
}
//=============================================================================
int
NLSPyRun_SimpleFileExFlags(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags)
{
    return PyRun_SimpleFileExFlagsPtr(fp, filename, closeit, flags);
}
//=============================================================================
int
NLSPyObject_IsTrue(PyObject* ob)
{
    return PyObject_IsTruePtr(ob);
}
//=============================================================================
void
NLSPy_DECREF(PyObject* op)
{
    return Py_DECREFPtr(op);
}
//=============================================================================
double
NLSPyFloat_AsDouble(PyObject* pyfloat)
{
    return PyFloat_AsDoublePtr(pyfloat);
}
//=============================================================================
double
NLSPyLong_AsDouble(PyObject* pylong)
{
    return PyLong_AsDoublePtr(pylong);
}
//=============================================================================
PyObject*
NLSPyErr_Occurred()
{
    return PyErr_OccurredPtr();
}
//=============================================================================
Py_complex
NLSPyComplex_AsCComplex(PyObject* op)
{
    return PyComplex_AsCComplexPtr(op);
}
//=============================================================================
PyObject*
NLSPyObject_Str(PyObject* o)
{
    return PyObject_StrPtr(o);
}
//=============================================================================
PyObject*
NLSPyObject_Dir(PyObject* o)
{
    return PyObject_DirPtr(o);
}
//=============================================================================
Py_ssize_t
NLSPyList_Size(PyObject* o)
{
    return PyList_SizePtr(o);
}
//=============================================================================
PyObject*
NLSPyList_GetItem(PyObject* o, Py_ssize_t s)
{
    return PyList_GetItemPtr(o, s);
}
//=============================================================================
int
NLSPyCallable_Check(PyObject* o)
{
    return PyCallable_CheckPtr(o);
}
//=============================================================================
PyObject*
NLSPyObject_CallNoArgs(PyObject* func)
{
    return PyObject_CallNoArgsPtr(func);
}
//=============================================================================
void
NLSPyErr_Print()
{
    return PyErr_PrintPtr();
}
//=============================================================================
PyObject*
NLSPyImport_ImportModule(const char* name)
{
    return PyImport_ImportModulePtr(name);
}
//=============================================================================
PyObject*
NLSPyObject_CallObject(PyObject* callable, PyObject* args)
{
    return PyObject_CallObjectPtr(callable, args);
}
//=============================================================================
PyObject*
NLSPyUnicode_FromString(const char* str)
{
    return PyUnicode_FromStringPtr(str);
}
//=============================================================================
PyObject*
NLSPyDict_New()
{
    return PyDict_NewPtr();
}
//=============================================================================
