//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "PythonConfig.hpp"
#include "nlsPython_engine_exports.h"
//=============================================================================
extern PyTypeObject* PyBool_TypePtr;
extern PyTypeObject* PyComplex_TypePtr;
extern PyTypeObject* PyFloat_TypePtr;
extern PyTypeObject* PyLong_TypePtr;
extern PyTypeObject* PyMemoryView_TypePtr;
extern PyTypeObject* PyUnicode_TypePtr;
extern PyTypeObject* PyDict_TypePtr;
//=============================================================================
extern PyObject* _Py_NoneStructPtr;
//=============================================================================
#define PyBool_Type *PyBool_TypePtr
#define PyComplex_Type *PyComplex_TypePtr
#define PyFloat_Type *PyFloat_TypePtr
#define PyLong_Type *PyLong_TypePtr
#define PyMemoryView_Type *PyMemoryView_TypePtr
#define PyUnicode_Type *PyUnicode_TypePtr
#define PyDict_Type *PyDict_TypePtr
//=============================================================================
#define _Py_NoneStruct *_Py_NoneStructPtr
//=============================================================================
bool
isPythonLibraryLoaded();
//=============================================================================
bool
loadPythonLibrary(const std::wstring& python3XXFullLibraryPathName);
//=============================================================================
bool
unloadPythonLibrary();
//=============================================================================
void
NLSPy_Initialize();
//=============================================================================
void
NLSPy_Finalize();
//=============================================================================
int
NLSPy_IsInitialized();
//=============================================================================
PyObject*
NLSPyImport_AddModule(const char* name);
//=============================================================================
int
NLSPyRun_SimpleStringFlags(const char* command, PyCompilerFlags* flags);
//=============================================================================
PyObject*
NLSPyObject_GetAttrString(PyObject* o, const char* attr_name);
//=============================================================================
PyObject*
NLSPyUnicode_AsUTF8String(PyObject* unicode);
//=============================================================================
const char*
NLSPyUnicode_AsUTF8(PyObject* unicode);
//=============================================================================
PyThreadState*
NLSPyEval_SaveThread();
//=============================================================================
void
NLSPyEval_RestoreThread(PyThreadState* tstate);
//=============================================================================
PyObject*
NLSPyModule_GetDict(PyObject* moduleo);
//=============================================================================
PyGILState_STATE
NLSPyGILState_Ensure();
//=============================================================================
void
NLSPyErr_Clear();
//=============================================================================
void
NLSPyGILState_Release(PyGILState_STATE state);
//=============================================================================
int
NLSPyObject_SetAttrString(PyObject* o, const char* attr_name, PyObject* v);
//=============================================================================
int
NLSPyRun_SimpleFileExFlags(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags);
//=============================================================================
int
NLSPyObject_IsTrue(PyObject* ob);
//=============================================================================
void
NLSPy_DECREF(PyObject* op);
//=============================================================================
double
NLSPyFloat_AsDouble(PyObject* pyfloat);
//=============================================================================
double
NLSPyLong_AsDouble(PyObject* pylong);
//=============================================================================
PyObject*
NLSPyErr_Occurred();
//=============================================================================
Py_complex
NLSPyComplex_AsCComplex(PyObject* op);
//=============================================================================
PyObject*
NLSPyObject_Str(PyObject* o);
//=============================================================================
PyObject*
NLSPyObject_Dir(PyObject* o);
//=============================================================================
Py_ssize_t
NLSPyList_Size(PyObject* o);
//=============================================================================
PyObject*
NLSPyList_GetItem(PyObject* o, Py_ssize_t s);
//=============================================================================
int
NLSPyCallable_Check(PyObject* o);
//=============================================================================
PyObject*
NLSPyObject_GetAttrString(PyObject* o, const char* m);
//=============================================================================
PyObject*
NLSPyObject_CallNoArgs(PyObject* func);
//=============================================================================
void
NLSPyErr_Print();
//=============================================================================
PyObject*
NLSPyImport_ImportModule(const char* name);
//=============================================================================
PyObject*
NLSPyObject_CallObject(PyObject* callable, PyObject* args);
//=============================================================================
PyObject*
NLSPyUnicode_FromString(const char* str);
//=============================================================================
PyObject*
NLSPyDict_New();
//=============================================================================
