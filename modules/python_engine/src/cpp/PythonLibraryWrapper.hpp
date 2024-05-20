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
#include <cstdio>
#include "PythonConfig.hpp"
#include "nlsPython_engine_exports.h"
//=============================================================================
extern PyTypeObject* PyBool_TypePtr;
extern PyTypeObject* PyComplex_TypePtr;
extern PyTypeObject* PyFloat_TypePtr;
extern PyTypeObject* PyLong_TypePtr;
extern PyTypeObject* PyBytes_TypePtr;
extern PyTypeObject* PyByteArray_TypePtr;
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
#define PyBytes_Type *PyBytes_TypePtr
#define PyByteArray_Type *PyByteArray_TypePtr
#define PyMemoryView_Type *PyMemoryView_TypePtr
#define PyUnicode_Type *PyUnicode_TypePtr
#define PyDict_Type *PyDict_TypePtr
//=============================================================================
#define _Py_NoneStruct *_Py_NoneStructPtr
#define Py_None (&_Py_NoneStruct)
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
NLSPyObject_IsTrue(PyObject* ob);
//=============================================================================
void
NLSPy_DECREF(PyObject* op);
//=============================================================================
void
NLSPy_INCREF(PyObject* op);
//=============================================================================
void
NLSPy_XDECREF(PyObject* op);
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
PyObject*
NLSPyFloat_FromDouble(double v);
//=============================================================================
PyObject*
NLSPy_BuildValue(const char* format, ...);
//=============================================================================
PyObject*
NLSPyObject_CallMethodOneItem(PyObject* obj, const char* name, const char* format, PyObject* item);
//=============================================================================
int
NLSPyObject_IsInstance(PyObject* object, PyObject* typeorclass);
//=============================================================================
char*
NLSPyBytes_AsString(PyObject*);
//=============================================================================
PyObject*
NLSPySequence_GetItem(PyObject* o, Py_ssize_t i);
//=============================================================================
Py_ssize_t
NLSPySequence_Size(PyObject* o);
//=============================================================================
PyObject*
NLSPyObject_Repr(PyObject*);
//=============================================================================
int
NLSPyBuffer_FillInfo(
    Py_buffer* view, PyObject* o, void* buf, Py_ssize_t len, int readonly, int flags);
//=============================================================================
PyObject*
NLSPyMemoryView_FromBuffer(const Py_buffer* info);
//=============================================================================
void
NLSPyBuffer_Release(Py_buffer* view);
//=============================================================================
PyObject*
NLSPyTuple_New(Py_ssize_t size);
//=============================================================================
PyObject* NLSPyLong_FromSize_t(size_t);
//=============================================================================
int
NLSPyTuple_SetItem(PyObject* p, Py_ssize_t pos, PyObject* o);
//=============================================================================
int
NLSPyObject_GetBuffer(PyObject* obj, Py_buffer* view, int flags);
//=============================================================================
PyObject*
NLSPyComplex_FromCComplex(Py_complex cplx);
//=============================================================================
PyObject*
NLSPyLong_FromLongLong(long long);
//=============================================================================
PyObject*
NLSPyLong_FromUnsignedLongLong(unsigned long long);
//=============================================================================
PyObject*
NLSPyBool_FromLong(long v);
//=============================================================================
PyObject*
NLSPyLong_FromUnsignedLong(unsigned long);
//=============================================================================
PyObject*
NLSPyLong_FromLong(long v);
//=============================================================================
PyObject*
NLSPyObject_CallMethodOneArg(PyObject* self, PyObject* name, PyObject* arg);
//=============================================================================
PyObject*
NLSPyObject_VectorcallMethod(
    PyObject* name, PyObject* const* args, size_t nargsf, PyObject* kwnames);
//=============================================================================
long long
NLSPyLong_AsLongLong(PyObject*);
//=============================================================================
unsigned long long
NLSPyLong_AsUnsignedLongLong(PyObject*);
//=============================================================================
double
NLSPyComplex_RealAsDouble(PyObject* op);
//=============================================================================
double
NLSPyComplex_ImagAsDouble(PyObject* op);
//=============================================================================
Py_ssize_t
NLSPyTuple_Size(PyObject*);
//=============================================================================
PyObject*
NLSPyTuple_GetItem(PyObject*, Py_ssize_t);
//=============================================================================
PyObject*
NLSPyEval_GetBuiltins(void);
//=============================================================================
PyObject*
NLSPyDict_GetItemString(PyObject* dp, const char* key);
//=============================================================================
int
NLSPyDict_SetItemString(PyObject* dp, const char* key, PyObject* item);
//=============================================================================
PyObject*
NLSPyDict_Keys(PyObject* mp);
//=============================================================================
int
NLSPyBytes_AsStringAndSize(PyObject* obj, char** s, Py_ssize_t* len);
//=============================================================================
char*
NLSPyByteArray_AsString(PyObject* obj);
//=============================================================================
PyObject*
NLSPyEval_EvalCode(PyObject* co, PyObject* globals, PyObject* locals);
//=============================================================================
PyObject*
NLSPyList_New(Py_ssize_t size);
//=============================================================================
int
NLSPyList_SetItem(PyObject* ob1, Py_ssize_t sz, PyObject* ob2);
//=============================================================================
PyObject*
NLSPyUnicode_DecodeFSDefault(const char* s);
//=============================================================================
int
NLSPyRun_SimpleFileExFlags(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags);
//=============================================================================
FILE*
NLS_Py_fopen_obj(PyObject* path, const char* mode);
//=============================================================================
PyObject*
NLSPyDict_Copy(PyObject* mp);
//=============================================================================
void
NLSPyDict_Clear(PyObject* mp);
//=============================================================================
int
NLSPyDict_Update(PyObject* mp, PyObject* other);
//=============================================================================
PyObject*
NLSPyNumber_Add(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_Subtract(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_Negative(PyObject* o);
//=============================================================================
PyObject*
NLSPyNumber_Positive(PyObject* o);
//=============================================================================
PyObject*
NLSPyNumber_Multiply(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_MatrixMultiply(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_TrueDivide(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_Remainder(PyObject* o1, PyObject* o2);
//=============================================================================
PyObject*
NLSPyNumber_Power(PyObject* o1, PyObject* o2, PyObject* o3);
//=============================================================================
int
NLSPyObject_RichCompareBool(PyObject* o1, PyObject* o2, int opid);
//=============================================================================
int
NLSPyDict_SetItem(PyObject* mp, PyObject* key, PyObject* item);
//=============================================================================
PyObject*
NLSPyDict_GetItem(PyObject* mp, PyObject* key);
//=============================================================================
