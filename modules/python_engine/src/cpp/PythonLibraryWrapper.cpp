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
#include <cstdarg>
#include <unordered_map>
//=============================================================================
PyTypeObject* PyBool_TypePtr = nullptr;
PyTypeObject* PyComplex_TypePtr = nullptr;
PyTypeObject* PyFloat_TypePtr = nullptr;
PyTypeObject* PyLong_TypePtr = nullptr;
PyTypeObject* PyBytes_TypePtr = nullptr;
PyTypeObject* PyByteArray_TypePtr = nullptr;
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
using PROC_Py_Finalize = void (*)();
using PROC_Py_IsInitialized = int (*)();
using PROC_PyImport_AddModule = PyObject* (*)(const char* name);
using PROC_PyRun_SimpleStringFlags = int (*)(const char* command, PyCompilerFlags* flags);
using PROC_PyObject_GetAttrString = PyObject* (*)(PyObject* o, const char* attr_name);
using PROC_PyUnicode_AsUTF8String = PyObject* (*)(PyObject* unicode);
using PROC_PyUnicode_AsUTF8 = const char* (*)(PyObject* unicode);
using PROC_PyEval_SaveThread = PyThreadState* (*)();
using PROC_PyEval_RestoreThread = void (*)(PyThreadState* tstate);
using PROC_PyModule_GetDict = PyObject* (*)(PyObject* moduleo);
using PROC_PyGILState_Ensure = PyGILState_STATE (*)();
using PROC_PyErr_Clear = void (*)();
using PROC_PyGILState_Release = void (*)(PyGILState_STATE state);
using PROC_PyObject_SetAttrString = int (*)(PyObject* o, const char* attr_name, PyObject* v);
using PROC_PyObject_IsTrue = int (*)(PyObject* ob);
using PROC_Py_DECREF = void (*)(PyObject* op);
using PROC_Py_INCREF = void (*)(PyObject* op);
using PROC_PyFloat_AsDouble = double (*)(PyObject* pyfloat);
using PROC_PyLong_AsDouble = double (*)(PyObject* pylong);
using PROC_PyErr_Occurred = PyObject* (*)(void);
using PROC_PyComplex_AsCComplex = Py_complex (*)(PyObject* op);
using PROC_PyObject_Str = PyObject* (*)(PyObject* o);
using PROC_PyObject_Dir = PyObject* (*)(PyObject* o);
using PROC_PyList_Size = Py_ssize_t (*)(PyObject* o);
using PROC_PyList_GetItem = PyObject* (*)(PyObject* o, Py_ssize_t s);
using PROC_PyCallable_Check = int (*)(PyObject* o);
using PROC_PyObject_CallNoArgs = PyObject* (*)(PyObject* func);
using PROC_PyErr_Print = void (*)();
using PROC_PyImport_ImportModule = PyObject* (*)(const char* name);
using PROC_PyObject_CallObject = PyObject* (*)(PyObject* callable, PyObject* args);
using PROC_PyUnicode_FromString = PyObject* (*)(const char* str);
using PROC_PyDict_New = PyObject* (*)();
using PROC_PyFloat_FromDouble = PyObject* (*)(double v);
using PROC_Py_VaBuildValue = PyObject* (*)(const char*, va_list vargs);
using PROC_PyObject_CallOneArg = PyObject* (*)(PyObject* callable, PyObject* arg);
using PROC_PyObject_CallMethod
    = PyObject* (*)(PyObject* obj, const char* name, const char* format, ...);
using PROC_PyObject_IsInstance = int (*)(PyObject* object, PyObject* typeorclass);
using PROC_PyBytes_AsString = char* (*)(PyObject* o);
using PROC_PySequence_GetItem = PyObject* (*)(PyObject* o, Py_ssize_t i);
using PROC_PySequence_Size = Py_ssize_t (*)(PyObject* o);
using PROC_PyObject_Repr = PyObject* (*)(PyObject* o);
using PROC_PyBuffer_FillInfo
    = int (*)(Py_buffer* view, PyObject* o, void* buf, Py_ssize_t len, int readonly, int flags);
using PROC_PyMemoryView_FromBuffer = PyObject* (*)(const Py_buffer* info);
using PROC_PyBuffer_Release = void (*)(Py_buffer* view);
using PROC_PyTuple_New = PyObject* (*)(Py_ssize_t size);
using PROC_PyLong_FromSize_t = PyObject* (*)(size_t sz);
using PROC_PyTuple_SetItem = int (*)(PyObject* p, Py_ssize_t pos, PyObject* o);
using PROC_PyObject_GetBuffer = int (*)(PyObject* obj, Py_buffer* view, int flags);
using PROC_PyComplex_FromCComplex = PyObject* (*)(Py_complex cplx);
using PROC_PyLong_FromLongLong = PyObject* (*)(long long v);
using PROC_PyLong_FromUnsignedLongLong = PyObject* (*)(unsigned long long);
using PROC_PyBool_FromLong = PyObject* (*)(long v);
using PROC_PyLong_FromUnsignedLong = PyObject* (*)(unsigned long v);
using PROC_PyLong_FromLong = PyObject* (*)(long v);
using PROC_PyObject_VectorcallMethod
    = PyObject* (*)(PyObject* name, PyObject* const* args, size_t nargsf, PyObject* kwnames);
using PROC_PyLong_AsLongLong = long long (*)(PyObject*);
using PROC_PyLong_AsUnsignedLongLong = unsigned long long (*)(PyObject*);
using PROC_PyComplex_RealAsDouble = double (*)(PyObject* op);
using PROC_PyComplex_ImagAsDouble = double (*)(PyObject* op);
using PROC_PyTuple_Size = Py_ssize_t (*)(PyObject* o);
using PROC_PyTuple_GetItem = PyObject* (*)(PyObject* o, Py_ssize_t sz);
using PROC_PyEval_GetBuiltins = PyObject* (*)(void);
using PROC_PyDict_GetItemString = PyObject* (*)(PyObject* dp, const char* key);
using PROC_PyDict_SetItemString = int (*)(PyObject* dp, const char* key, PyObject* item);
using PROC_PyDict_Keys = PyObject* (*)(PyObject* mp);
using PROC_PyBytes_AsStringAndSize = int (*)(PyObject* obj, char** s, Py_ssize_t* len);
using PROC_PyByteArray_AsString = char* (*)(PyObject* obj);
using PROC_PyEval_EvalCode = PyObject* (*)(PyObject* co, PyObject* globals, PyObject* locals);
using PROC_PyList_New = PyObject* (*)(Py_ssize_t size);
using PROC_PyList_SetItem = int (*)(PyObject* ob1, Py_ssize_t sz, PyObject* ob2);
using PROC_PyUnicode_DecodeFSDefault = PyObject* (*)(const char* s);
using PROC_PyRun_SimpleFileExFlags
    = int (*)(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags);
using PROC_Py_fopen_obj = FILE* (*)(PyObject* path, const char* mode);
using PROC_PyDict_Copy = PyObject* (*)(PyObject* mp);
using PROC_PyDict_Clear = void (*)(PyObject* mp);
using PROC_PyDict_Update = int (*)(PyObject* mp, PyObject* other);
using PROC_PyObject_GetAttr = PyObject* (*)(PyObject* pyObjA, PyObject* pyObjB);
using PROC_PyObject_CallFunctionObjArgs = PyObject* (*)(PyObject* callable, ...);
using PROC_PyNumber_Add = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_Subtract = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_Multiply = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_MatrixMultiply = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_TrueDivide = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_Remainder = PyObject* (*)(PyObject* o1, PyObject* o2);
using PROC_PyNumber_Power = PyObject* (*)(PyObject* o1, PyObject* o2, PyObject* o3);
using PROC_PyNumber_Negative = PyObject* (*)(PyObject* o);
using PROC_PyNumber_Positive = PyObject* (*)(PyObject* o);
using PROC_PyObject_RichCompareBool = int (*)(PyObject* o1, PyObject* o2, int opid);
using PROC_PyDict_SetItem = int (*)(PyObject* mp, PyObject* key, PyObject* item);
using PROC_PyDict_GetItem = PyObject* (*)(PyObject* mp, PyObject* key);
//=============================================================================
static std::unordered_map<std::string, void*> pythonSymbols;
//=============================================================================
#define LOAD_PYTHON_SYMBOL(symbol_name)                                                            \
    pythonSymbols[#symbol_name]                                                                    \
        = reinterpret_cast<void*>(Nelson::get_function(python3XX_handle, #symbol_name));           \
    if (!pythonSymbols[#symbol_name]) {                                                            \
        printf("Python Symbol not found: %s\n", #symbol_name);                                     \
        return false;                                                                              \
    }
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
    LOAD_PYTHON_SYMBOL(PyBool_Type);
    LOAD_PYTHON_SYMBOL(PyComplex_Type);
    LOAD_PYTHON_SYMBOL(PyFloat_Type);
    LOAD_PYTHON_SYMBOL(PyLong_Type);
    LOAD_PYTHON_SYMBOL(PyBytes_Type);
    LOAD_PYTHON_SYMBOL(PyByteArray_Type);
    LOAD_PYTHON_SYMBOL(PyMemoryView_Type);
    LOAD_PYTHON_SYMBOL(PyUnicode_Type);
    LOAD_PYTHON_SYMBOL(PyDict_Type);
    LOAD_PYTHON_SYMBOL(_Py_NoneStruct);
    //=============================================================================
    PyBool_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyBool_Type"]);
    PyComplex_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyComplex_Type"]);
    PyFloat_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyFloat_Type"]);
    PyLong_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyLong_Type"]);
    PyBytes_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyBytes_Type"]);
    PyByteArray_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyByteArray_Type"]);
    PyMemoryView_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyMemoryView_Type"]);
    PyUnicode_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyUnicode_Type"]);
    PyDict_TypePtr = reinterpret_cast<PyTypeObject*>(pythonSymbols["PyDict_Type"]);
    //=============================================================================
    _Py_NoneStructPtr = reinterpret_cast<PyObject*>(pythonSymbols["_Py_NoneStruct"]);
    //=============================================================================
    LOAD_PYTHON_SYMBOL(Py_Initialize);
    LOAD_PYTHON_SYMBOL(Py_Finalize);
    LOAD_PYTHON_SYMBOL(Py_IsInitialized);
    LOAD_PYTHON_SYMBOL(PyImport_AddModule);
    LOAD_PYTHON_SYMBOL(PyRun_SimpleStringFlags);
    LOAD_PYTHON_SYMBOL(PyObject_GetAttrString);
    LOAD_PYTHON_SYMBOL(PyUnicode_AsUTF8String);
    LOAD_PYTHON_SYMBOL(PyUnicode_AsUTF8);
    LOAD_PYTHON_SYMBOL(PyEval_SaveThread);
    LOAD_PYTHON_SYMBOL(PyEval_RestoreThread);
    LOAD_PYTHON_SYMBOL(PyModule_GetDict);
    LOAD_PYTHON_SYMBOL(PyGILState_Ensure);
    LOAD_PYTHON_SYMBOL(PyErr_Clear);
    LOAD_PYTHON_SYMBOL(PyGILState_Release);
    LOAD_PYTHON_SYMBOL(PyObject_SetAttrString);
    LOAD_PYTHON_SYMBOL(PyRun_SimpleFileExFlags);
    LOAD_PYTHON_SYMBOL(PyObject_IsTrue);
    LOAD_PYTHON_SYMBOL(_Py_DecRef);
    LOAD_PYTHON_SYMBOL(_Py_IncRef);
    LOAD_PYTHON_SYMBOL(PyFloat_AsDouble);
    LOAD_PYTHON_SYMBOL(PyLong_AsDouble);
    LOAD_PYTHON_SYMBOL(PyErr_Occurred);
    LOAD_PYTHON_SYMBOL(PyComplex_AsCComplex);
    LOAD_PYTHON_SYMBOL(PyObject_Str);
    LOAD_PYTHON_SYMBOL(PyObject_Dir);
    LOAD_PYTHON_SYMBOL(PyList_Size);
    LOAD_PYTHON_SYMBOL(PyList_GetItem);
    LOAD_PYTHON_SYMBOL(PyCallable_Check);
    LOAD_PYTHON_SYMBOL(PyObject_CallNoArgs);
    LOAD_PYTHON_SYMBOL(PyErr_Print);
    LOAD_PYTHON_SYMBOL(PyImport_ImportModule);
    LOAD_PYTHON_SYMBOL(PyObject_CallObject);
    LOAD_PYTHON_SYMBOL(PyUnicode_FromString);
    LOAD_PYTHON_SYMBOL(PyDict_New);
    LOAD_PYTHON_SYMBOL(PyFloat_FromDouble);
    LOAD_PYTHON_SYMBOL(Py_VaBuildValue);
    LOAD_PYTHON_SYMBOL(PyObject_CallMethod);
    LOAD_PYTHON_SYMBOL(PyObject_IsInstance);
    LOAD_PYTHON_SYMBOL(PyBytes_AsString);
    LOAD_PYTHON_SYMBOL(PySequence_GetItem);
    LOAD_PYTHON_SYMBOL(PySequence_Size);
    LOAD_PYTHON_SYMBOL(PyObject_Repr);
    LOAD_PYTHON_SYMBOL(PyBuffer_FillInfo);
    LOAD_PYTHON_SYMBOL(PyMemoryView_FromBuffer);
    LOAD_PYTHON_SYMBOL(PyBuffer_Release);
    LOAD_PYTHON_SYMBOL(PyTuple_New);
    LOAD_PYTHON_SYMBOL(PyLong_FromSize_t);
    LOAD_PYTHON_SYMBOL(PyTuple_SetItem);
    LOAD_PYTHON_SYMBOL(PyObject_GetBuffer);
    LOAD_PYTHON_SYMBOL(PyComplex_FromCComplex);
    LOAD_PYTHON_SYMBOL(PyLong_FromLongLong);
    LOAD_PYTHON_SYMBOL(PyLong_FromLong);
    LOAD_PYTHON_SYMBOL(PyLong_FromUnsignedLongLong);
    LOAD_PYTHON_SYMBOL(PyBool_FromLong);
    LOAD_PYTHON_SYMBOL(PyLong_FromUnsignedLong);
    LOAD_PYTHON_SYMBOL(PyObject_VectorcallMethod);
    LOAD_PYTHON_SYMBOL(PyLong_AsLongLong);
    LOAD_PYTHON_SYMBOL(PyLong_AsUnsignedLongLong);
    LOAD_PYTHON_SYMBOL(PyComplex_RealAsDouble);
    LOAD_PYTHON_SYMBOL(PyComplex_ImagAsDouble);
    LOAD_PYTHON_SYMBOL(PyTuple_Size);
    LOAD_PYTHON_SYMBOL(PyTuple_GetItem);
    LOAD_PYTHON_SYMBOL(PyEval_GetBuiltins);
    LOAD_PYTHON_SYMBOL(PyDict_GetItemString);
    LOAD_PYTHON_SYMBOL(PyDict_SetItemString);
    LOAD_PYTHON_SYMBOL(PyDict_Keys);
    LOAD_PYTHON_SYMBOL(PyBytes_AsStringAndSize);
    LOAD_PYTHON_SYMBOL(PyByteArray_AsString);
    LOAD_PYTHON_SYMBOL(PyEval_EvalCode);
    LOAD_PYTHON_SYMBOL(PyList_New);
    LOAD_PYTHON_SYMBOL(PyList_SetItem);
    LOAD_PYTHON_SYMBOL(PyUnicode_DecodeFSDefault);
    LOAD_PYTHON_SYMBOL(PyRun_SimpleFileExFlags);
    LOAD_PYTHON_SYMBOL(_Py_fopen_obj);
    LOAD_PYTHON_SYMBOL(PyDict_Copy);
    LOAD_PYTHON_SYMBOL(PyDict_Clear);
    LOAD_PYTHON_SYMBOL(PyDict_Update);

    LOAD_PYTHON_SYMBOL(PyNumber_Add);
    LOAD_PYTHON_SYMBOL(PyNumber_Subtract);
    LOAD_PYTHON_SYMBOL(PyNumber_Multiply);
    LOAD_PYTHON_SYMBOL(PyNumber_MatrixMultiply);
    LOAD_PYTHON_SYMBOL(PyNumber_TrueDivide);
    LOAD_PYTHON_SYMBOL(PyNumber_Remainder);
    LOAD_PYTHON_SYMBOL(PyNumber_Power);
    LOAD_PYTHON_SYMBOL(PyNumber_Negative);
    LOAD_PYTHON_SYMBOL(PyNumber_Positive);

    LOAD_PYTHON_SYMBOL(PyObject_GetAttr);
    LOAD_PYTHON_SYMBOL(PyObject_CallFunctionObjArgs);
    LOAD_PYTHON_SYMBOL(PyObject_RichCompareBool);
    LOAD_PYTHON_SYMBOL(PyDict_SetItem);
    LOAD_PYTHON_SYMBOL(PyDict_GetItem);
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
    PyBool_TypePtr = nullptr;
    PyComplex_TypePtr = nullptr;
    PyFloat_TypePtr = nullptr;
    PyLong_TypePtr = nullptr;
    PyBytes_TypePtr = nullptr;
    PyByteArray_TypePtr = nullptr;
    PyMemoryView_TypePtr = nullptr;
    PyUnicode_TypePtr = nullptr;
    _Py_NoneStructPtr = nullptr;

    pythonSymbols.clear();

    pythonLibraryLoaded = false;
    return true;
}
//=============================================================================
void
NLSPy_Initialize()
{
    reinterpret_cast<PROC_Py_Initialize>(pythonSymbols["Py_Initialize"])();
}
//=============================================================================
void
NLSPy_Finalize()
{
    reinterpret_cast<PROC_Py_Finalize>(pythonSymbols["Py_Finalize"])();
}
//=============================================================================
int
NLSPy_IsInitialized()
{
    return reinterpret_cast<PROC_Py_IsInitialized>(pythonSymbols["Py_IsInitialized"])();
}
//=============================================================================
PyObject*
NLSPyImport_AddModule(const char* name)
{
    return reinterpret_cast<PROC_PyImport_AddModule>(pythonSymbols["PyImport_AddModule"])(name);
}
//=============================================================================
int
NLSPyRun_SimpleStringFlags(const char* command, PyCompilerFlags* flags)
{
    return reinterpret_cast<PROC_PyRun_SimpleStringFlags>(pythonSymbols["PyRun_SimpleStringFlags"])(
        command, flags);
}
//=============================================================================
PyObject*
NLSPyObject_GetAttrString(PyObject* o, const char* attr_name)
{
    return reinterpret_cast<PROC_PyObject_GetAttrString>(pythonSymbols["PyObject_GetAttrString"])(
        o, attr_name);
}
//=============================================================================
PyObject*
NLSPyUnicode_AsUTF8String(PyObject* unicode)
{
    return reinterpret_cast<PROC_PyUnicode_AsUTF8String>(pythonSymbols["PyUnicode_AsUTF8String"])(
        unicode);
}
//=============================================================================
const char*
NLSPyUnicode_AsUTF8(PyObject* unicode)
{
    return reinterpret_cast<PROC_PyUnicode_AsUTF8>(pythonSymbols["PyUnicode_AsUTF8"])(unicode);
}
//=============================================================================
PyThreadState*
NLSPyEval_SaveThread()
{
    return reinterpret_cast<PROC_PyEval_SaveThread>(pythonSymbols["PyEval_SaveThread"])();
}
//=============================================================================
void
NLSPyEval_RestoreThread(PyThreadState* tstate)
{
    return reinterpret_cast<PROC_PyEval_RestoreThread>(pythonSymbols["PyEval_RestoreThread"])(
        tstate);
}
//=============================================================================
PyObject*
NLSPyModule_GetDict(PyObject* moduleo)
{
    return reinterpret_cast<PROC_PyModule_GetDict>(pythonSymbols["PyModule_GetDict"])(moduleo);
}
//=============================================================================
PyGILState_STATE
NLSPyGILState_Ensure()
{
    return reinterpret_cast<PROC_PyGILState_Ensure>(pythonSymbols["PyGILState_Ensure"])();
}
//=============================================================================
void
NLSPyErr_Clear()
{
    return reinterpret_cast<PROC_PyErr_Clear>(pythonSymbols["PyErr_Clear"])();
}
//=============================================================================
void
NLSPyGILState_Release(PyGILState_STATE state)
{
    return reinterpret_cast<PROC_PyGILState_Release>(pythonSymbols["PyGILState_Release"])(state);
}
//=============================================================================
int
NLSPyObject_SetAttrString(PyObject* o, const char* attr_name, PyObject* v)
{
    return reinterpret_cast<PROC_PyObject_SetAttrString>(pythonSymbols["PyObject_SetAttrString"])(
        o, attr_name, v);
}
//=============================================================================
int
NLSPyObject_IsTrue(PyObject* ob)
{
    return reinterpret_cast<PROC_PyObject_IsTrue>(pythonSymbols["PyObject_IsTrue"])(ob);
}
//=============================================================================
void
NLSPy_DECREF(PyObject* op)
{
    reinterpret_cast<PROC_Py_DECREF>(pythonSymbols["_Py_DecRef"])(op);
}
//=============================================================================
void
NLSPy_INCREF(PyObject* op)
{
    reinterpret_cast<PROC_Py_INCREF>(pythonSymbols["_Py_IncRef"])(op);
}
//=============================================================================
void
NLSPy_XDECREF(PyObject* op)
{
    if (op != NULL) {
        NLSPy_DECREF(op);
    }
}
//=============================================================================
double
NLSPyFloat_AsDouble(PyObject* pyfloat)
{
    return reinterpret_cast<PROC_PyFloat_AsDouble>(pythonSymbols["PyFloat_AsDouble"])(pyfloat);
}
//=============================================================================
double
NLSPyLong_AsDouble(PyObject* pylong)
{
    return reinterpret_cast<PROC_PyLong_AsDouble>(pythonSymbols["PyLong_AsDouble"])(pylong);
}
//=============================================================================
PyObject*
NLSPyErr_Occurred()
{
    return reinterpret_cast<PROC_PyErr_Occurred>(pythonSymbols["PyErr_Occurred"])();
}
//=============================================================================
Py_complex
NLSPyComplex_AsCComplex(PyObject* op)
{
    return reinterpret_cast<PROC_PyComplex_AsCComplex>(pythonSymbols["PyComplex_AsCComplex"])(op);
}
//=============================================================================
PyObject*
NLSPyObject_Str(PyObject* o)
{
    return reinterpret_cast<PROC_PyObject_Str>(pythonSymbols["PyObject_Str"])(o);
}
//=============================================================================
PyObject*
NLSPyObject_Dir(PyObject* o)
{
    return reinterpret_cast<PROC_PyObject_Dir>(pythonSymbols["PyObject_Dir"])(o);
}
//=============================================================================
Py_ssize_t
NLSPyList_Size(PyObject* o)
{
    return reinterpret_cast<PROC_PyList_Size>(pythonSymbols["PyList_Size"])(o);
}
//=============================================================================
PyObject*
NLSPyList_GetItem(PyObject* o, Py_ssize_t s)
{
    return reinterpret_cast<PROC_PyList_GetItem>(pythonSymbols["PyList_GetItem"])(o, s);
}
//=============================================================================
int
NLSPyCallable_Check(PyObject* o)
{
    return reinterpret_cast<PROC_PyCallable_Check>(pythonSymbols["PyCallable_Check"])(o);
}
//=============================================================================
PyObject*
NLSPyObject_CallNoArgs(PyObject* func)
{
    return reinterpret_cast<PROC_PyObject_CallNoArgs>(pythonSymbols["PyObject_CallNoArgs"])(func);
}
//=============================================================================
void
NLSPyErr_Print()
{
    return reinterpret_cast<PROC_PyErr_Print>(pythonSymbols["PyErr_Print"])();
}
//=============================================================================
PyObject*
NLSPyImport_ImportModule(const char* name)
{
    return reinterpret_cast<PROC_PyImport_ImportModule>(pythonSymbols["PyImport_ImportModule"])(
        name);
}
//=============================================================================
PyObject*
NLSPyObject_CallObject(PyObject* callable, PyObject* args)
{
    return reinterpret_cast<PROC_PyObject_CallObject>(pythonSymbols["PyObject_CallObject"])(
        callable, args);
}
//=============================================================================
PyObject*
NLSPyUnicode_FromString(const char* str)
{
    return reinterpret_cast<PROC_PyUnicode_FromString>(pythonSymbols["PyUnicode_FromString"])(str);
}
//=============================================================================
PyObject*
NLSPyDict_New()
{
    return reinterpret_cast<PROC_PyDict_New>(pythonSymbols["PyDict_New"])();
}
//=============================================================================
PyObject*
NLSPyFloat_FromDouble(double v)
{
    return reinterpret_cast<PROC_PyFloat_FromDouble>(pythonSymbols["PyFloat_FromDouble"])(v);
}
//=============================================================================
PyObject*
NLSPy_BuildValue(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    PyObject* result
        = reinterpret_cast<PROC_Py_VaBuildValue>(pythonSymbols["Py_VaBuildValue"])(format, args);
    va_end(args);
    return result;
}
//=============================================================================
PyObject*
NLSPyObject_CallMethodOneItem(PyObject* obj, const char* name, const char* format, PyObject* item)
{

    return reinterpret_cast<PROC_PyObject_CallMethod>(pythonSymbols["PyObject_CallMethod"])(
        obj, name, format, item);
}
//=============================================================================
int
NLSPyObject_IsInstance(PyObject* object, PyObject* typeorclass)
{
    return reinterpret_cast<PROC_PyObject_IsInstance>(pythonSymbols["PyObject_IsInstance"])(
        object, typeorclass);
}
//=============================================================================
char*
NLSPyBytes_AsString(PyObject* o)
{
    return reinterpret_cast<PROC_PyBytes_AsString>(pythonSymbols["PyBytes_AsString"])(o);
}
//=============================================================================
PyObject*
NLSPySequence_GetItem(PyObject* o, Py_ssize_t i)
{
    return reinterpret_cast<PROC_PySequence_GetItem>(pythonSymbols["PySequence_GetItem"])(o, i);
}
//=============================================================================
Py_ssize_t
NLSPySequence_Size(PyObject* o)
{
    return reinterpret_cast<PROC_PySequence_Size>(pythonSymbols["PySequence_Size"])(o);
}
//=============================================================================
PyObject*
NLSPyObject_Repr(PyObject* o)
{
    return reinterpret_cast<PROC_PyObject_Repr>(pythonSymbols["PyObject_Repr"])(o);
}
//=============================================================================
int
NLSPyBuffer_FillInfo(
    Py_buffer* view, PyObject* o, void* buf, Py_ssize_t len, int readonly, int flags)
{
    return reinterpret_cast<PROC_PyBuffer_FillInfo>(pythonSymbols["PyBuffer_FillInfo"])(
        view, o, buf, len, readonly, flags);
}
//=============================================================================
PyObject*
NLSPyMemoryView_FromBuffer(const Py_buffer* info)
{
    return reinterpret_cast<PROC_PyMemoryView_FromBuffer>(pythonSymbols["PyMemoryView_FromBuffer"])(
        info);
}
//=============================================================================
void
NLSPyBuffer_Release(Py_buffer* view)
{
    return reinterpret_cast<PROC_PyBuffer_Release>(pythonSymbols["PyBuffer_Release"])(view);
}
//=============================================================================
PyObject*
NLSPyTuple_New(Py_ssize_t size)
{
    return reinterpret_cast<PROC_PyTuple_New>(pythonSymbols["PyTuple_New"])(size);
}
//=============================================================================
PyObject*
NLSPyLong_FromSize_t(size_t sz)
{
    return reinterpret_cast<PROC_PyLong_FromSize_t>(pythonSymbols["PyLong_FromSize_t"])(sz);
}
//=============================================================================
int
NLSPyTuple_SetItem(PyObject* p, Py_ssize_t pos, PyObject* o)
{
    return reinterpret_cast<PROC_PyTuple_SetItem>(pythonSymbols["PyTuple_SetItem"])(p, pos, o);
}
//=============================================================================
int
NLSPyObject_GetBuffer(PyObject* obj, Py_buffer* view, int flags)
{
    return reinterpret_cast<PROC_PyObject_GetBuffer>(pythonSymbols["PyObject_GetBuffer"])(
        obj, view, flags);
}
//=============================================================================
PyObject*
NLSPyComplex_FromCComplex(Py_complex cplx)
{
    return reinterpret_cast<PROC_PyComplex_FromCComplex>(pythonSymbols["PyComplex_FromCComplex"])(
        cplx);
}
//=============================================================================
PyObject*
NLSPyLong_FromLongLong(long long v)
{
    return reinterpret_cast<PROC_PyLong_FromLongLong>(pythonSymbols["PyLong_FromLongLong"])(v);
}
//=============================================================================
PyObject*
NLSPyLong_FromUnsignedLongLong(unsigned long long v)
{
    return reinterpret_cast<PROC_PyLong_FromUnsignedLongLong>(
        pythonSymbols["PyLong_FromUnsignedLongLong"])(v);
}
//=============================================================================
PyObject*
NLSPyBool_FromLong(long v)
{
    return reinterpret_cast<PROC_PyBool_FromLong>(pythonSymbols["PyBool_FromLong"])(v);
}
//=============================================================================
PyObject*
NLSPyLong_FromUnsignedLong(unsigned long v)
{
    return reinterpret_cast<PROC_PyLong_FromUnsignedLong>(pythonSymbols["PyLong_FromUnsignedLong"])(
        v);
}
//=============================================================================
PyObject*
NLSPyLong_FromLong(long v)
{
    return reinterpret_cast<PROC_PyLong_FromLong>(pythonSymbols["PyLong_FromLong"])(v);
}
//=============================================================================
PyObject*
NLSPyObject_CallMethodOneArg(PyObject* self, PyObject* name, PyObject* arg)
{
    PyObject* args[2] = { self, arg };
    size_t nargsf = 2 | PY_VECTORCALL_ARGUMENTS_OFFSET;
    return NLSPyObject_VectorcallMethod(name, args, nargsf, NULL);
}
//=============================================================================
PyObject*
NLSPyObject_VectorcallMethod(
    PyObject* name, PyObject* const* args, size_t nargsf, PyObject* kwnames)
{
    return reinterpret_cast<PROC_PyObject_VectorcallMethod>(
        pythonSymbols["PyObject_VectorcallMethod"])(name, args, nargsf, kwnames);
}
//=============================================================================
long long
NLSPyLong_AsLongLong(PyObject* o)
{
    return reinterpret_cast<PROC_PyLong_AsLongLong>(pythonSymbols["PyLong_AsLongLong"])(o);
}
//=============================================================================
unsigned long long
NLSPyLong_AsUnsignedLongLong(PyObject* o)
{
    return reinterpret_cast<PROC_PyLong_AsUnsignedLongLong>(
        pythonSymbols["PyLong_AsUnsignedLongLong"])(o);
}
//=============================================================================
double
NLSPyComplex_RealAsDouble(PyObject* op)
{
    return reinterpret_cast<PROC_PyComplex_RealAsDouble>(pythonSymbols["PyComplex_RealAsDouble"])(
        op);
}
//=============================================================================
double
NLSPyComplex_ImagAsDouble(PyObject* op)
{
    return reinterpret_cast<PROC_PyComplex_ImagAsDouble>(pythonSymbols["PyComplex_ImagAsDouble"])(
        op);
}
//=============================================================================
Py_ssize_t
NLSPyTuple_Size(PyObject* o)
{
    return reinterpret_cast<PROC_PyTuple_Size>(pythonSymbols["PyTuple_Size"])(o);
}
//=============================================================================
PyObject*
NLSPyTuple_GetItem(PyObject* o, Py_ssize_t sz)
{
    return reinterpret_cast<PROC_PyTuple_GetItem>(pythonSymbols["PyTuple_GetItem"])(o, sz);
}
//=============================================================================
PyObject*
NLSPyEval_GetBuiltins(void)
{
    return reinterpret_cast<PROC_PyEval_GetBuiltins>(pythonSymbols["PyEval_GetBuiltins"])();
}
//=============================================================================
PyObject*
NLSPyDict_GetItemString(PyObject* dp, const char* key)
{
    return reinterpret_cast<PROC_PyDict_GetItemString>(pythonSymbols["PyDict_GetItemString"])(
        dp, key);
}
//=============================================================================
int
NLSPyDict_SetItemString(PyObject* dp, const char* key, PyObject* item)
{
    return reinterpret_cast<PROC_PyDict_SetItemString>(pythonSymbols["PyDict_SetItemString"])(
        dp, key, item);
}
//=============================================================================
PyObject*
NLSPyDict_Keys(PyObject* mp)
{
    return reinterpret_cast<PROC_PyDict_Keys>(pythonSymbols["PyDict_Keys"])(mp);
}
//=============================================================================
int
NLSPyBytes_AsStringAndSize(PyObject* obj, char** s, Py_ssize_t* len)
{
    return reinterpret_cast<PROC_PyBytes_AsStringAndSize>(pythonSymbols["PyBytes_AsStringAndSize"])(
        obj, s, len);
}
//=============================================================================
char*
NLSPyByteArray_AsString(PyObject* obj)
{
    return reinterpret_cast<PROC_PyByteArray_AsString>(pythonSymbols["PyByteArray_AsString"])(obj);
}
//=============================================================================
PyObject*
NLSPyEval_EvalCode(PyObject* co, PyObject* globals, PyObject* locals)
{
    return reinterpret_cast<PROC_PyEval_EvalCode>(pythonSymbols["PyEval_EvalCode"])(
        co, globals, locals);
}
//=============================================================================
PyObject*
NLSPyList_New(Py_ssize_t size)
{
    return reinterpret_cast<PROC_PyList_New>(pythonSymbols["PyList_New"])(size);
}
//=============================================================================
int
NLSPyList_SetItem(PyObject* ob1, Py_ssize_t sz, PyObject* ob2)
{
    return reinterpret_cast<PROC_PyList_SetItem>(pythonSymbols["PyList_SetItem"])(ob1, sz, ob2);
}
//=============================================================================
PyObject*
NLSPyUnicode_DecodeFSDefault(const char* s)
{
    return reinterpret_cast<PROC_PyUnicode_DecodeFSDefault>(
        pythonSymbols["PyUnicode_DecodeFSDefault"])(s);
}
//=============================================================================
int
NLSPyRun_SimpleFileExFlags(FILE* fp, const char* filename, int closeit, PyCompilerFlags* flags)
{
    return reinterpret_cast<PROC_PyRun_SimpleFileExFlags>(pythonSymbols["PyRun_SimpleFileExFlags"])(
        fp, filename, closeit, flags);
}
//=============================================================================
FILE*
NLS_Py_fopen_obj(PyObject* path, const char* mode)
{
    return reinterpret_cast<PROC_Py_fopen_obj>(pythonSymbols["_Py_fopen_obj"])(path, mode);
}
//=============================================================================
PyObject*
NLSPyDict_Copy(PyObject* mp)
{
    return reinterpret_cast<PROC_PyDict_Copy>(pythonSymbols["PyDict_Copy"])(mp);
}
//=============================================================================
void
NLSPyDict_Clear(PyObject* mp)
{
    reinterpret_cast<PROC_PyDict_Clear>(pythonSymbols["PyDict_Clear"])(mp);
}
//=============================================================================
int
NLSPyDict_Update(PyObject* mp, PyObject* other)
{
    return reinterpret_cast<PROC_PyDict_Update>(pythonSymbols["PyDict_Update"])(mp, other);
}
//=============================================================================
PyObject*
NLSPyNumber_Add(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_Add>(pythonSymbols["PyNumber_Add"])(o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_Subtract(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_Subtract>(pythonSymbols["PyNumber_Subtract"])(o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_Multiply(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_Multiply>(pythonSymbols["PyNumber_Multiply"])(o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_MatrixMultiply(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_MatrixMultiply>(pythonSymbols["PyNumber_MatrixMultiply"])(
        o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_TrueDivide(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_TrueDivide>(pythonSymbols["PyNumber_TrueDivide"])(o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_Remainder(PyObject* o1, PyObject* o2)
{
    return reinterpret_cast<PROC_PyNumber_Remainder>(pythonSymbols["PyNumber_Remainder"])(o1, o2);
}
//=============================================================================
PyObject*
NLSPyNumber_Power(PyObject* o1, PyObject* o2, PyObject* o3)
{
    return reinterpret_cast<PROC_PyNumber_Power>(pythonSymbols["PyNumber_Power"])(o1, o2, o3);
}
//=============================================================================
PyObject*
NLSPyNumber_Negative(PyObject* o)
{
    return reinterpret_cast<PROC_PyNumber_Negative>(pythonSymbols["PyNumber_Negative"])(o);
}
//=============================================================================
PyObject*
NLSPyNumber_Positive(PyObject* o)
{
    return reinterpret_cast<PROC_PyNumber_Positive>(pythonSymbols["PyNumber_Positive"])(o);
}
//=============================================================================
int
NLSPyObject_RichCompareBool(PyObject* o1, PyObject* o2, int opid)
{
    return reinterpret_cast<PROC_PyObject_RichCompareBool>(
        pythonSymbols["PyObject_RichCompareBool"])(o1, o2, opid);
}
//=============================================================================
int
NLSPyDict_SetItem(PyObject* mp, PyObject* key, PyObject* item)
{
    return reinterpret_cast<PROC_PyDict_SetItem>(pythonSymbols["PyDict_SetItem"])(mp, key, item);
}
//=============================================================================
PyObject*
NLSPyDict_GetItem(PyObject* mp, PyObject* key)
{
    return reinterpret_cast<PROC_PyDict_GetItem>(pythonSymbols["PyDict_GetItem"])(mp, key);
}
//=============================================================================
