//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
template <class T>
bool
castRealMethodNumpyType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodMemoryViewType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodListOrTupleType(
    PyObject* pyObject, PythonType pyType, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodLongType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodNumpyType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodFloatType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodComplexType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodLongType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodBytesType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodByteArrayType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodBoolType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
template <class T>
bool
castRealMethodArrayArrayType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results);
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
        return castRealMethodNumpyType<T>(pyObject, nelsonType, results);
    } break;
    case PY_FLOAT_TYPE: {
        return castRealMethodFloatType<T>(pyObject, nelsonType, results);
    } break;
    case PY_BOOL_TYPE: {
        return castRealMethodBoolType<T>(pyObject, nelsonType, results);
    } break;
    case PY_COMPLEX_TYPE: {
        return castRealMethodComplexType<T>(pyObject, nelsonType, results);
    } break;
    case PY_LONG_TYPE: {
        return castRealMethodLongType<T>(pyObject, nelsonType, results);
    } break;
    case PY_BYTES_TYPE: {
        return castRealMethodBytesType<T>(pyObject, nelsonType, results);
    } break;
    case PY_BYTE_ARRAY_TYPE: {
        return castRealMethodByteArrayType<T>(pyObject, nelsonType, results);
    } break;
    case PY_MEMORY_VIEW_TYPE: {
        return castRealMethodMemoryViewType<T>(pyObject, nelsonType, results);
    } break;
    case PY_LIST_TYPE:
    case PY_TUPLE_TYPE: {
        return castRealMethodListOrTupleType<T>(pyObject, pyType, nelsonType, results);
    } break;
    case PY_ARRAY_ARRAY_TYPE: {
        return castRealMethodArrayArrayType<T>(pyObject, nelsonType, results);
    } break;
    case PY_NONE_TYPE:
    case PY_STR_TYPE:
    case PY_DICT_TYPE:
    case PY_NOT_MANAGED:
    default: {
    } break;
    }
    return false;
}
//=============================================================================
template <class T>
bool
castRealMethodNumpyType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    PyObject* data = NLSPyObject_GetAttrString(pyObject, "data");
    if (data) {
        bool res = castRealMethod<T>(data, nelsonType, results);
        NLSPy_DECREF(data);
        return res;
    }
    return false;
}
//=============================================================================
template <class T>
bool
castRealMethodFloatType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    double result = NLSPyFloat_AsDouble(pyObject);
    ArrayOf res = ArrayOf::doubleConstructor(result);
    res.promoteType(nelsonType);
    results << res;
    return true;
}
//=============================================================================
template <class T>
bool
castRealMethodBoolType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    logical value = (logical)NLSPyObject_IsTrue(pyObject);
    ArrayOf res = ArrayOf::logicalConstructor(value);
    res.promoteType(nelsonType);
    results << res;
    return true;
}
//=============================================================================
template <class T>
bool
castRealMethodComplexType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
    ArrayOf res = ArrayOf::dcomplexConstructor(pyCplx.real, pyCplx.imag);
    if (nelsonType == NLS_DOUBLE) {
        res.promoteType(NLS_DCOMPLEX);
    } else {
        res.promoteType(NLS_SCOMPLEX);
    }
    results << res;
    return true;
}
//=============================================================================
template <class T>
bool
castRealMethodLongType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    double value = NLSPyLong_AsDouble(pyObject);
    ArrayOf res = ArrayOf::doubleConstructor(value);
    res.promoteType(nelsonType);
    results << res;
    return true;
}
//=============================================================================
template <class T>
bool
castRealMethodBytesType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
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
    return false;
}
//=============================================================================
template <class T>
bool
castRealMethodByteArrayType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
    char* buffer = NLSPyByteArray_AsString(pyObject);
    size_t len = strlen(buffer);
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, len);
    ArrayOf res(nelsonType, Dimensions(1, len), ptr);
    for (size_t k = 0; k < len; ++k) {
        ptr[k] = (T)buffer[k];
    }
    results << res;
    return true;
}
//=============================================================================
template <class T>
bool
castRealMethodMemoryViewType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
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
}
//=============================================================================
template <class T>
bool
castRealMethodListOrTupleType(
    PyObject* pyObject, PythonType pyType, NelsonType nelsonType, ArrayOfVector& results)
{
    Py_ssize_t sz = (pyType == PY_LIST_TYPE) ? NLSPyList_Size(pyObject) : NLSPyTuple_Size(pyObject);
    T* elements
        = (T*)ArrayOf::allocateArrayOf(nelsonType == NLS_DOUBLE ? NLS_DCOMPLEX : NLS_SCOMPLEX, sz);
    Dimensions dims(1, sz);
    ArrayOf res(nelsonType == NLS_DOUBLE ? NLS_DCOMPLEX : NLS_SCOMPLEX, dims, elements);
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
}
//=============================================================================
template <class T>
bool
castRealMethodArrayArrayType(PyObject* pyObject, NelsonType nelsonType, ArrayOfVector& results)
{
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
    case PY_NONE_TYPE:
    case PY_STR_TYPE:
    case PY_DICT_TYPE:
    case PY_NOT_MANAGED:
    default: {
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
    case PY_NONE_TYPE:
    case PY_FLOAT_TYPE:
    case PY_BOOL_TYPE:
    case PY_COMPLEX_TYPE:
    case PY_LONG_TYPE:
    case PY_BYTES_TYPE:
    case PY_BYTE_ARRAY_TYPE:
    case PY_MEMORY_VIEW_TYPE:
    case PY_DICT_TYPE:
    case PY_ARRAY_ARRAY_TYPE:
    case PY_NUMPY_TYPE:
    case PY_NOT_MANAGED:
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
        ArrayOfVector values;
        stringVector names;
        for (Py_ssize_t i = 0; i < size; i++) {
            PyObject* key = NLSPyList_GetItem(keys, i);
            PyObject* value = NLSPyDict_GetItem(pyObject, key);
            PyObject* str = NLSPyObject_Str(key);
            names.push_back(NLSPyUnicode_AsUTF8(str));
            NLSPy_DECREF(str);
            bool needToDecreaseReference;
            values.push_back(PyObjectToArrayOf(value, needToDecreaseReference));
        }
        NLSPy_DECREF(keys);
        results << ArrayOf::structScalarConstructor(names, values);
        return true;
    } break;
    case PY_NONE_TYPE:
    case PY_FLOAT_TYPE:
    case PY_BOOL_TYPE:
    case PY_COMPLEX_TYPE:
    case PY_LONG_TYPE:
    case PY_BYTES_TYPE:
    case PY_BYTE_ARRAY_TYPE:
    case PY_MEMORY_VIEW_TYPE:
    case PY_LIST_TYPE:
    case PY_TUPLE_TYPE:
    case PY_ARRAY_ARRAY_TYPE:
    case PY_NUMPY_TYPE:
    case PY_NOT_MANAGED:
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
    case PY_NONE_TYPE:
    case PY_FLOAT_TYPE:
    case PY_BOOL_TYPE:
    case PY_COMPLEX_TYPE:
    case PY_LONG_TYPE:
    case PY_BYTES_TYPE:
    case PY_BYTE_ARRAY_TYPE:
    case PY_MEMORY_VIEW_TYPE:
    case PY_LIST_TYPE:
    case PY_TUPLE_TYPE:
    case PY_DICT_TYPE:
    case PY_ARRAY_ARRAY_TYPE:
    case PY_NUMPY_TYPE:
    case PY_NOT_MANAGED:
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
}
//=============================================================================
