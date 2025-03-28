//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonTypesWrapper.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PythonObjectHandle.hpp"
#include "PyObjectHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static PyObject*
convertDoubleArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertSingleArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertDoubleComplexArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertSingleComplexArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertLogicalArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertIntegerArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertUnsignedIntegerArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertCharactersArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertStringArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertStructArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertCellArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertHandleArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertClassArray(const ArrayOf& A);
//=============================================================================
static ArrayOf
PyArrayArrayToDoubleArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize);
//=============================================================================
static ArrayOf
PyArrayArrayToSingleArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize);
//=============================================================================
static ArrayOf
PyArrayArrayToDoubleComplexArrayOf(
    NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize);
//=============================================================================
static ArrayOf
PyArrayArrayToSingleComplexArrayOf(
    NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize);
//=============================================================================
static PyObject*
rowVectorToPyArray(const void* ptrValues, size_t len, const char* dataType);
//=============================================================================
PyObject*
arrayOfToPyObject(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;

    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        return convertDoubleArray(A);
    } break;
    case NLS_SINGLE: {
        return convertSingleArray(A);
    } break;
    case NLS_DCOMPLEX: {
        return convertDoubleComplexArray(A);
    } break;
    case NLS_SCOMPLEX: {
        return convertSingleComplexArray(A);
    } break;
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64: {
        return convertIntegerArray(A);
    } break;
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        return convertUnsignedIntegerArray(A);
    } break;
    case NLS_LOGICAL: {
        return convertLogicalArray(A);
    } break;
    case NLS_CHAR: {
        return convertCharactersArray(A);
    } break;
    case NLS_STRUCT_ARRAY: {
        return convertStructArray(A);
    } break;
    case NLS_CELL_ARRAY: {
        return convertCellArray(A);
    } break;
    case NLS_STRING_ARRAY: {
        return convertStringArray(A);
    } break;
    case NLS_HANDLE: {
        return convertHandleArray(A);
    } break;
    case NLS_CLASS_ARRAY: {
        return convertClassArray(A);
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
        Error(_W("Conversion to Python is not supported."));
    } break;
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertDoubleArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isSparse()) {
        Error(_W("Conversion to Python is not supported."));
    }
    if (A.isScalar()) {
        pyObj = NLSPyFloat_FromDouble(A.getContentAsDoubleScalar());
    } else if (!A.isComplex() && (A.isEmpty() || A.isVector())) {
        pyObj = rowVectorToPyArray(
            A.getDataPointer(), A.getElementCount(), nelsonTypeToTypeCode(A.getDataClass()));
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertSingleArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        pyObj = NLSPyFloat_FromDouble((double)A.getContentAsSingleScalar());
    } else if (!A.isComplex() && (A.isEmpty() || A.isVector())) {
        pyObj = rowVectorToPyArray(
            A.getDataPointer(), A.getElementCount(), nelsonTypeToTypeCode(A.getDataClass()));
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertDoubleComplexArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isSparse()) {
        Error(_W("Conversion to Python is not supported."));
    }
    if (A.isScalar()) {
        std::complex<double> cplx = A.getContentAsDoubleComplexScalar();
        Py_complex pyCplx;
        pyCplx.imag = cplx.imag();
        pyCplx.real = cplx.real();
        pyObj = NLSPyComplex_FromCComplex(pyCplx);
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertSingleComplexArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        std::complex<single> cplx = A.getContentAsSingleComplexScalar();
        Py_complex pyCplx;
        pyCplx.imag = cplx.imag();
        pyCplx.real = cplx.real();
        pyObj = NLSPyComplex_FromCComplex(pyCplx);
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertLogicalArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isSparse()) {
        Error(_W("Conversion to Python is not supported."));
    }
    if (A.isScalar()) {
        pyObj = NLSPyBool_FromLong(A.getContentAsLogicalScalar());
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertIntegerArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        int64 v = A.getContentAsInteger64Scalar();
        pyObj = NLSPyLong_FromLongLong(v);
    } else if (A.isVector() || A.isEmpty()) {
        pyObj = rowVectorToPyArray(
            A.getDataPointer(), A.getElementCount(), nelsonTypeToTypeCode(A.getDataClass()));
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertUnsignedIntegerArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        uint64 v = A.getContentAsUnsignedInteger64Scalar();
        pyObj = NLSPyLong_FromUnsignedLongLong(v);
    } else if (A.isVector() || A.isEmpty()) {
        pyObj = rowVectorToPyArray(
            A.getDataPointer(), A.getElementCount(), nelsonTypeToTypeCode(A.getDataClass()));
    } else {
        pyObj = arrayToMemoryView(A.getDataPointer(), A.getDataClass(), A.getDimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertCharactersArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isRowVector() || A.isEmpty()) {
        std::wstring wstr = A.getContentAsWideString();
        std::string str = wstring_to_utf8(wstr);
        pyObj = NLSPyUnicode_FromString(str.c_str());
    } else {
        Error(_W("Conversion of 'char' to Python is only supported for 1-N vectors."));
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertStringArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (!A.isScalar()) {
        Error(_W("Conversion to Python is not supported."));
    }
    auto* elements = (ArrayOf*)A.getDataPointer();
    if (elements[0].isCharacterArray()) {
        std::wstring wstr = A.getContentAsWideString();
        std::string str = wstring_to_utf8(wstr);
        pyObj = NLSPyUnicode_FromString(str.c_str());
    } else {
        pyObj = Py_None;
        NLSPy_INCREF(pyObj);
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertStructArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (!A.isScalar()) {
        Error(_W("Conversion to Python is not supported."));
    }
    pyObj = NLSPyDict_New();
    stringVector names = A.getFieldNames();
    for (auto name : names) {
        ArrayOf value = A.getField(name);
        NLSPyDict_SetItemString(pyObj, name.c_str(), arrayOfToPyObject(value));
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertCellArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (!A.isRowVector()) {
        Error(_W("Conversion to Python is not supported."));
    }
    ArrayOf* elements = (ArrayOf*)A.getDataPointer();
    pyObj = NLSPyTuple_New(A.getElementCount());
    for (indexType k = 0; k < A.getElementCount(); ++k) {
        NLSPyTuple_SetItem(pyObj, k, arrayOfToPyObject(elements[k]));
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertHandleArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.getHandleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        HandleGenericObject* hgo = A.getContentAsHandleScalar();
        PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
        pyObj = (PyObject*)poh->getPointer();
        NLSPy_INCREF(pyObj);
        return pyObj;
    }
    Error(_W("Conversion to Python is not supported."));
    return pyObj;
}
//=============================================================================
PyObject*
convertClassArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.getClassType() == "dictionary") {
        pyObj = NLSPyDict_New();
        stringVector fieldnames = A.getFieldNames();
        auto it = std::find(fieldnames.begin(), fieldnames.end(), "map");
        if (it == fieldnames.end()) {
            Error(_W("Invalid dictionary."));
        }
        it = std::find(fieldnames.begin(), fieldnames.end(), "allKeys");
        if (it == fieldnames.end()) {
            Error(_W("Invalid dictionary."));
        }
        ArrayOf map = A.getField("map");
        stringVector mapHash = map.getFieldNames();
        ArrayOf allKeys = A.getField("allKeys");

        if (allKeys.isCell() || allKeys.isStringArray()) {
            ArrayOf* element = (ArrayOf*)allKeys.getDataPointer();
            indexType nbElements = allKeys.getElementCount();

            for (indexType k = 0; k < nbElements; ++k) {
                NLSPyDict_SetItem(pyObj, arrayOfToPyObject(element[k]),
                    arrayOfToPyObject(map.getField(mapHash[k])));
            }
        } else {
            Error(_("Type not managed."));
        }
        return pyObj;
    }
    if (A.getClassType() == "pyargs") {
        pyObj = NLSPyDict_New();
        stringVector names = A.getFieldNames();
        for (auto name : names) {
            ArrayOf value = A.getField(name);
            NLSPyDict_SetItemString(pyObj, name.c_str(), arrayOfToPyObject(value));
        }
        return pyObj;
    }
    Error(_W("Conversion to Python is not supported."));
    return pyObj;
}
//=============================================================================
PyObject*
rowVectorToPyArray(const void* ptrValues, size_t len, const char* dataType)
{
    if (strcmp(dataType, "?") == 0) {
        Error(_W("Type not supported"));
    }

    PyObject* arrayModule = NLSPyImport_ImportModule("array");
    PyObject* arrayConstructor = NLSPyObject_GetAttrString(arrayModule, "array");

    PyObject* args = NLSPy_BuildValue("(s)", dataType);
    PyObject* pyArray = NLSPyObject_CallObject(arrayConstructor, args);
    for (size_t k = 0; k < len; k++) {
        PyObject* item = nullptr;
        if (!item && strcmp(dataType, "Zf") == 0) {
            // not supported
            Py_complex pyCplx;
            pyCplx.real = ((const single*)ptrValues)[k];
            pyCplx.imag = ((const single*)ptrValues)[k + 1];
            item = NLSPyComplex_FromCComplex(pyCplx);
        }
        if (!item && strcmp(dataType, "Zd") == 0) {
            // not supported
            Py_complex pyCplx
                = { ((const double*)ptrValues)[k], ((const double*)ptrValues)[k + 1] };
            item = NLSPyComplex_FromCComplex(pyCplx);
        }
        if (!item && strcmp(dataType, "d") == 0) {
            item = NLSPyFloat_FromDouble(((const double*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "f") == 0) {
            item = NLSPyFloat_FromDouble((double)((const single*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "B") == 0) {
            item = NLSPyLong_FromUnsignedLong((unsigned long)((const uint8*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "H") == 0) {
            item = NLSPyLong_FromUnsignedLong((unsigned long)((const uint16*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "I") == 0) {
            item = NLSPyLong_FromUnsignedLong((unsigned long)((const uint32*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "L") == 0) {
            item = NLSPyLong_FromUnsignedLong((unsigned long)((const uint32*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "Q") == 0) {
            item
                = NLSPyLong_FromUnsignedLongLong((unsigned long long)((const uint64*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "b") == 0) {
            int8* ptr = (int8*)(ptrValues);
            int8 i8value = ptr[k];
            item = NLSPyLong_FromLong((long)(i8value));
        }
        if (!item && strcmp(dataType, "h") == 0) {
            item = NLSPyLong_FromLong((long)((const int16*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "i") == 0) {
            item = NLSPyLong_FromLong((long)((const int32*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "l") == 0) {
            item = NLSPyLong_FromLong((long)((const int32*)ptrValues)[k]);
        }
        if (!item && strcmp(dataType, "q") == 0) {
            item = NLSPyLong_FromLongLong((long long)((const int64*)ptrValues)[k]);
        }

        if (item == nullptr) {
            // Handle unsupported data type
            NLSPy_DECREF(pyArray);
            NLSPy_DECREF(args);
            NLSPy_DECREF(arrayConstructor);
            NLSPy_DECREF(arrayModule);
            Error(_W("Type not supported"));
            return NULL;
        }
        NLSPyObject_CallMethodOneItem(pyArray, "append", "(O)", item);
        NLSPy_DECREF(item);
    }

    NLSPy_DECREF(args);
    NLSPy_DECREF(arrayConstructor);
    NLSPy_DECREF(arrayModule);

    return pyArray;
}
//=============================================================================
static Py_ssize_t
getTypeSize(NelsonType nelsonType)
{
    Py_ssize_t sz;
    switch (nelsonType) {
    case NLS_DOUBLE: {
        sz = sizeof(double);
    } break;
    case NLS_SINGLE: {
        sz = sizeof(single);
    } break;
    case NLS_DCOMPLEX: {
        sz = sizeof(double) * 2;
    } break;
    case NLS_SCOMPLEX: {
        sz = sizeof(single) * 2;
    } break;
    case NLS_INT8: {
        sz = sizeof(int8);
    } break;
    case NLS_INT16: {
        sz = sizeof(int16);
    } break;
    case NLS_INT32: {
        sz = sizeof(int32);
    } break;
    case NLS_INT64: {
        sz = sizeof(int64);
    } break;
    case NLS_UINT8: {
        sz = sizeof(uint8);
    } break;
    case NLS_UINT16: {
        sz = sizeof(uint16);
    } break;
    case NLS_UINT32: {
        sz = sizeof(uint32);
    } break;
    case NLS_UINT64: {
        sz = sizeof(uint64);
    } break;
    case NLS_LOGICAL: {
        sz = sizeof(logical);
    } break;
    default: {
        Error(_W("Type not managed."));
    }
    }
    return sz;
}
//=============================================================================
PyObject*
arrayToMemoryView(const void* data, NelsonType nelsonType, const Dimensions& dims)
{
    Py_buffer pyBuffer;
    pyBuffer.format = nelsonTypeToTypeCode(nelsonType);
    if (strcmp(pyBuffer.format, "") == 0) {
        Error(_W("Type not managed."));
    }
    pyBuffer.ndim = (int)dims.getLength();

    Py_ssize_t* shape = (Py_ssize_t*)malloc(sizeof(Py_ssize_t) * dims.getLength());
    for (size_t k = 0; k < dims.getLength(); ++k) {
        shape[k] = dims.getAt(k);
    }
    pyBuffer.shape = shape;

    void* ptr = (void*)ArrayOf::allocateArrayOf(nelsonType, dims.getElementCount());
    if (!ptr) {
        Error(_W("Cannot create MemoryView."));
        return nullptr;
    }

    Py_ssize_t sz = getTypeSize(nelsonType);
    memcpy(ptr, data, sz * dims.getElementCount());
    pyBuffer.buf = ptr;

    Py_ssize_t* strides = (Py_ssize_t*)malloc(sizeof(Py_ssize_t) * dims.getLength());
    for (size_t k = 0; k < dims.getLength(); ++k) {
        strides[k] = sz;
    }
    if (dims.getLength() > 0) {
        strides[dims.getLength() - 1] = dims.getRows() * sz;
    }
    pyBuffer.strides = strides;
    pyBuffer.suboffsets = NULL;
    pyBuffer.itemsize = sz;
    pyBuffer.internal = NULL;
    PyObject* memoryView = NLSPyMemoryView_FromBuffer(&pyBuffer);
    if (!memoryView) {
        ArrayOf r = ArrayOf(nelsonType, dims, ptr);
        Error(_W("Cannot create MemoryView."));
    }
    return memoryView;
}
//=============================================================================
ArrayOf
PyMemoryViewToArrayOf(PyObject* pyObject)
{
    ArrayOf res = {};

    Py_buffer pyBuffer;
    if (NLSPyObject_GetBuffer(pyObject, &pyBuffer, PyBUF_FORMAT | PyBUF_STRIDES) == -1) {
        Error(_W("Failed to get buffer from memory view."));
    }
    size_t nbElements = pyBuffer.len / pyBuffer.itemsize;

    size_t ndim = pyBuffer.ndim;
    std::vector<indexType> dimsVector;
    dimsVector.reserve(ndim);
    for (size_t k = 0; k < ndim; ++k) {
        dimsVector.push_back(pyBuffer.shape[k]);
    }
    if (nbElements > 0 && dimsVector.empty()) {
        dimsVector.push_back(1);
        dimsVector.push_back(nbElements);
    }
    if (dimsVector.size() == 1) {
        indexType d = dimsVector[0];
        dimsVector[0] = 1;
        dimsVector.push_back(d);
    }
    Dimensions dims(dimsVector);
    NelsonType nelsonType = PyTypecodeToNelsonType(pyBuffer.format);
    switch (nelsonType) {
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL: {
        void* data = (void*)pyBuffer.buf;
        void* ptr = (void*)ArrayOf::allocateArrayOf(nelsonType, dims.getElementCount());
        res = ArrayOf(nelsonType, dims, ptr);
        memcpy(ptr, data, res.getByteSize());
        return res;
    } break;
    case NLS_CHAR:
    case NLS_STRUCT_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
    } break;
    }

    Error(_W("Unsupported data format."));

    return res;
}
//=============================================================================
ArrayOf
PyObjectToArrayOf(PyObject* pyObject, bool& needToDecreaseReference)
{
    needToDecreaseReference = true;
    if (!pyObject) {
        needToDecreaseReference = false;
        PyObject* pyObj = Py_None;
        NLSPy_INCREF(pyObj);
        PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(pyObj);
        return ArrayOf::handleConstructor(pythonObjectHandle);
    }

    ArrayOf res = {};
    PyTypeObject* pyTypeObject = Py_TYPE(pyObject);

    if (pyTypeObject == PyFloat_TypePtr) {
        double result = NLSPyFloat_AsDouble(pyObject);
        return ArrayOf::doubleConstructor(result);
    }
    if (pyTypeObject == PyBool_TypePtr) {
        logical value = (logical)NLSPyObject_IsTrue(pyObject);
        return ArrayOf::logicalConstructor(value);
    }
    if (pyTypeObject == PyComplex_TypePtr) {
        Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
        return ArrayOf::dcomplexConstructor(pyCplx.real, pyCplx.imag);
    }
    needToDecreaseReference = false;
    NLSPy_INCREF(pyObject);
    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(pyObject);
    return ArrayOf::handleConstructor(pythonObjectHandle);
}
//=============================================================================
template <typename T>
ArrayOf
PyArrayArrayToIntegerArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    T* ptrRes = static_cast<T*>(ptr);
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[i] = static_cast<T>(NLSPyLong_AsLongLong(item));
        NLSPy_DECREF(item);
    }
    return res;
}
//=============================================================================
template <typename T>
ArrayOf
PyArrayArrayToUnsignedIntegerArrayOf(
    NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    T* ptrRes = static_cast<T*>(ptr);
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[i] = static_cast<T>(NLSPyLong_AsUnsignedLongLong(item));
        NLSPy_DECREF(item);
    }
    return res;
}
//=============================================================================
ArrayOf
PyArrayArrayToArrayOf(PyObject* pyObject)
{
    NelsonType nelsonType = PyTypecodeToNelsonType(getArrayArrayTypeCode(pyObject));
    Py_ssize_t vectorSize = NLSPySequence_Size(pyObject);

    switch (nelsonType) {
    case NLS_DOUBLE: {
        return PyArrayArrayToDoubleArrayOf(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_SINGLE: {
        return PyArrayArrayToSingleArrayOf(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_DCOMPLEX: {
        return PyArrayArrayToDoubleComplexArrayOf(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_SCOMPLEX: {
        return PyArrayArrayToSingleComplexArrayOf(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_UINT8: {
        return PyArrayArrayToUnsignedIntegerArrayOf<uint8>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_UINT16: {
        return PyArrayArrayToUnsignedIntegerArrayOf<uint16>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_UINT32: {
        return PyArrayArrayToUnsignedIntegerArrayOf<uint32>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_UINT64: {
        return PyArrayArrayToUnsignedIntegerArrayOf<uint64>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_INT8: {
        return PyArrayArrayToIntegerArrayOf<int8>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_INT16: {
        return PyArrayArrayToIntegerArrayOf<int16>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_INT32: {
        return PyArrayArrayToIntegerArrayOf<int32>(nelsonType, pyObject, vectorSize);
    } break;
    case NLS_INT64: {
        return PyArrayArrayToIntegerArrayOf<int64>(nelsonType, pyObject, vectorSize);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
PyArrayArrayToDoubleArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    double* ptrRes = (double*)ptr;
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[i] = NLSPyFloat_AsDouble(item);
        NLSPy_DECREF(item);
    }
    return res;
}
//=============================================================================
ArrayOf
PyArrayArrayToSingleArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    single* ptrRes = (single*)ptr;
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[i] = (single)NLSPyFloat_AsDouble(item);
        NLSPy_DECREF(item);
    }
    return res;
}
//=============================================================================
ArrayOf
PyArrayArrayToDoubleComplexArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    double* ptrRes = (double*)ptr;
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    Py_ssize_t k = 0;
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[k] = NLSPyComplex_RealAsDouble(item);
        ptrRes[k + 1] = NLSPyComplex_ImagAsDouble(item);
        NLSPy_DECREF(item);
        k = k + 2;
    }
    return res;
}
//=============================================================================
ArrayOf
PyArrayArrayToSingleComplexArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.getElementCount());
    single* ptrRes = (single*)ptr;
    ArrayOf res = ArrayOf(nelsonType, dimsVector, ptr);
    Py_ssize_t k = 0;
    for (Py_ssize_t i = 0; i < vectorSize; ++i) {
        PyObject* item = NLSPySequence_GetItem(pyObject, i);
        ptrRes[k] = (single)NLSPyComplex_RealAsDouble(item);
        ptrRes[k + 1] = (single)NLSPyComplex_ImagAsDouble(item);
        NLSPy_DECREF(item);
        k = k + 2;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
