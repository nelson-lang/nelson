//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PythonTypesWrapper.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "PythonObjectHandle.hpp"
#include "PyObjectHelpers.hpp"
#include <cstring>
#include <cmath>
#include <vector>
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
static PyObject*
convertTableArray(const ArrayOf& A);
//=============================================================================
static PyObject*
convertTableColumn(const ArrayOf& A);
//=============================================================================
static PyObject*
convertTableArrayToDict(const ArrayOf& A);
//=============================================================================
static PyObject*
convertTableArrayToDataFrame(const ArrayOf& A, PyObject* pandasModule);
//=============================================================================
static PyObject*
tableColumnToPyList(const ArrayOf& A);
//=============================================================================
static PyObject*
importPandasModule();
//=============================================================================
static PyObject*
stringVectorToPyList(const stringVector& values);
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
static bool
isFortranContiguousBuffer(const Py_buffer& pyBuffer);
//=============================================================================
static void
copyBufferToColumnMajor(const Py_buffer& pyBuffer, void* destination);
//=============================================================================
PyObject*
arrayOfToPyObject(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;

    switch (A.dataClass()) {
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
        if (A.isTable()) {
            return convertTableArray(A);
        }
        return convertClassArray(A);
    } break;
    case NLS_CLASSDEF_ARRAY: {
        if (A.isTable()) {
            return convertTableArray(A);
        }
        return convertClassArray(A);
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
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
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    if (A.isScalar()) {
        pyObj = NLSPyFloat_FromDouble(A.asDoubleScalar());
    } else if (!A.isComplex() && (A.isEmpty() || A.isVector())) {
        pyObj = rowVectorToPyArray(
            A.dataPointer(), A.elementCount(), nelsonTypeToTypeCode(A.dataClass()));
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertSingleArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        pyObj = NLSPyFloat_FromDouble((double)A.asSingleScalar());
    } else if (!A.isComplex() && (A.isEmpty() || A.isVector())) {
        pyObj = rowVectorToPyArray(
            A.dataPointer(), A.elementCount(), nelsonTypeToTypeCode(A.dataClass()));
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertDoubleComplexArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isSparse()) {
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    if (A.isScalar()) {
        std::complex<double> cplx = A.asDoubleComplexScalar();
        Py_complex pyCplx;
        pyCplx.imag = cplx.imag();
        pyCplx.real = cplx.real();
        pyObj = NLSPyComplex_FromCComplex(pyCplx);
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertSingleComplexArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        std::complex<single> cplx = A.asSingleComplexScalar();
        Py_complex pyCplx;
        pyCplx.imag = cplx.imag();
        pyCplx.real = cplx.real();
        pyObj = NLSPyComplex_FromCComplex(pyCplx);
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertLogicalArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isSparse()) {
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    if (A.isScalar()) {
        pyObj = NLSPyBool_FromLong(A.asLogicalScalar());
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertIntegerArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        int64 v = A.asInt64Scalar();
        pyObj = NLSPyLong_FromLongLong(v);
    } else if (A.isVector() || A.isEmpty()) {
        pyObj = rowVectorToPyArray(
            A.dataPointer(), A.elementCount(), nelsonTypeToTypeCode(A.dataClass()));
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertUnsignedIntegerArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isScalar()) {
        uint64 v = A.asUint64Scalar();
        pyObj = NLSPyLong_FromUnsignedLongLong(v);
    } else if (A.isVector() || A.isEmpty()) {
        pyObj = rowVectorToPyArray(
            A.dataPointer(), A.elementCount(), nelsonTypeToTypeCode(A.dataClass()));
    } else {
        pyObj = arrayToMemoryView(A.dataPointer(), A.dataClass(), A.dimensions());
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertCharactersArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.isRowVector() || A.isEmpty()) {
        std::wstring wstr = A.asWideString();
        std::string str = wstring_to_utf8(wstr);
        pyObj = NLSPyUnicode_FromString(str.c_str());
    } else {
        Error(L"Nelson:Python:charConversionUnsupported",
            L"Conversion of 'char' to Python is only supported for 1-N vectors.");
    }

    return pyObj;
}
//=============================================================================
PyObject*
convertStringArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (!A.isScalar()) {
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    auto* elements = (ArrayOf*)A.dataPointer();
    if (elements[0].isCharacterArray()) {
        std::wstring wstr = A.asWideString();
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
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    pyObj = NLSPyDict_New();
    stringVector names = A.fieldNames();
    for (auto name : names) {
        ArrayOf value = A.field(name);
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
        Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    }
    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    pyObj = NLSPyTuple_New(A.elementCount());
    for (indexType k = 0; k < A.elementCount(); ++k) {
        NLSPyTuple_SetItem(pyObj, k, arrayOfToPyObject(elements[k]));
    }
    return pyObj;
}
//=============================================================================
PyObject*
convertHandleArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.handleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        HandleGenericObject* hgo = A.asHandleScalar();
        PythonObjectHandle* poh = (PythonObjectHandle*)hgo;
        pyObj = (PyObject*)poh->pointer();
        NLSPy_INCREF(pyObj);
        return pyObj;
    }
    Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    return pyObj;
}
//=============================================================================
PyObject*
convertClassArray(const ArrayOf& A)
{
    PyObject* pyObj = nullptr;
    if (A.classType() == "dictionary") {
        pyObj = NLSPyDict_New();
        stringVector fieldnames = A.fieldNames();
        auto it = std::find(fieldnames.begin(), fieldnames.end(), "allKeys");
        if (it == fieldnames.end()) {
            Error(L"Nelson:Python:invalidDictionary", L"Invalid dictionary.");
        }
        it = std::find(fieldnames.begin(), fieldnames.end(), "allValues");
        if (it == fieldnames.end()) {
            Error(L"Nelson:Python:invalidDictionary", L"Invalid dictionary.");
        }
        // Values are stored in the allValues cell, parallel to allKeys.
        ArrayOf allKeys = A.field("allKeys");
        ArrayOf allValues = A.field("allValues");

        if (allKeys.isCell() || allKeys.isStringArray()) {
            ArrayOf* keyElement = (ArrayOf*)allKeys.dataPointer();
            ArrayOf* valueElement = (ArrayOf*)allValues.dataPointer();
            indexType nbElements = allKeys.elementCount();

            for (indexType k = 0; k < nbElements; ++k) {
                NLSPyDict_SetItem(
                    pyObj, arrayOfToPyObject(keyElement[k]), arrayOfToPyObject(valueElement[k]));
            }
        } else {
            Error("Nelson:Python:typeNotManaged", "Type not supported.");
        }
        return pyObj;
    }
    if (A.classType() == "pyargs") {
        pyObj = NLSPyDict_New();
        ArrayOf arguments = A.field("Arguments");
        stringVector names = arguments.fieldNames();
        for (const auto& name : names) {
            ArrayOf value = arguments.field(name);
            NLSPyDict_SetItemString(pyObj, name.c_str(), arrayOfToPyObject(value));
        }
        return pyObj;
    }
    Error(L"Nelson:Python:conversionNotSupported", L"Conversion to Python is not supported.");
    return pyObj;
}
//=============================================================================
PyObject*
convertTableArray(const ArrayOf& A)
{
    // Fall back to the dictionary representation when pandas cannot be imported.
    PyObject* pandasModule = importPandasModule();
    if (pandasModule) {
        PyObject* dataFrame = convertTableArrayToDataFrame(A, pandasModule);
        NLSPy_DECREF(pandasModule);
        if (dataFrame) {
            return dataFrame;
        }
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
        }
    }
    return convertTableArrayToDict(A);
}
//=============================================================================
PyObject*
convertTableArrayToDict(const ArrayOf& A)
{
    PyObject* pyObj = NLSPyDict_New();
    PyObject* dataDict = NLSPyDict_New();

    stringVector variableNames = A.tableVariableNames();
    for (const auto& variableName : variableNames) {
        NLSPyDict_SetItemString(
            dataDict, variableName.c_str(), convertTableColumn(A.tableColumn(variableName)));
    }
    NLSPyDict_SetItemString(pyObj, "data", dataDict);

    PyObject* propertiesDict = NLSPyDict_New();
    NLSPyDict_SetItemString(propertiesDict, "VariableNames", stringVectorToPyList(variableNames));
    NLSPyDict_SetItemString(
        propertiesDict, "VariableTypes", stringVectorToPyList(A.tableVariableTypes()));
    NLSPyDict_SetItemString(propertiesDict, "RowNames", stringVectorToPyList(A.tableRowNames()));
    NLSPyDict_SetItemString(pyObj, "Properties", propertiesDict);
    return pyObj;
}
//=============================================================================
PyObject*
convertTableArrayToDataFrame(const ArrayOf& A, PyObject* pandasModule)
{
    stringVector variableNames = A.tableVariableNames();
    // pandas keeps the dict order for the DataFrame columns.
    PyObject* dataDict = NLSPyDict_New();
    if (!dataDict) {
        return nullptr;
    }
    for (const auto& variableName : variableNames) {
        PyObject* column = tableColumnToPyList(A.tableColumn(variableName));
        if (!column) {
            NLSPy_DECREF(dataDict);
            return nullptr;
        }
        NLSPyDict_SetItemString(dataDict, variableName.c_str(), column);
        NLSPy_DECREF(column);
    }

    PyObject* dataFrameClass = NLSPyObject_GetAttrString(pandasModule, "DataFrame");
    if (!dataFrameClass) {
        NLSPy_DECREF(dataDict);
        return nullptr;
    }

    PyObject* kwargs = nullptr;
    stringVector rowNames = A.tableRowNames();
    if (!rowNames.empty()) {
        kwargs = NLSPyDict_New();
        PyObject* indexList = stringVectorToPyList(rowNames);
        NLSPyDict_SetItemString(kwargs, "index", indexList);
        NLSPy_DECREF(indexList);
    }
    PyObject* args = NLSPy_BuildValue("(O)", dataDict);
    PyObject* dataFrame = NLSPyObject_Call(dataFrameClass, args, kwargs);

    NLSPy_DECREF(args);
    NLSPy_XDECREF(kwargs);
    NLSPy_DECREF(dataFrameClass);
    NLSPy_DECREF(dataDict);
    return dataFrame;
}
//=============================================================================
PyObject*
convertTableColumn(const ArrayOf& A)
{
    if (!A.isCell()) {
        return arrayOfToPyObject(A);
    }
    ArrayOf* elements = (ArrayOf*)A.dataPointer();
    PyObject* pyObj = NLSPyList_New(A.elementCount());
    for (indexType k = 0; k < A.elementCount(); ++k) {
        NLSPyList_SetItem(pyObj, k, arrayOfToPyObject(elements[k]));
    }
    return pyObj;
}
//=============================================================================
PyObject*
stringVectorToPyList(const stringVector& values)
{
    PyObject* pyObj = NLSPyList_New(values.size());
    for (size_t k = 0; k < values.size(); ++k) {
        NLSPyList_SetItem(pyObj, k, NLSPyUnicode_FromString(values[k].c_str()));
    }
    return pyObj;
}
//=============================================================================
PyObject*
importPandasModule()
{
    PyObject* pandasModule = NLSPyImport_ImportModule("pandas");
    if (!pandasModule && NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return pandasModule;
}
//=============================================================================
static PyObject*
scalarElementToPyObject(const ArrayOf& A, indexType k)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL: {
        return NLSPyBool_FromLong((long)((const logical*)A.dataPointer())[k]);
    } break;
    case NLS_DOUBLE: {
        return NLSPyFloat_FromDouble(((const double*)A.dataPointer())[k]);
    } break;
    case NLS_SINGLE: {
        return NLSPyFloat_FromDouble((double)((const single*)A.dataPointer())[k]);
    } break;
    case NLS_INT8: {
        return NLSPyLong_FromLongLong((long long)((const int8*)A.dataPointer())[k]);
    } break;
    case NLS_INT16: {
        return NLSPyLong_FromLongLong((long long)((const int16*)A.dataPointer())[k]);
    } break;
    case NLS_INT32: {
        return NLSPyLong_FromLongLong((long long)((const int32*)A.dataPointer())[k]);
    } break;
    case NLS_INT64: {
        return NLSPyLong_FromLongLong((long long)((const int64*)A.dataPointer())[k]);
    } break;
    case NLS_UINT8: {
        return NLSPyLong_FromUnsignedLongLong(
            (unsigned long long)((const uint8*)A.dataPointer())[k]);
    } break;
    case NLS_UINT16: {
        return NLSPyLong_FromUnsignedLongLong(
            (unsigned long long)((const uint16*)A.dataPointer())[k]);
    } break;
    case NLS_UINT32: {
        return NLSPyLong_FromUnsignedLongLong(
            (unsigned long long)((const uint32*)A.dataPointer())[k]);
    } break;
    case NLS_UINT64: {
        return NLSPyLong_FromUnsignedLongLong(
            (unsigned long long)((const uint64*)A.dataPointer())[k]);
    } break;
    case NLS_DCOMPLEX: {
        const double* ptr = (const double*)A.dataPointer();
        Py_complex cplx = { ptr[2 * k], ptr[2 * k + 1] };
        return NLSPyComplex_FromCComplex(cplx);
    } break;
    case NLS_SCOMPLEX: {
        const single* ptr = (const single*)A.dataPointer();
        Py_complex cplx = { (double)ptr[2 * k], (double)ptr[2 * k + 1] };
        return NLSPyComplex_FromCComplex(cplx);
    } break;
    case NLS_STRING_ARRAY: {
        ArrayOf element = ((const ArrayOf*)A.dataPointer())[k];
        if (element.isCharacterArray()) {
            std::string str = wstring_to_utf8(element.asWideString());
            return NLSPyUnicode_FromString(str.c_str());
        }
        PyObject* none = Py_None;
        NLSPy_INCREF(none);
        return none;
    } break;
    case NLS_CELL_ARRAY: {
        ArrayOf element = ((const ArrayOf*)A.dataPointer())[k];
        return arrayOfToPyObject(element);
    } break;
    default: {
    } break;
    }
    PyObject* none = Py_None;
    NLSPy_INCREF(none);
    return none;
}
//=============================================================================
PyObject*
tableColumnToPyList(const ArrayOf& A)
{
    // A list (not a scalar) so pandas gets a 1-D sequence even for a single-row table.
    // Only the first column of a non-vector variable is exported.
    indexType nbRows = A.isEmpty() ? 0 : A.dimensions().rows();
    PyObject* list = NLSPyList_New(nbRows);
    for (indexType k = 0; k < nbRows; ++k) {
        PyObject* item = scalarElementToPyObject(A, k);
        if (!item) {
            item = Py_None;
            NLSPy_INCREF(item);
        }
        NLSPyList_SetItem(list, k, item);
    }
    return list;
}
//=============================================================================
PyObject*
rowVectorToPyArray(const void* ptrValues, size_t len, const char* dataType)
{
    if (strcmp(dataType, "?") == 0) {
        Error(L"Nelson:Python:typeNotManaged", L"Type not supported.");
    }

    PyObject* arrayModule = NLSPyImport_ImportModule("array");
    PyObject* arrayConstructor = NLSPyObject_GetAttrString(arrayModule, "array");

    PyObject* args = NLSPy_BuildValue("(s)", dataType);
    PyObject* pyArray = NLSPyObject_CallObject(arrayConstructor, args);
    for (size_t k = 0; k < len; k++) {
        PyObject* item = nullptr;
        if (!item && strcmp(dataType, "Zf") == 0) {
            Py_complex pyCplx;
            pyCplx.real = ((const single*)ptrValues)[k];
            pyCplx.imag = ((const single*)ptrValues)[k + 1];
            item = NLSPyComplex_FromCComplex(pyCplx);
        }
        if (!item && strcmp(dataType, "Zd") == 0) {
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
            NLSPy_DECREF(pyArray);
            NLSPy_DECREF(args);
            NLSPy_DECREF(arrayConstructor);
            NLSPy_DECREF(arrayModule);
            Error(L"Nelson:Python:typeNotManaged", L"Type not supported.");
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
        Error(L"Nelson:Python:typeNotManaged", L"Type not supported.");
    }
    }
    return sz;
}
//=============================================================================
// Owns buffer/shape/strides of a memoryview. PyMemoryView_FromBuffer with obj == NULL
// never reclaims them, so a PyCapsule is set as the buffer's obj and its destructor frees
// them when the memoryview is released.
struct NelsonMemoryViewOwner
{
    void* buf = nullptr;
    Py_ssize_t* shape = nullptr;
    Py_ssize_t* strides = nullptr;
    NelsonType nelsonType = NLS_DOUBLE;
    Dimensions dims;
};
//=============================================================================
static void
nelsonMemoryViewCapsuleDestructor(PyObject* capsule)
{
    auto* owner = static_cast<NelsonMemoryViewOwner*>(
        NLSPyCapsule_GetPointer(capsule, "Nelson::memoryview::owner"));
    if (owner == nullptr) {
        return;
    }
    if (owner->buf != nullptr) {
        // ArrayOf allocated this buffer and frees it on scope exit.
        ArrayOf reclaim(owner->nelsonType, owner->dims, owner->buf);
    }
    free(owner->shape);
    free(owner->strides);
    delete owner;
}
//=============================================================================
PyObject*
arrayToMemoryView(const void* data, NelsonType nelsonType, const Dimensions& dims)
{
    Py_buffer pyBuffer;
    pyBuffer.format = nelsonTypeToTypeCode(nelsonType);
    if (strcmp(pyBuffer.format, "") == 0) {
        Error(L"Nelson:Python:typeNotManaged", L"Type not supported.");
    }
    pyBuffer.ndim = (int)dims.length();

    Py_ssize_t* shape = (Py_ssize_t*)malloc(sizeof(Py_ssize_t) * dims.length());
    for (size_t k = 0; k < dims.length(); ++k) {
        shape[k] = dims.at(k);
    }
    pyBuffer.shape = shape;

    void* ptr = (void*)ArrayOf::allocateArrayOf(nelsonType, dims.elementCount());
    if (!ptr) {
        Error(L"Nelson:Python:memoryViewCreateFailed", L"Cannot create MemoryView.");
        return nullptr;
    }

    Py_ssize_t sz = getTypeSize(nelsonType);
    memcpy(ptr, data, sz * dims.elementCount());
    pyBuffer.buf = ptr;

    Py_ssize_t* strides = (Py_ssize_t*)malloc(sizeof(Py_ssize_t) * dims.length());
    Py_ssize_t stride = sz;
    for (size_t k = 0; k < dims.length(); ++k) {
        strides[k] = stride;
        stride *= (Py_ssize_t)dims.at(k);
    }
    pyBuffer.strides = strides;
    pyBuffer.suboffsets = NULL;
    pyBuffer.itemsize = sz;
    pyBuffer.internal = NULL;
    pyBuffer.len = sz * (Py_ssize_t)dims.elementCount();
    pyBuffer.readonly = 0;
    // PyMemoryView_FromBuffer takes over the obj reference.
    auto* owner = new NelsonMemoryViewOwner { ptr, shape, strides, nelsonType, dims };
    PyObject* capsule
        = NLSPyCapsule_New(owner, "Nelson::memoryview::owner", nelsonMemoryViewCapsuleDestructor);
    if (capsule == nullptr) {
        delete owner;
        free(shape);
        free(strides);
        ArrayOf r(nelsonType, dims, ptr);
        Error(L"Nelson:Python:memoryViewCreateFailed", L"Cannot create MemoryView.");
        return nullptr;
    }
    pyBuffer.obj = capsule;
    PyObject* memoryView = NLSPyMemoryView_FromBuffer(&pyBuffer);
    if (!memoryView) {
        NLSPy_DECREF(capsule);
        Error(L"Nelson:Python:memoryViewCreateFailed", L"Cannot create MemoryView.");
        return nullptr;
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
        Error(L"Nelson:Python:memoryViewBufferFailed", L"Failed to get buffer from memory view.");
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
        void* ptr = (void*)ArrayOf::allocateArrayOf(nelsonType, dims.elementCount());
        res = ArrayOf(nelsonType, dims, ptr);
        copyBufferToColumnMajor(pyBuffer, ptr);
        NLSPyBuffer_Release(&pyBuffer);
        return res;
    } break;
    case NLS_CHAR:
    case NLS_STRUCT_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
    } break;
    }

    NLSPyBuffer_Release(&pyBuffer);
    Error(L"Nelson:Python:unsupportedDataFormat", L"Unsupported data format.");

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
        return ArrayOf::handle(pythonObjectHandle);
    }

    ArrayOf res = {};
    PyTypeObject* pyTypeObject = Py_TYPE(pyObject);

    if (pyTypeObject == PyFloat_TypePtr) {
        double result = NLSPyFloat_AsDouble(pyObject);
        return ArrayOf::doubleScalar(result);
    }
    if (pyTypeObject == PyBool_TypePtr) {
        logical value = (logical)NLSPyObject_IsTrue(pyObject);
        return ArrayOf::logicalScalar(value);
    }
    if (pyTypeObject == PyComplex_TypePtr) {
        Py_complex pyCplx = NLSPyComplex_AsCComplex(pyObject);
        return ArrayOf::dsingleComplexScalar(pyCplx.real, pyCplx.imag);
    }
    needToDecreaseReference = false;
    NLSPy_INCREF(pyObject);
    PythonObjectHandle* pythonObjectHandle = new PythonObjectHandle(pyObject);
    return ArrayOf::handle(pythonObjectHandle);
}
//=============================================================================
template <typename T>
ArrayOf
PyArrayArrayToIntegerArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
        Error(L"Nelson:Python:typeNotManaged", L"Type not supported.");
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
PyArrayArrayToDoubleArrayOf(NelsonType nelsonType, PyObject* pyObject, Py_ssize_t vectorSize)
{
    Dimensions dimsVector(1, vectorSize);
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
    void* ptr = ArrayOf::allocateArrayOf(nelsonType, dimsVector.elementCount());
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
static bool
isFortranContiguousBuffer(const Py_buffer& pyBuffer)
{
    if (pyBuffer.ndim <= 0 || pyBuffer.strides == nullptr) {
        return pyBuffer.ndim <= 0;
    }
    Py_ssize_t expected = pyBuffer.itemsize;
    for (int d = 0; d < pyBuffer.ndim; ++d) {
        if (pyBuffer.shape[d] != 1 && pyBuffer.strides[d] != expected) {
            return false;
        }
        expected *= pyBuffer.shape[d];
    }
    return true;
}
//=============================================================================
static void
copyBufferToColumnMajor(const Py_buffer& pyBuffer, void* destination)
{
    const Py_ssize_t itemsize = pyBuffer.itemsize;
    const size_t nbElements = (itemsize > 0) ? (size_t)(pyBuffer.len / itemsize) : 0;
    if (nbElements == 0 || destination == nullptr) {
        return;
    }
    if (isFortranContiguousBuffer(pyBuffer)) {
        memcpy(destination, pyBuffer.buf, nbElements * (size_t)itemsize);
        return;
    }
    const int ndim = pyBuffer.ndim;
    std::vector<Py_ssize_t> strides(ndim);
    if (pyBuffer.strides != nullptr) {
        for (int d = 0; d < ndim; ++d) {
            strides[d] = pyBuffer.strides[d];
        }
    } else {
        Py_ssize_t stride = itemsize;
        for (int d = ndim - 1; d >= 0; --d) {
            strides[d] = stride;
            stride *= pyBuffer.shape[d];
        }
    }
    std::vector<Py_ssize_t> index(ndim, 0);
    const char* source = static_cast<const char*>(pyBuffer.buf);
    char* dest = static_cast<char*>(destination);
    Py_ssize_t offset = 0;
    for (size_t k = 0; k < nbElements; ++k) {
        memcpy(dest, source + offset, (size_t)itemsize);
        dest += itemsize;
        for (int d = 0; d < ndim; ++d) {
            index[d]++;
            offset += strides[d];
            if (index[d] < pyBuffer.shape[d]) {
                break;
            }
            offset -= strides[d] * pyBuffer.shape[d];
            index[d] = 0;
        }
    }
}
//=============================================================================
static PyObject*
callNoArgMethod(PyObject* obj, const char* methodName)
{
    if (!obj) {
        return nullptr;
    }
    PyObject* method = NLSPyObject_GetAttrString(obj, methodName);
    if (!method) {
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
        }
        return nullptr;
    }
    PyObject* result = NLSPyObject_CallNoArgs(method);
    NLSPy_DECREF(method);
    if (!result && NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return result;
}
//=============================================================================
static std::string
pyObjectToUtf8(PyObject* obj)
{
    std::string result;
    if (!obj) {
        return result;
    }
    if (getPythonType(obj) == PY_STR_TYPE) {
        const char* c = NLSPyUnicode_AsUTF8(obj);
        if (c) {
            result = c;
        }
        return result;
    }
    PyObject* str = NLSPyObject_Str(obj);
    if (str) {
        const char* c = NLSPyUnicode_AsUTF8(str);
        if (c) {
            result = c;
        }
        NLSPy_DECREF(str);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return result;
}
//=============================================================================
static long long
pyAttrAsLongLong(PyObject* obj, const char* name, long long defaultValue)
{
    PyObject* attr = NLSPyObject_GetAttrString(obj, name);
    long long value = defaultValue;
    if (attr) {
        value = (long long)NLSPyLong_AsLongLong(attr);
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
            value = defaultValue;
        }
        NLSPy_DECREF(attr);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return value;
}
//=============================================================================
static indexType
pandasRowCount(PyObject* dataFrame)
{
    PyObject* shape = NLSPyObject_GetAttrString(dataFrame, "shape");
    indexType nbRows = 0;
    if (shape) {
        PyObject* first = NLSPyTuple_GetItem(shape, 0); // borrowed
        if (first) {
            nbRows = (indexType)NLSPyLong_AsLongLong(first);
            if (NLSPyErr_Occurred()) {
                NLSPyErr_Clear();
                nbRows = 0;
            }
        }
        NLSPy_DECREF(shape);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return nbRows;
}
//=============================================================================
static char
pandasColumnDtypeKind(PyObject* column)
{
    char kind = 0;
    PyObject* dtype = NLSPyObject_GetAttrString(column, "dtype");
    if (dtype) {
        PyObject* kindObj = NLSPyObject_GetAttrString(dtype, "kind");
        if (kindObj) {
            const char* c = NLSPyUnicode_AsUTF8(kindObj);
            if (c && c[0]) {
                kind = c[0];
            }
            NLSPy_DECREF(kindObj);
        } else if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
        }
        NLSPy_DECREF(dtype);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    return kind;
}
//=============================================================================
static bool
pandasHasDefaultRangeIndex(PyObject* dataFrame, PyObject* pandasModule)
{
    bool isDefault = false;
    PyObject* rangeIndexClass = NLSPyObject_GetAttrString(pandasModule, "RangeIndex");
    PyObject* index = NLSPyObject_GetAttrString(dataFrame, "index");
    if (rangeIndexClass && index && NLSPyObject_IsInstance(index, rangeIndexClass) == 1) {
        long long start = pyAttrAsLongLong(index, "start", 0);
        long long step = pyAttrAsLongLong(index, "step", 1);
        isDefault = (start == 0 && step == 1);
    }
    if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    NLSPy_XDECREF(rangeIndexClass);
    NLSPy_XDECREF(index);
    return isDefault;
}
//=============================================================================
static ArrayOf
pandasNumericColumnToArrayOf(PyObject* column, indexType nbRows)
{
    ArrayOf res;
    PyObject* numpyArray = callNoArgMethod(column, "to_numpy");
    if (!numpyArray) {
        return res;
    }
    PyObject* data = NLSPyObject_GetAttrString(numpyArray, "data");
    if (data) {
        res = PyMemoryViewToArrayOf(data);
        NLSPy_DECREF(data);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    NLSPy_DECREF(numpyArray);
    // PyMemoryViewToArrayOf yields a (1, N) row; a table column must be (N, 1).
    if (nbRows > 0 && res.elementCount() == (indexType)nbRows) {
        Dimensions columnDims(nbRows, 1);
        res.reshape(columnDims);
    }
    return res;
}
//=============================================================================
static ArrayOf
pandasTextColumnToArrayOf(PyObject* column, indexType nbRows)
{
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
    ArrayOf res(NLS_STRING_ARRAY, Dimensions(nbRows, 1), elements);
    PyObject* list = callNoArgMethod(column, "tolist");
    if (list) {
        Py_ssize_t count = NLSPyList_Size(list);
        for (indexType k = 0; k < nbRows; ++k) {
            // Default to <missing> (a non-char element in a string array).
            ArrayOf element = ArrayOf::doubleScalar(std::nan(""));
            if ((Py_ssize_t)k < count) {
                PyObject* item = NLSPyList_GetItem(list, (Py_ssize_t)k); // borrowed
                if (item && item != Py_None) {
                    bool isMissing = false;
                    if (getPythonType(item) == PY_FLOAT_TYPE) {
                        double d = NLSPyFloat_AsDouble(item);
                        isMissing = std::isnan(d);
                    }
                    if (!isMissing) {
                        element = ArrayOf::characterArray(pyObjectToUtf8(item));
                    }
                }
            }
            elements[k] = element;
        }
        NLSPy_DECREF(list);
    }
    return res;
}
//=============================================================================
static ArrayOf
pandasColumnToArrayOf(PyObject* column, indexType nbRows)
{
    switch (pandasColumnDtypeKind(column)) {
    case 'b':
    case 'i':
    case 'u':
    case 'f':
    case 'c': {
        ArrayOf numeric = pandasNumericColumnToArrayOf(column, nbRows);
        if (!numeric.isEmpty() || nbRows == 0) {
            return numeric;
        }
    } break;
    default: {
        // datetime ('M'), timedelta ('m'), object/unicode/bytes/string columns
        // are exported as text; datetime values keep their ISO representation.
    } break;
    }
    return pandasTextColumnToArrayOf(column, nbRows);
}
//=============================================================================
static ArrayOf
cellFromArrayOfVector(const ArrayOfVector& values)
{
    size_t n = values.size();
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n);
    Dimensions dims(n == 0 ? 0 : 1, n);
    ArrayOf res(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < n; ++k) {
        elements[k] = values[k];
    }
    return res;
}
//=============================================================================
static ArrayOf
cellstrRowFromStringVector(const stringVector& values)
{
    size_t n = values.size();
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n);
    Dimensions dims(n == 0 ? 0 : 1, n);
    ArrayOf res(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < n; ++k) {
        elements[k] = ArrayOf::characterArray(values[k]);
    }
    return res;
}
//=============================================================================
static bool
isInstanceOfPandasClass(PyObject* pyObject, const char* className)
{
    if (!pyObject) {
        return false;
    }
    PyObject* pandasModule = importPandasModule();
    if (!pandasModule) {
        return false;
    }
    bool result = false;
    PyObject* pandasClass = NLSPyObject_GetAttrString(pandasModule, className);
    if (pandasClass) {
        result = (NLSPyObject_IsInstance(pyObject, pandasClass) == 1);
        if (NLSPyErr_Occurred()) {
            NLSPyErr_Clear();
            result = false;
        }
        NLSPy_DECREF(pandasClass);
    } else if (NLSPyErr_Occurred()) {
        NLSPyErr_Clear();
    }
    NLSPy_DECREF(pandasModule);
    return result;
}
//=============================================================================
bool
isPandasDataFrame(PyObject* pyObject)
{
    return isInstanceOfPandasClass(pyObject, "DataFrame");
}
//=============================================================================
bool
isPandasSeries(PyObject* pyObject)
{
    return isInstanceOfPandasClass(pyObject, "Series");
}
//=============================================================================
bool
PyPandasToTableData(PyObject* pyObject, ArrayOf& columns, ArrayOf& variableNames, ArrayOf& rowNames)
{
    if (!pyObject) {
        return false;
    }
    bool isDataFrame = isPandasDataFrame(pyObject);
    bool isSeries = !isDataFrame && isPandasSeries(pyObject);
    if (!isDataFrame && !isSeries) {
        return false;
    }

    PyObject* pandasModule = importPandasModule();
    if (!pandasModule) {
        return false;
    }

    // A Series becomes a single-column DataFrame (an unnamed Series yields the
    // column label 0, i.e. the variable name "0").
    PyObject* dataFrame = nullptr;
    if (isSeries) {
        dataFrame = callNoArgMethod(pyObject, "to_frame");
    } else {
        dataFrame = pyObject;
        NLSPy_INCREF(dataFrame);
    }
    if (!dataFrame) {
        NLSPy_DECREF(pandasModule);
        return false;
    }

    // A non-default index is moved into leading variable(s) via reset_index
    // (a plain index becomes a variable named 'index'); a default RangeIndex is
    // dropped so the table has no row names.
    if (!pandasHasDefaultRangeIndex(dataFrame, pandasModule)) {
        PyObject* reset = callNoArgMethod(dataFrame, "reset_index");
        if (reset) {
            NLSPy_DECREF(dataFrame);
            dataFrame = reset;
        }
    }

    indexType nbRows = pandasRowCount(dataFrame);

    stringVector names;
    ArrayOfVector columnValues;
    PyObject* columnsAttr = NLSPyObject_GetAttrString(dataFrame, "columns");
    PyObject* labels = columnsAttr ? callNoArgMethod(columnsAttr, "tolist") : nullptr;
    NLSPy_XDECREF(columnsAttr);
    if (labels) {
        Py_ssize_t nbColumns = NLSPyList_Size(labels);
        for (Py_ssize_t j = 0; j < nbColumns; ++j) {
            PyObject* label = NLSPyList_GetItem(labels, j); // borrowed
            names.push_back(pyObjectToUtf8(label));
            PyObject* column = NLSPyObject_CallMethodOneItem(dataFrame, "get", "(O)", label);
            if (column) {
                columnValues.push_back(pandasColumnToArrayOf(column, nbRows));
                NLSPy_DECREF(column);
            } else {
                if (NLSPyErr_Occurred()) {
                    NLSPyErr_Clear();
                }
                columnValues.push_back(pandasTextColumnToArrayOf(nullptr, nbRows));
            }
        }
        NLSPy_DECREF(labels);
    }
    NLSPy_DECREF(dataFrame);
    NLSPy_DECREF(pandasModule);

    columns = cellFromArrayOfVector(columnValues);
    variableNames = cellstrRowFromStringVector(names);
    rowNames = cellstrRowFromStringVector(stringVector());
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
