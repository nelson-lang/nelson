//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <Propvarutil.h>
#include "nlsBuildConfig.h"
#include "VariantConversionHelpers.hpp"
#include "ClassName.hpp"
#include "ComHandleObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
ComVariantToScalarDouble(VARIANT variant, double& value, std::wstring& errorMessage)
{
    errorMessage.clear();
    VARIANT variantConverted;
    VariantInit(&variantConverted);
    if (SUCCEEDED(VariantChangeType(&variantConverted, &variant, VARIANT_NOUSEROVERRIDE, VT_R8))) {
        value = variantConverted.dblVal;
        return true;
    }
    errorMessage = _W("VARIANT conversion fails.");
    return false;
}
//=============================================================================
bool
ComVariantToNelson(VARIANT* variant, ArrayOf& res, std::wstring& errorMessage)
{
    errorMessage.clear();
    switch (variant->vt) {
    case VT_DISPATCH:
    case VT_UNKNOWN: {
        auto* comhandle = new ComHandleObject(variant);
        res = ArrayOf::handleConstructor(comhandle);
        return true;
    } break;
    case VT_LPWSTR:
    case VT_LPSTR:
    case VT_BSTR:
    case VT_FILETIME:
    case VT_ERROR:
    case VT_DECIMAL:
    case VT_CLSID:
    case VT_DATE: {
        VARIANT variantConverted;
        VariantInit(&variantConverted);
        if (SUCCEEDED(
                VariantChangeType(&variantConverted, variant, VARIANT_NOUSEROVERRIDE, VT_BSTR))) {
            res = ArrayOf::characterArrayConstructor(variantConverted.bstrVal);
            return true;
        }
        errorMessage = _W("VARIANT conversion fails.");
        return false;

    } break;
    case VT_I2: {
        res = ArrayOf::int16Constructor(variant->iVal);
        return true;
    } break;
    case VT_UI2: {
        res = ArrayOf::uint16Constructor(variant->uiVal);
        return true;
    } break;
    case VT_I4:
    case VT_INT: {
        VARIANT variantConverted;
        VariantInit(&variantConverted);
        if (SUCCEEDED(
                VariantChangeType(&variantConverted, variant, VARIANT_NOUSEROVERRIDE, VT_I4))) {
            res = ArrayOf::int32Constructor(variantConverted.lVal);
            return true;
        }
        errorMessage = _W("VARIANT conversion fails.");
        return false;

    } break;
    case VT_UI4:
    case VT_UINT: {
        VARIANT variantConverted;
        VariantInit(&variantConverted);
        if (SUCCEEDED(
                VariantChangeType(&variantConverted, variant, VARIANT_NOUSEROVERRIDE, VT_UI4))) {
            res = ArrayOf::uint32Constructor(variantConverted.ulVal);
            return true;
        }
        errorMessage = _W("VARIANT conversion fails.");
        return false;

    } break;
    case VT_I8: {
        res = ArrayOf::int64Constructor(variant->llVal);
        return true;
    } break;
    case VT_UI8: {
        res = ArrayOf::uint64Constructor(variant->ullVal);
        return true;
    } break;
    case VT_BOOL: {
        res = ArrayOf::logicalConstructor(variant->boolVal == VARIANT_TRUE ? true : false);
        return true;
    } break;
    case VT_R4: {
        res = ArrayOf::singleConstructor(variant->fltVal);
        return true;
    } break;
    case VT_R8:
    case VT_CY: {
        double value;
        bool bOK = ComVariantToScalarDouble(variant[0], value, errorMessage);
        if (bOK) {
            res = ArrayOf::doubleConstructor(value);
            return true;
        }
        errorMessage = _W("VARIANT conversion fails.");
        return false;

    } break;
    case VT_I1: {
        res = ArrayOf::int8Constructor(variant->cVal);
        return true;
    } break;
    case VT_UI1: {
        res = ArrayOf::uint8Constructor(variant->bVal);
        return true;
    } break;
    case VT_EMPTY: {
        Dimensions dims(0, 0);
        res = ArrayOf::emptyConstructor(dims);
        return true;
    }
    default: {
        if (variant->vt & VT_ARRAY) {
            int subtype = variant->vt & VT_TYPEMASK;
            SAFEARRAY* arr = variant->parray;
            int dimCount;
            dimCount = SafeArrayGetDim(arr);
            if (dimCount > 2) {
                errorMessage = L"maximum array dimensions supported is 2.";
                return false;
            }
            Dimensions dims(2);
            for (int k = 0; k < dimCount; k++) {
                long lb, ub;
                SafeArrayGetLBound(arr, k + 1, &lb);
                SafeArrayGetUBound(arr, k + 1, &ub);
                dims.setDimensionLength(k, ub - lb + 1);
            }
            switch (subtype) {
            case VT_VARIANT: {
                VARIANT* pvar;
                HRESULT hr;
                ArrayOf* pCell = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                    NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false));
                hr = SafeArrayAccessData(arr, reinterpret_cast<void**>(&pvar));
                if (FAILED(hr)) {
                    errorMessage = L"Failed accessing array data.";
                    return false;
                }
                indexType elementCount = dims.getElementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    ArrayOf element;
                    if (ComVariantToNelson(&pvar[k], element, errorMessage)) {
                        pCell[k] = element;
                    } else {
                        delete[] pCell;
                        return false;
                    }
                }
                SafeArrayUnaccessData(arr);
                res = ArrayOf(NLS_CELL_ARRAY, dims, pCell);
                return true;

            } break;
            case VT_R4: {
                VARIANT* pvar;
                HRESULT hr;
                FLOAT cellVal;
                Dimensions dims(2);
                for (int k = 0; k < dimCount; k++) {
                    long lb, ub;
                    SafeArrayGetLBound(arr, k + 1, &lb);
                    SafeArrayGetUBound(arr, k + 1, &ub);
                    dims.setDimensionLength(k, ub - lb + 1);
                }
                single* pSingle = static_cast<single*>(ArrayOf::allocateArrayOf(
                    NLS_SINGLE, dims.getElementCount(), Nelson::stringVector(), true));
                hr = SafeArrayAccessData(arr, reinterpret_cast<void**>(&pvar));
                if (FAILED(hr)) {
                    errorMessage = L"Failed accessing array data.";
                    return false;
                }
                long arrDims[2] = { 0, 0 };
                for (int dimNum = 0; dimNum < dimCount; dimNum++) {
                    arrDims[0] = dimNum;
                    for (indexType k = 0; k < dims.getColumns(); k++) {
                        arrDims[dimCount - 1] = (long)k;
                        if (SafeArrayGetElement(arr, arrDims, &cellVal) == S_OK) {
                            if (dimCount == 2) {
                                pSingle[dims.getRows() * k + dimNum] = cellVal;
                            } else {
                                pSingle[k] = cellVal;
                            }
                        } else {
                            errorMessage = L"error accessing value";
                            delete[] pSingle;
                            return false;
                        }
                    }
                }
                res = ArrayOf(NLS_SINGLE, dims, pSingle);
                SafeArrayUnaccessData(arr);
                return true;

            } break;
            case VT_R8: {
                VARIANT* pvar;
                HRESULT hr;
                DOUBLE cellVal;
                Dimensions dims(2);
                for (int k = 0; k < dimCount; k++) {
                    long lb, ub;
                    SafeArrayGetLBound(arr, k + 1, &lb);
                    SafeArrayGetUBound(arr, k + 1, &ub);
                    dims.setDimensionLength(k, ub - lb + 1);
                }
                double* pDouble = static_cast<double*>(ArrayOf::allocateArrayOf(
                    NLS_DOUBLE, dims.getElementCount(), Nelson::stringVector(), true));
                hr = SafeArrayAccessData(arr, reinterpret_cast<void**>(&pvar));
                if (FAILED(hr)) {
                    errorMessage = L"Failed accessing array data.";
                    return false;
                }
                long arrDims[2] = { 0, 0 };
                for (int dimNum = 0; dimNum < dimCount; dimNum++) {
                    arrDims[0] = dimNum;
                    for (indexType k = 0; k < dims.getColumns(); k++) {
                        arrDims[dimCount - 1] = (long)k;
                        if (SafeArrayGetElement(arr, arrDims, &cellVal) == S_OK) {
                            if (dimCount == 2) {
                                pDouble[dims.getRows() * k + dimNum] = cellVal;
                            } else {
                                pDouble[k] = cellVal;
                            }
                        } else {
                            errorMessage = L"error accessing value";
                            delete[] pDouble;
                            return false;
                        }
                    }
                }
                res = ArrayOf(NLS_DOUBLE, dims, pDouble);
                SafeArrayUnaccessData(arr);
                return true;

            } break;
            case VT_UI1: {
                VARIANT* pvar;
                HRESULT hr;
                BYTE cellVal;
                Dimensions dims(2);
                for (int k = 0; k < dimCount; k++) {
                    long lb, ub;
                    SafeArrayGetLBound(arr, k + 1, &lb);
                    SafeArrayGetUBound(arr, k + 1, &ub);
                    dims.setDimensionLength(k, ub - lb + 1);
                }
                uint8* pUint8 = static_cast<uint8*>(ArrayOf::allocateArrayOf(
                    NLS_UINT8, dims.getElementCount(), Nelson::stringVector(), true));
                hr = SafeArrayAccessData(arr, reinterpret_cast<void**>(&pvar));
                if (FAILED(hr)) {
                    errorMessage = L"Failed accessing array data.";
                    return false;
                }
                long arrDims[2] = { 0, 0 };
                for (int dimNum = 0; dimNum < dimCount; dimNum++) {
                    arrDims[0] = dimNum;
                    for (indexType k = 0; k < dims.getColumns(); k++) {
                        arrDims[dimCount - 1] = (long)k;
                        if (SafeArrayGetElement(arr, arrDims, &cellVal) == S_OK) {
                            if (dimCount == 2) {
                                pUint8[dims.getRows() * k + dimNum] = cellVal;
                            } else {
                                pUint8[k] = cellVal;
                            }
                        } else {
                            errorMessage = L"error accessing value";
                            delete[] pUint8;
                            return false;
                        }
                    }
                }
                res = ArrayOf(NLS_UINT8, dims, pUint8);
                SafeArrayUnaccessData(arr);
                return true;

            } break;
            case VT_I2: {
                VARIANT* pvar;
                HRESULT hr;
                SHORT cellVal;
                Dimensions dims(2);
                for (int k = 0; k < dimCount; k++) {
                    long lb, ub;
                    SafeArrayGetLBound(arr, k + 1, &lb);
                    SafeArrayGetUBound(arr, k + 1, &ub);
                    dims.setDimensionLength(k, ub - lb + 1);
                }
                int16* pInt16 = static_cast<int16*>(ArrayOf::allocateArrayOf(
                    NLS_INT16, dims.getElementCount(), Nelson::stringVector(), true));
                hr = SafeArrayAccessData(arr, reinterpret_cast<void**>(&pvar));
                if (FAILED(hr)) {
                    errorMessage = L"Failed accessing array data.";
                    return false;
                }
                long arrDims[2] = { 0, 0 };
                for (int dimNum = 0; dimNum < dimCount; dimNum++) {
                    arrDims[0] = dimNum;
                    for (indexType k = 0; k < dims.getColumns(); k++) {
                        arrDims[dimCount - 1] = (long)k;
                        if (SafeArrayGetElement(arr, arrDims, &cellVal) == S_OK) {
                            if (dimCount == 2) {
                                pInt16[dims.getRows() * k + dimNum] = cellVal;
                            } else {
                                pInt16[k] = cellVal;
                            }
                        } else {
                            errorMessage = L"error accessing value";
                            delete[] pInt16;
                            return false;
                        }
                    }
                }
                res = ArrayOf(NLS_INT16, dims, pInt16);
                SafeArrayUnaccessData(arr);
                return true;

            } break;
            default: {
                errorMessage = L"cannot convert COM variant.";
                return false;
            } break;
            }
        } else {
            errorMessage = L"cannot convert COM variant.";
            return false;
        }
    } break;
    }
    return false;
}
//=============================================================================
static SAFEARRAY*
makeSafeArrayFromDimensions(const Dimensions& dims, VARTYPE vt)
{
    auto* bounds = static_cast<SAFEARRAYBOUND*>(
        LocalAlloc(LMEM_FIXED | LMEM_ZEROINIT, sizeof(SAFEARRAYBOUND) * dims.getLength()));
    if (bounds) {
        indexType len = dims.getLength();
        Dimensions tmp(dims);
        for (indexType k = 0; k < len; k++) {
            indexType v = tmp[k];
            bounds[k].cElements = static_cast<ULONG>(v);
        }
        SAFEARRAY* arr = SafeArrayCreate(vt, static_cast<UINT>(len), bounds);
        LocalFree(bounds);
        return arr;
    }
    return nullptr;
}
//=============================================================================
bool
NelsonToComVariant(const ArrayOf& A, VARIANT* variant, std::wstring& errorMessage)
{
    errorMessage.clear();
    NelsonType type = A.getDataClass();
    if (A.isSparse()) {
        errorMessage = _W("Sparse not supported.");
        return false;
    }
    if (!A.is2D()) {
        errorMessage = _W("N dimensions array not supported.");
        return false;
    }
    VariantInit(variant);
    VariantClear(variant);
    if (A.isEmpty()) {
        variant->vt = VT_EMPTY;
        return true;
    }
    if (A.isScalar()) {
        switch (type) {
        case NLS_HANDLE: {
            std::string name;
            ClassName(A, name);
            if (name == NLS_HANDLE_COM_CATEGORY_STR) {
                HandleGenericObject* ptr = A.getContentAsHandleScalar();
                if (ptr) {
                    auto* pVariant = static_cast<VARIANT*>(ptr->getPointer());
                    if (pVariant) {
                        variant->vt = VT_DISPATCH;
                        variant->pdispVal = pVariant->pdispVal;
                        variant->pdispVal->AddRef();
                        return true;
                    }
                }
            }
            errorMessage = _W("VARIANT conversion fails.");
            return false;
        } break;
        case NLS_STRING_ARRAY:
        case NLS_CELL_ARRAY: {
            auto* cell = (ArrayOf*)(A.getDataPointer());
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_VARIANT);
            if (arr) {
                VARIANT* data;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                indexType elementCount = dims.getElementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    if (!NelsonToComVariant(cell[k], &data[k], errorMessage)) {
                        return false;
                    }
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_VARIANT;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_LOGICAL: {
            logical r = A.getContentAsLogicalScalar();
            variant->vt = VT_BOOL;
            variant->boolVal = r;
            return true;
        } break;
        case NLS_UINT8: {
            uint8 r = A.getContentAsUnsignedInteger8Scalar();
            variant->vt = VT_UI1;
            variant->bVal = r;
            return true;
        } break;
        case NLS_INT8: {
            int8 r = A.getContentAsInteger8Scalar();
            variant->vt = VT_I1;
            variant->cVal = r;
            return true;
        } break;
        case NLS_UINT16: {
            uint16 r = A.getContentAsUnsignedInteger16Scalar();
            variant->vt = VT_UI2;
            variant->uiVal = r;
            return true;
        } break;
        case NLS_INT16: {
            int16 r = A.getContentAsInteger16Scalar();
            variant->vt = VT_I2;
            variant->iVal = r;
            return true;
        } break;
        case NLS_UINT32: {
            uint32 r = A.getContentAsUnsignedInteger32Scalar();
            variant->vt = VT_UINT;
            variant->uintVal = r;
            return true;
        } break;
        case NLS_INT32: {
            int32 r = A.getContentAsInteger32Scalar();
            variant->vt = VT_INT;
            variant->uintVal = r;
            return true;
        } break;
        case NLS_UINT64: {
            uint64 r = A.getContentAsUnsignedInteger64Scalar();
            variant->vt = VT_UI8;
            variant->llVal = r;
            return true;
        } break;
        case NLS_INT64: {
            int64 r = A.getContentAsInteger64Scalar();
            variant->vt = VT_I8;
            variant->llVal = r;
            return true;
        } break;
        case NLS_SINGLE: {
            single r = A.getContentAsSingleScalar();
            variant->vt = VT_R4;
            variant->fltVal = r;
            return true;
        } break;
        case NLS_DOUBLE: {
            double r = A.getContentAsDoubleScalar();
            VariantClear(variant);
            variant->vt = VT_R8;
            variant->dblVal = r;
            return true;
        } break;
        case NLS_CHAR: {
            std::wstring wstr = A.getContentAsWideString();
            variant->vt = VT_BSTR;
            variant->bstrVal = SysAllocString(wstr.c_str());
            return true;
        } break;
        case NLS_STRUCT_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_FUNCTION_HANDLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        default: {
            errorMessage = _W("VARIANT conversion fails.");
            return false;
        } break;
        }
    } else {
        switch (type) {
        case NLS_CHAR: {
            if (A.isVector()) {
                std::wstring wstr = A.getContentAsWideString();
                VariantClear(variant);
                variant->vt = VT_BSTR;
                variant->bstrVal = SysAllocString(wstr.c_str());
                return true;
            }
            errorMessage = _W("VARIANT conversion fails.");
            return false;

        } break;
        case NLS_STRING_ARRAY:
        case NLS_CELL_ARRAY: {
            auto* cell = (ArrayOf*)(A.getDataPointer());
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_VARIANT);
            if (arr) {
                VARIANT* data;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                indexType elementCount = dims.getElementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    if (!NelsonToComVariant(cell[k], &data[k], errorMessage)) {
                        return false;
                    }
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_VARIANT;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_DOUBLE: {
            auto* pDouble = (double*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_R8);
            if (arr) {
                double* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pDouble[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_R8;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_SINGLE: {
            auto* pSingle = static_cast<single*>(
                const_cast<void*>(static_cast<const void*>(A.getDataPointer())));
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_R4);
            if (arr) {
                single* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pSingle[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_R4;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_LOGICAL: {
            auto* pLogical = (logical*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_BOOL);
            if (arr) {
                logical* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pLogical[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_BOOL;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_UINT8: {
            auto* pUint8 = (uint8*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_UI1);
            if (arr) {
                uint8* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pUint8[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_UI1;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_INT8: {
            int8* pInt8 = (int8*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_I1);
            if (arr) {
                int8* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pInt8[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_I1;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_UINT16: {
            auto* pUint16 = (uint16*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_UI2);
            if (arr) {
                uint16* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pUint16[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_UI2;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_INT16: {
            auto* pInt16 = (int16*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_I2);
            if (arr) {
                int16* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pInt16[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_I2;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_UINT32: {
            auto* pUint32 = (uint32*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_UI4);
            if (arr) {
                uint32* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pUint32[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_UI4;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_INT32: {
            auto* pInt32 = (int32*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_I4);
            if (arr) {
                int32* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pInt32[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_I4;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_UINT64: {
            auto* pUint64 = (uint64*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_UI8);
            if (arr) {
                uint64* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pUint64[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_UI8;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_INT64: {
            auto* pInt64 = (int64*)A.getDataPointer();
            Dimensions dims = A.getDimensions();
            SAFEARRAY* arr = makeSafeArrayFromDimensions(dims, VT_I8);
            if (arr) {
                int64* data = nullptr;
                SafeArrayAccessData(arr, reinterpret_cast<void**>(&data));
                ompIndexType elementCount = dims.getElementCount();
#if WITH_OPENMP
#pragma omp parallel for
#endif
                for (ompIndexType k = 0; k < elementCount; k++) {
                    data[k] = pInt64[k];
                }
                SafeArrayUnaccessData(arr);
                variant->vt = VT_ARRAY | VT_I8;
                variant->parray = arr;
                return true;
            }
        } break;
        case NLS_STRUCT_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_FUNCTION_HANDLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        default: {
            errorMessage = _W("VARIANT conversion fails.");
            return false;
        } break;
        }
    }

    return false;
}
//=============================================================================
} // namespace Nelson
