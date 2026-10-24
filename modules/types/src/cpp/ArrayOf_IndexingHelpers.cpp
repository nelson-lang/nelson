//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4724)
#endif
//=============================================================================
#include <cwchar>
#include <cstring>
#include "lapack_eigen_config.hpp"
#include "omp_for_loop.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <algorithm>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <limits>
#include <memory>
#include "StringHelpers.hpp"
#include "ArrayOf.hpp"
#include "ArrayOfView.hpp"
#include "Data.hpp"
#include "HandleState.hpp"
#include "IEEEFP.hpp"
#include "SparseType.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
ordinalIndexClassForIndexingView();
//=============================================================================
bool*
ArrayOf::getBinaryMap(indexType maxD)
{
    materializeScalar();
    bool* map = new_with_exception<bool>(maxD, true);
    indexType N = elementCount();
    ArrayOfView<indexType> indexView(*this, ordinalIndexClassForIndexingView());
    constIndexPtr rp = indexView.data();
    for (indexType i = 0; i < N; i++) {
        indexType n = (rp[i] - 1);
#ifdef NLS_INDEX_TYPE_64
        if (n >= maxD)
#else
        if (n < 0 || n >= maxD)
#endif
        {
            if (map) {
                delete[] map;
                map = nullptr;
            }
            if (n != 0) {
                Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
            }
        }
        if (map) {
            map[n] = true;
        }
    }
    return map;
}
//=============================================================================
indexType
ArrayOf::maxAsIndex()
{
    materializeScalar();
    indexType maxval;
    if (elementCount() == 0) {
        // An empty index (e.g. an all-false logical mask) selects no elements:
        // the maximum required bound is 0, not an error.
        return 0;
    }
    if (dp == nullptr) {
        Error(L"Nelson:types:invalidIndex:invalidIndex", L"Invalid index");
    }
    ArrayOfView<indexType> indexView(*this, ordinalIndexClassForIndexingView());
    constIndexPtr rp = indexView.data();
    if (rp == nullptr) {
        Error(L"Nelson:types:invalidIndex:illegalZeroNegativeIndex",
            L"Illegal zero or negative index");
    }
    indexType K = elementCount();
    maxval = rp[0];
    for (indexType k = 1; k < K; k++) {
        if (rp[k] > maxval) {
            maxval = rp[k];
        }
    }
    if (maxval <= 0) {
        Error(L"Nelson:types:invalidIndex:illegalZeroNegativeIndex",
            L"Illegal zero or negative index");
    }
    return maxval;
}
//=============================================================================
namespace {
    //=============================================================================
    NelsonType
    ordinalIndexClass()
    {
#ifdef NLS_INDEX_TYPE_64
        return NLS_UINT64;
#else
        return NLS_UINT32;
#endif
    }
    //=============================================================================
    void
    validatePositiveOrdinal(indexType value)
    {
        if (value <= 0) {
            Error(L"Nelson:types:invalidIndex:zeroNegativeIndexEncountered",
                L"Zero or negative index encountered.");
        }
    }
    //=============================================================================
    void
    errorCannotConvertToOrdinal(NelsonType dataClass)
    {
        switch (dataClass) {
        case NLS_CHAR:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"string data types");
            break;
        case NLS_GO_HANDLE:
        case NLS_HANDLE:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"handle arrays");
            break;
        case NLS_CELL_ARRAY:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"cell arrays");
            break;
        case NLS_STRING_ARRAY:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"string arrays");
            break;
        case NLS_FUNCTION_HANDLE:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"function_handle arrays");
            break;
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"class arrays");
            break;
        case NLS_STRUCT_ARRAY:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"structure arrays");
            break;
        default:
            Error(L"Nelson:types:cannotConvertToIndex", L"Cannot convert %s to indices.",
                L"unknown type");
            break;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
Data*
ArrayOf::putOrdinalData(const Dimensions& dimensions, indexType* values)
{
    return dp->putData(ordinalIndexClass(), dimensions, values);
}
//=============================================================================
Data*
ArrayOf::logicalOrdinalData(indexType elementCount, bool isRowVector)
{
    const logical* rp = (const logical*)dp->data();
    int indexCount = 0;
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        if (rp[i] != 0) {
            indexCount++;
        }
    }
    indexType* lp = new_with_exception<indexType>(indexCount, false);
    indexType* qp = lp;
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        if (rp[i] != 0) {
            *qp++ = (indexType)(i + 1);
        }
    }
    Dimensions dimensions;
    if (isRowVector) {
        dimensions[1] = indexCount;
        dimensions[0] = 1;
    } else {
        dimensions[1] = 1;
        dimensions[0] = indexCount;
    }
    return putOrdinalData(dimensions, lp);
}
//=============================================================================
template <class T>
Data*
ArrayOf::realOrdinalData(indexType elementCount, indexType stride)
{
    const T* rp = (const T*)dp->data();
    std::unique_ptr<indexType[]> lp(new_with_exception<indexType>(elementCount, false));
    // An exclusive power-of-two bound is exact even when the largest ordinal
    // cannot be represented as a double. Validate before any integer cast.
    const double ordinalLimit = std::ldexp(1.0, std::numeric_limits<indexType>::digits);
    for (indexType i = 0; i < elementCount; i++) {
        const double value = static_cast<double>(rp[i * stride]);
        if (!std::isfinite(value) || value < 0 || value >= ordinalLimit
            || std::trunc(value) != value) {
            Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                L"index must either be real positive integers or logicals.");
        }
        const indexType ndx = static_cast<indexType>(value);
        validatePositiveOrdinal(ndx);
        lp[i] = ndx;
    }
    return putOrdinalData(dp->dimensions(), lp.release());
}
//=============================================================================
template <class T>
Data*
ArrayOf::integerOrdinalData(indexType elementCount)
{
    const T* rp = (const T*)dp->data();
    indexType* lp = new_with_exception<indexType>(elementCount, false);
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        // check before the unsigned cast: a negative value would wrap
        // around to a huge positive ordinal and evade validation.
        if (rp[i] <= 0) {
            Error(L"Nelson:types:invalidIndex:zeroNegativeIndexEncountered",
                L"Zero or negative index encountered.");
        }
        indexType ndx = (indexType)rp[i];
        validatePositiveOrdinal(ndx);
        lp[i] = ndx;
    }
    return putOrdinalData(dp->dimensions(), lp);
}
//=============================================================================
Data*
ArrayOf::uint64OrdinalData(indexType elementCount)
{
    const uint64* rp = (const uint64*)dp->data();
    indexType* lp = new_with_exception<indexType>(elementCount, false);
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        if (rp[i] > std::numeric_limits<indexType>::max()) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        indexType ndx = (indexType)rp[i];
        validatePositiveOrdinal(ndx);
        lp[i] = ndx;
    }
    return putOrdinalData(dp->dimensions(), lp);
}
//=============================================================================
void
ArrayOf::toOrdinalType()
{
    if (isSparse()) {
        makeDense();
    }
    materializeScalar();
    indexType nbElements = elementCount();
    switch (dp->dataClass) {
    case NLS_LOGICAL:
        dp = logicalOrdinalData(nbElements, isRowVector());
        break;
    case NLS_DCOMPLEX:
        RaiseWarning(L"Nelson:imaginaryPartIgnored",
            resolveMessageTemplate(L"Nelson:types:imaginaryPartComplexIndex",
                L"Imaginary part of complex index ignored.")
                + L"\n");
        dp = realOrdinalData<double>(nbElements, 2);
        break;
    case NLS_SCOMPLEX:
        RaiseWarning("Nelson:imaginaryPartIgnored", "Imaginary part of complex index ignored.\n");
        dp = realOrdinalData<single>(nbElements, 2);
        break;
    case NLS_DOUBLE:
        dp = realOrdinalData<double>(nbElements, 1);
        break;
    case NLS_SINGLE:
        dp = realOrdinalData<single>(nbElements, 1);
        break;
    case NLS_INT64:
        dp = integerOrdinalData<int64>(nbElements);
        break;
    case NLS_UINT64:
        dp = uint64OrdinalData(nbElements);
        break;
    case NLS_INT32:
        dp = integerOrdinalData<int32>(nbElements);
        break;
    case NLS_UINT32:
        dp = integerOrdinalData<uint32>(nbElements);
        break;
    case NLS_INT16:
        dp = integerOrdinalData<int16>(nbElements);
        break;
    case NLS_UINT16:
        dp = integerOrdinalData<uint16>(nbElements);
        break;
    case NLS_INT8:
        dp = integerOrdinalData<int8>(nbElements);
        break;
    case NLS_UINT8:
        dp = integerOrdinalData<uint8>(nbElements);
        break;
    default:
        errorCannotConvertToOrdinal(dp->dataClass);
        break;
    }
}
//=============================================================================
void
ArrayOf::copyElements(indexType srcIndex, void* dstPtr, indexType dstIndex, indexType count)
{
    indexType elSize(elementSize());
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"copyElements");
    }
    if (hasScalarStorage()) {
        if (count == 0) {
            return;
        }
        if (srcIndex != 0 || count > 1) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        char* qp = static_cast<char*>(dstPtr);
        memcpy(qp + dstIndex * elSize, dataPointer(), count * elSize);
        return;
    }
    switch (dp->dataClass) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->data();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        for (indexType i = 0; i < count; i++) {
            qp[dstIndex + i] = sp[srcIndex + i];
        }
    } break;
    case NLS_FUNCTION_HANDLE: {
        const ArrayOf* sp = (const ArrayOf*)dp->data();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        indexType fieldCount(dp->fieldNames().size());
        for (indexType i = 0; i < count; i++) {
            for (indexType j = 0; j < (indexType)fieldCount; j++) {
                qp[(dstIndex + i) * fieldCount + j] = sp[(srcIndex + i) * fieldCount + j];
            }
        }
    } break;
    case NLS_CLASS_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->data();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        indexType fieldCount(dp->fieldNames().size());
        for (indexType i = 0; i < count; i++) {
            for (indexType j = 0; j < (indexType)fieldCount; j++) {
                qp[(dstIndex + i) * fieldCount + j] = sp[(srcIndex + i) * fieldCount + j];
            }
        }
    } break;
    case NLS_CLASSDEF_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->data();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        indexType fieldCount(dp->fieldNames().size());
        for (indexType i = 0; i < count; i++) {
            for (indexType j = 0; j < (indexType)fieldCount; j++) {
                qp[(dstIndex + i) * fieldCount + j] = sp[(srcIndex + i) * fieldCount + j];
            }
        }
    } break;
    case NLS_STRUCT_ARRAY: {
        const ArrayOf* sp = (const ArrayOf*)dp->data();
        ArrayOf* qp = (ArrayOf*)dstPtr;
        indexType fieldCount(dp->fieldNames().size());
        for (indexType i = 0; i < count; i++) {
            for (indexType j = 0; j < (indexType)fieldCount; j++) {
                qp[(dstIndex + i) * fieldCount + j] = sp[(srcIndex + i) * fieldCount + j];
            }
        }
    } break;
    case NLS_HANDLE: {
        const HandleRef* sp = (const HandleRef*)dp->data();
        HandleRef* qp = (HandleRef*)dstPtr;
        for (indexType i = 0; i < count; ++i) {
            qp[dstIndex + i] = sp[srcIndex + i];
        }
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    default: {
        const char* sp = (const char*)dp->data();
        if (sp != nullptr) {
            char* qp = (char*)dstPtr;
            memcpy(qp + dstIndex * elSize, sp + srcIndex * elSize, count * elSize);
        }
    } break;
    }
}
//=============================================================================
static bool
isDoubleClass(NelsonType classIn)
{
    return (classIn == NLS_DOUBLE || classIn == NLS_DCOMPLEX);
}
//=============================================================================
static bool
isSingleClass(NelsonType classIn)
{
    return (classIn == NLS_SINGLE || classIn == NLS_SCOMPLEX);
}
//=============================================================================
static bool
isDoubleOrSingleClass(NelsonType classIn)
{
    return (isSingleClass(classIn) || isDoubleClass(classIn));
}
//=============================================================================
#undef caseMacro
//=============================================================================
indexType
ArrayOf::asScalarIndex(
    bool bWithZero, bool checkIsIntegerValue, bool convertNegativeValueAsZero) const
{
    indexType idx = 0;
    if (elementCount() != 1) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    double valueAsDouble;
    ArrayOf P;
    if (dataClass() != NLS_DOUBLE) {
        P = *this;
        P.promoteType(NLS_DOUBLE);
        double* qp = (double*)P.dataPointer();
        valueAsDouble = qp[0];
    } else {
        const double* qp = static_cast<const double*>(dataPointer());
        valueAsDouble = qp[0];
    }
    if ((floor(valueAsDouble) == valueAsDouble) && IsFinite((valueAsDouble))) {
        double maxIndexType = (double)std::numeric_limits<indexType>::max();
        if ((valueAsDouble) > maxIndexType) {
            idx = static_cast<indexType>(maxIndexType);
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        } else if (valueAsDouble < 0) {
            if (convertNegativeValueAsZero) {
                idx = static_cast<indexType>(0);
            } else {
                Error(L"Nelson:types:invalidValue", L"Expected a positive integer scalar.");
            }
        } else {
            idx = static_cast<indexType>(valueAsDouble);
        }
    } else {
        if (IsFinite(valueAsDouble)) {
            Error(L"Nelson:integerValueExpected", L"A real integer value scalar expected.");
        } else {
            Error(L"Nelson:types:nanInfNotAllowed", L"NaN and Inf not allowed.");
        }
    }
    if (!bWithZero) {
        if (idx == 0) {
            Error(L"Nelson:types:invalidDimensions",
                L"Dimension argument must be a positive integer scalar within indexing range.");
        }
    }
    return idx;
}
//=============================================================================
std::vector<indexType>
ArrayOf::asIndexVector()
{
    std::vector<indexType> index;
    promoteType(NLS_DOUBLE);
    const double* qp = static_cast<const double*>(dataPointer());
    size_t nbElements = elementCount();
    constexpr double maxIndexType = (double)std::numeric_limits<indexType>::max();
    for (size_t k = 0; k < nbElements; k++) {
        if ((fabs(floor(qp[k]) - qp[k]) < std::numeric_limits<double>::epsilon())
            && IsFinite((qp[k]))) {
            if ((qp[k]) > maxIndexType) {
                index.push_back(std::numeric_limits<indexType>::max());
            } else if (qp[k] < 0) {
                index.push_back(0);
            } else {
                double dVal = qp[k];
                index.push_back(static_cast<indexType>(dVal));
            }
        } else {
            if (IsFinite(qp[k])) {
                Error(L"Nelson:types:invalidIndex", L"%s must be an integer.", L"Index");
            } else {
                Error(L"Nelson:types:nanInfNotAllowed", L"NaN and Inf not allowed.");
            }
        }
    }
    return index;
}
//=============================================================================
indexType*
ArrayOf::indexPointer()
{
    promoteType(NLS_DOUBLE);
    const double* qp = static_cast<const double*>(dataPointer());
    size_t nbElements = elementCount();
    indexType* pIndex = new_with_exception<indexType>(nbElements, false);
    constexpr double maxIndexType = (double)std::numeric_limits<indexType>::max();
    for (size_t k = 0; k < nbElements; k++) {
        if ((fabs(floor(qp[k]) - qp[k]) < std::numeric_limits<double>::epsilon())
            && IsFinite((qp[k]))) {
            if ((qp[k]) > maxIndexType) {
                pIndex[k] = std::numeric_limits<indexType>::max();
            } else if (qp[k] < 0) {
                pIndex[k] = 0;
            } else {
                double dVal = qp[k];
                pIndex[k] = static_cast<indexType>(dVal);
            }
        } else {
            delete[] pIndex;
            if (IsFinite(qp[k])) {
                Error(L"Nelson:types:invalidIndex", L"%s must be an integer.", L"Index");
            } else {
                Error(L"Nelson:types:nanInfNotAllowed", L"NaN and Inf not allowed.");
            }
        }
    }
    return pIndex;
}
//=============================================================================
bool
isColonOperator(const ArrayOf& A)
{
    if ((A.dataClass() == NLS_CHAR) && (A.elementCount() == 1)) {
        std::wstring str = A.asWideString();
        return (str == L":");
    }
    return false;
}
//=============================================================================
static NelsonType
ordinalIndexClassForIndexingView()
{
#ifdef NLS_INDEX_TYPE_64
    return NLS_UINT64;
#else
    return NLS_UINT32;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
