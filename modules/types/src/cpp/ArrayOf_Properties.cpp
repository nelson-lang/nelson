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
#include "StringHelpers.hpp"
#include "CharRowComparison.hpp"
#include "ClassName.hpp"
#include "ArrayOf.hpp"
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
template <class T>
static bool
sparseImaginaryPartIsZero(const void* spData);
//=============================================================================
NelsonType
ArrayOf::dataClass() const
{
    if (hasScalarStorage()) {
        return static_cast<NelsonType>(scalarType_);
    }
    return (dp == nullptr) ? NLS_DOUBLE : dp->dataClass;
}
//=============================================================================
indexType
ArrayOf::elementSize() const
{
    if (hasScalarStorage()) {
        return scalarStorageSize(static_cast<NelsonType>(scalarType_));
    }
    if (dp == nullptr) {
        Error(L"Nelson:types:typeNotManaged", L"Type not supported.");
    }
    switch (dp->dataClass) {
    case NLS_HANDLE:
        return sizeof(HandleRef);
    case NLS_GO_HANDLE:
        return sizeof(nelson_handle);
    case NLS_STRING_ARRAY:
        return sizeof(ArrayOf);
    case NLS_CELL_ARRAY:
        return sizeof(ArrayOf);
    case NLS_FUNCTION_HANDLE:
        return (sizeof(ArrayOf) * dp->fieldNames().size());
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
        return (sizeof(ArrayOf) * dp->fieldNames().size());
    case NLS_STRUCT_ARRAY:
        return (sizeof(ArrayOf) * dp->fieldNames().size());
    case NLS_LOGICAL:
        return sizeof(logical);
    case NLS_UINT8:
        return sizeof(uint8);
    case NLS_INT8:
        return sizeof(int8);
    case NLS_UINT16:
        return sizeof(uint16);
    case NLS_INT16:
        return sizeof(int16);
    case NLS_UINT32:
        return sizeof(uint32);
    case NLS_INT32:
        return sizeof(int32);
    case NLS_UINT64:
        return sizeof(uint64);
    case NLS_INT64:
        return sizeof(int64);
    case NLS_SINGLE:
        return sizeof(single);
    case NLS_DOUBLE:
        return sizeof(double);
    case NLS_SCOMPLEX:
        return sizeof(single) * 2;
    case NLS_DCOMPLEX:
        return sizeof(double) * 2;
    case NLS_CHAR:
        return sizeof(charType);
    default: {
    } break;
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::byteSize() const
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported:byteSizeCalculationNotSupportedSparse",
            L"Byte size calculation not supported for sparse arrays.");
    }
    return elementSize() * elementCount();
}
//=============================================================================
template <class T>
bool
isTPositive(const void* data, indexType len)
{
    const T* qp = (const T*)data;
    for (indexType i = 0; i < len; ++i) {
        if (qp[i] < 0) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
ArrayOf::isPositive() const
{
    if (hasScalarStorage()) {
        if (scalarType_ == NLS_LOGICAL) {
            return false;
        }
        if (scalarType_ == NLS_SINGLE) {
            return !(*static_cast<const single*>(dataPointer()) < 0);
        }
        return !(*static_cast<const double*>(dataPointer()) < 0);
    }
    if (isSparse()) {
        switch (dp->dataClass) {
        case NLS_DCOMPLEX:
            return false;
        case NLS_DOUBLE: {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)dp->data();
            return isTPositive<double>(spMat->valuePtr(), spMat->nonZeros());
        } break;
        default:
            return false;
        }
    }

    switch (dp->dataClass) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
        return true;
    case NLS_SINGLE:
        return isTPositive<single>(dp->data(), elementCount());
    case NLS_DOUBLE:
        return isTPositive<double>(dp->data(), elementCount());
    case NLS_INT8:
        return isTPositive<int8>(dp->data(), elementCount());
    case NLS_INT16:
        return isTPositive<int16>(dp->data(), elementCount());
    case NLS_INT32:
        return isTPositive<int32>(dp->data(), elementCount());
    case NLS_INT64:
        return isTPositive<int64>(dp->data(), elementCount());
    case NLS_UNKNOWN:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRING_ARRAY:
    case NLS_LOGICAL:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
    default: {
        return false;
    } break;
    }
    return false;
}
//=============================================================================
namespace {
    // The characters of a char row or of a scalar string.
    static const ArrayOf&
    switchTextOf(const ArrayOf& value)
    {
        if (value.isScalarStringArray()) {
            return static_cast<const ArrayOf*>(value.dataPointer())[0];
        }
        return value;
    }
    //=============================================================================
    // One number of any real or complex numeric, logical or char class, kept exact.
    struct SwitchNumber
    {
        bool isInteger = false;
        bool isNegative = false;
        uint64 magnitude = 0;
        double real = 0.;
        double imag = 0.;
    };
    //=============================================================================
    static bool
    switchNumberOf(const ArrayOf& value, SwitchNumber& number)
    {
        const void* data = value.dataPointer();
        auto setSigned = [&number](int64 v) {
            number.isInteger = true;
            number.isNegative = v < 0;
            number.magnitude = v < 0 ? static_cast<uint64>(-(v + 1)) + 1 : static_cast<uint64>(v);
        };
        auto setUnsigned = [&number](uint64 v) {
            number.isInteger = true;
            number.magnitude = v;
        };
        switch (value.dataClass()) {
        case NLS_LOGICAL:
            setUnsigned(*static_cast<const logical*>(data));
            return true;
        case NLS_CHAR:
            setUnsigned(static_cast<uint64>(*static_cast<const charType*>(data)));
            return true;
        case NLS_UINT8:
            setUnsigned(*static_cast<const uint8*>(data));
            return true;
        case NLS_UINT16:
            setUnsigned(*static_cast<const uint16*>(data));
            return true;
        case NLS_UINT32:
            setUnsigned(*static_cast<const uint32*>(data));
            return true;
        case NLS_UINT64:
            setUnsigned(*static_cast<const uint64*>(data));
            return true;
        case NLS_INT8:
            setSigned(*static_cast<const int8*>(data));
            return true;
        case NLS_INT16:
            setSigned(*static_cast<const int16*>(data));
            return true;
        case NLS_INT32:
            setSigned(*static_cast<const int32*>(data));
            return true;
        case NLS_INT64:
            setSigned(*static_cast<const int64*>(data));
            return true;
        case NLS_SINGLE:
            number.real = *static_cast<const single*>(data);
            return true;
        case NLS_DOUBLE:
            number.real = *static_cast<const double*>(data);
            return true;
        case NLS_SCOMPLEX:
            number.real = static_cast<const single*>(data)[0];
            number.imag = static_cast<const single*>(data)[1];
            return true;
        case NLS_DCOMPLEX:
            number.real = static_cast<const double*>(data)[0];
            number.imag = static_cast<const double*>(data)[1];
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    // An integer equals a floating value only when that value is the same whole number.
    static bool
    integerEqualsFloating(const SwitchNumber& integer, const SwitchNumber& floating)
    {
        const double value = floating.real;
        if (floating.imag != 0. || !std::isfinite(value) || std::floor(value) != value
            || (value < 0.) != integer.isNegative) {
            return false;
        }
        const double magnitude = std::fabs(value);
        if (magnitude >= 18446744073709551616.) {
            return false;
        }
        return static_cast<uint64>(magnitude) == integer.magnitude;
    }
    //=============================================================================
    static bool
    switchNumbersEqual(const SwitchNumber& a, const SwitchNumber& b)
    {
        if (a.isInteger && b.isInteger) {
            return a.magnitude == b.magnitude && (a.isNegative == b.isNegative || a.magnitude == 0);
        }
        if (a.isInteger) {
            return integerEqualsFloating(a, b);
        }
        if (b.isInteger) {
            return integerEqualsFloating(b, a);
        }
        return a.real == b.real && a.imag == b.imag;
    }
} // namespace
//=============================================================================
// Text matches text character by character; one-element values compare by value, exactly
// across classes; a string compared with anything but text is an error.
bool
ArrayOf::testCaseMatchScalar(ArrayOf x) const
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"isPositive");
    }
    const bool switchIsString = isScalarStringArray();
    const bool caseIsString = x.isScalarStringArray();
    const bool switchIsText = switchIsString || isRowVectorCharacterArray();
    const bool caseIsText = caseIsString || x.isRowVectorCharacterArray();
    if ((switchIsString || caseIsString) && !(switchIsText && caseIsText)) {
        Error(L"Nelson:string:ComparisonNotDefined",
            L"Comparison between %s and %s is not supported.",
            utf8_to_wstring(ClassName(x)).c_str(), utf8_to_wstring(ClassName(*this)).c_str());
    }
    if (switchIsText && caseIsText) {
        const ArrayOf& switchText = switchTextOf(*this);
        const ArrayOf& caseText = switchTextOf(x);
        if (switchText.isEmpty() || caseText.isEmpty()) {
            return switchText.isEmpty() && caseText.isEmpty()
                && (switchIsString || caseIsString
                    || switchText.dimensions().equals(caseText.dimensions()));
        }
        bool equal = false;
        return tryCompareCharRows(switchText, caseText, true, equal) && equal;
    }
    if (!isScalar() || !x.isScalar()) {
        return false;
    }
    SwitchNumber switchNumber;
    SwitchNumber caseNumber;
    return switchNumberOf(*this, switchNumber) && switchNumberOf(x, caseNumber)
        && switchNumbersEqual(switchNumber, caseNumber);
}
//=============================================================================
bool
ArrayOf::testForCaseMatch(ArrayOf x) const
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"isPositive");
    }
    if (!(isScalar() || isCharacterArray())) {
        Error(L"Nelson:types:switchArgument", L"Switch argument must be a scalar or a string");
    }
    // A scalar string is a reference type internally but is a valid textual switch value.
    if (isReferenceType() && !isScalarStringArray()) {
        Error(L"Nelson:types:switchArgumentReferenceType",
            L"Switch argument cannot be a reference type (struct or cell array)");
    }
    // A cell or string array is a list of case alternatives, matched element by element even
    // when it holds a single element. Keep it out of the scalar fast path below.
    const bool xIsCaseList = (x.dp != nullptr)
        && (x.dp->dataClass == NLS_CELL_ARRAY || x.dp->dataClass == NLS_STRING_ARRAY);
    if (!xIsCaseList && (x.isScalar() || x.isRowVectorCharacterArray())) {
        return testCaseMatchScalar(x);
    }
    if (!xIsCaseList) {
        Error(L"Nelson:types:caseArgument",
            L"Case arguments must either be a scalar or a cell array");
    }
    // strings compare with text only
    if (x.dp->dataClass == NLS_STRING_ARRAY && !isRowVectorCharacterArray()
        && !isScalarStringArray()) {
        Error(L"Nelson:string:ComparisonNotDefined",
            L"Comparison between %s and %s is not supported.",
            utf8_to_wstring(ClassName(x)).c_str(), utf8_to_wstring(ClassName(*this)).c_str());
    }
    const ArrayOf* qp = (const ArrayOf*)x.dp->data();
    indexType len = x.elementCount();
    bool foundMatch = false;
    indexType i = 0;
    while (i < len && !foundMatch) {
        foundMatch = testCaseMatchScalar(qp[i]);
        i++;
    }
    return foundMatch;
}
//=============================================================================
bool
ArrayOf::isEmpty(bool allDimensionsIsZero) const
{
    if (hasScalarStorage()) {
        return false;
    }
    if (dp == nullptr) {
        return true;
    }
    if (allDimensionsIsZero) {
        Dimensions dims = dp->dimensions();
        return dims.isEmpty(allDimensionsIsZero);
    }
    return (dp->elementCount() == 0);
}
//=============================================================================
bool
ArrayOf::isScalar() const
{
    if (hasScalarStorage()) {
        return true;
    }
    if (dp) {
        return dp->isScalar();
    }
    return false;
}
//=============================================================================
bool
ArrayOf::is2D() const
{
    if (hasScalarStorage()) {
        return scalarDimensionCount_ <= 2;
    }
    if (dp) {
        return dp->is2D();
    }
    return false;
}
//=============================================================================
/**
 * Returns TRUE if we are 2-Dimensional and cols == rows.
 */
bool
ArrayOf::isSquare() const
{
    if (hasScalarStorage()) {
        return is2D();
    }
    if (dp) {
        return dp->dimensions_.isSquare();
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isVector() const
{
    if (hasScalarStorage()) {
        return scalarDimensionCount_ == 2;
    }
    return dp->isVector();
}
//=============================================================================
bool
ArrayOf::isRowVector() const
{
    if (hasScalarStorage()) {
        return scalarDimensionCount_ == 2;
    }
    return dp->dimensions_.isRowVector();
}
//=============================================================================
bool
ArrayOf::isColumnVector() const
{
    if (hasScalarStorage()) {
        return scalarDimensionCount_ == 2;
    }
    return dp->dimensions_.isColumnVector();
}
//=============================================================================
/**
 * Returns TRUE if we are a reference type (cell array, string array,
 * struct array, or handle).
 */
bool
ArrayOf::isReferenceType() const
{
    if (hasScalarStorage()) {
        return false;
    }
    return (dp->dataClass > NLS_CHAR);
}
//=============================================================================
bool
ArrayOf::isComplex() const
{
    if (hasScalarStorage()) {
        return false;
    }
    return (dp->dataClass == NLS_DCOMPLEX || dp->dataClass == NLS_SCOMPLEX);
}
//=============================================================================
bool
ArrayOf::isReal() const
{
    return (!isComplex());
}
//=============================================================================
bool
ArrayOf::allReal() const
{
    if (hasScalarStorage()) {
        return true;
    }
    bool res;
    switch (dp->dataClass) {
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE: {
        res = true;
    } break;
    case NLS_SCOMPLEX: {
        // no element: nothing imaginary (an empty sparse may hold no matrix)
        if (isEmpty()) {
            res = true;
        } else if (isSparse()) {
            res = sparseImaginaryPartIsZero<singlecomplex>(dp->data());
        } else {
            single* psingle = (single*)dp->data();
            singlecomplex* Bz = reinterpret_cast<singlecomplex*>(psingle);
            Eigen::Map<Eigen::MatrixXcf> mat(Bz, 1, dp->dimensions().elementCount());
            res = mat.imag().isZero(0);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (isEmpty()) {
            res = true;
        } else {
            if (isSparse()) {
                res = sparseImaginaryPartIsZero<doublecomplex>(dp->data());
            } else {
                double* pdouble = (double*)dp->data();
                doublecomplex* Bz = reinterpret_cast<doublecomplex*>(pdouble);
                Eigen::Map<Eigen::MatrixXcd> mat(Bz, 1, dp->dimensions().elementCount());
                res = mat.imag().isZero(0);
            }
        }
    } break;
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        res = false;
    }
    }
    return res;
}
//=============================================================================
bool
ArrayOf::isNumeric() const
{
    return this->dataClass() < NLS_LOGICAL;
}
//=============================================================================
template <class T>
indexType
DoCountNNZReal(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if WITH_OPENMP
    const bool parallel = NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled()
        && static_cast<uint64_t>(len)
            > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold();
#pragma omp parallel for reduction(+ : accum) if (parallel)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
        if (cp[i]) {
            accum++;
        }
    }
    return accum;
}
//=============================================================================
template <class T>
indexType
DoCountNNZComplex(const void* dp, indexType len)
{
    indexType accum = 0;
    const T* cp = static_cast<const T*>(dp);
#if WITH_OPENMP
    const bool parallel = NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled()
        && static_cast<uint64_t>(len)
            > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold();
#pragma omp parallel for reduction(+ : accum) if (parallel)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)len; i++) {
        if (cp[2 * i] || cp[2 * i + 1]) {
            accum++;
        }
    }
    return accum;
}
//=============================================================================
indexType
ArrayOf::nzmax() const
{
    if (isSparse()) {
        indexType actualNzmax = Eigen_CountNonzerosMax(
            dp->dataClass, dimensionLength(0), dimensionLength(1), dp->data());
        return std::max(actualNzmax, dp->sparseNzmax);
    }
    switch (dataClass()) {
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_CHAR:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        return numel();
    case NLS_CELL_ARRAY: {
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nzmax", L"cell");
    } break;
    case NLS_STRING_ARRAY: {
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nzmax", L"string");
    } break;
    case NLS_CLASS_ARRAY: {
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nzmax", L"class");
    } break;
    case NLS_CLASSDEF_ARRAY: {
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nzmax", L"class");
    } break;
    case NLS_STRUCT_ARRAY: {
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nzmax", L"struct");
    } break;
    default: {
        Error(L"Nelson:types:undefinedFunctionNoType",
            L"Undefined function '%s' for input arguments.", L"nzmax");
    } break;
    }
    return 0; // never here
}
//=============================================================================
void
ArrayOf::setSparseNzmax(indexType requestedNzmax)
{
    if (isSparse()) {
        ensureSingleOwner();
        dp->sparseNzmax = requestedNzmax;
    }
}
//=============================================================================
indexType
ArrayOf::nnz() const
{
    if (hasScalarStorage()) {
        if (scalarType_ == NLS_LOGICAL) {
            return *static_cast<const logical*>(dataPointer()) != 0;
        }
        if (scalarType_ == NLS_SINGLE) {
            return *static_cast<const single*>(dataPointer()) != 0;
        }
        return *static_cast<const double*>(dataPointer()) != 0;
    }
    if (isSparse()) {
        if (isEmpty()) {
            return 0;
        }
        return Eigen_CountNonzeros(
            dp->dataClass, dimensionLength(0), dimensionLength(1), dp->data());
    }
    switch (dp->dataClass) {
    case NLS_LOGICAL:
        return DoCountNNZReal<logical>(dp->data(), elementCount());
    case NLS_INT8:
        return DoCountNNZReal<int8>(dp->data(), elementCount());
    case NLS_UINT8:
        return DoCountNNZReal<uint8>(dp->data(), elementCount());
    case NLS_CHAR:
        return DoCountNNZReal<charType>(dp->data(), elementCount());
    case NLS_INT16:
        return DoCountNNZReal<int16>(dp->data(), elementCount());
    case NLS_UINT16:
        return DoCountNNZReal<uint16>(dp->data(), elementCount());
    case NLS_INT32:
        return DoCountNNZReal<int32>(dp->data(), elementCount());
    case NLS_UINT32:
        return DoCountNNZReal<uint32>(dp->data(), elementCount());
    case NLS_INT64:
        return DoCountNNZReal<int64>(dp->data(), elementCount());
    case NLS_UINT64:
        return DoCountNNZReal<uint64>(dp->data(), elementCount());
    case NLS_SINGLE:
        return DoCountNNZReal<single>(dp->data(), elementCount());
    case NLS_DOUBLE:
        return DoCountNNZReal<double>(dp->data(), elementCount());
    case NLS_SCOMPLEX:
        return DoCountNNZComplex<single>(dp->data(), elementCount());
    case NLS_DCOMPLEX:
        return DoCountNNZComplex<double>(dp->data(), elementCount());
    case NLS_CELL_ARRAY:
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nnz", L"cell");
    case NLS_STRING_ARRAY:
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nnz", L"string");
    case NLS_CLASS_ARRAY:
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nnz", L"class");
    case NLS_CLASSDEF_ARRAY:
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nnz", L"class");
    case NLS_STRUCT_ARRAY:
        Error(L"Nelson:types:undefinedFunction",
            L"Undefined function '%s' for input arguments of type '%s'.", L"nnz", L"struct");
    default:
        Error(L"Nelson:types:undefinedFunctionNoType",
            L"Undefined function '%s' for input arguments.", L"nnz");
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::numel() const
{
    Dimensions dims = dimensions();
    return dims.elementCount();
}
//=============================================================================
template <class T>
bool
sparseImaginaryPartIsZero(const void* spData)
{
    const auto* spCplxMat = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(spData);
    for (signedIndexType k = 0; k < spCplxMat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spCplxMat, k);
            it; ++it) {
            if (it.value().imag() != 0) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
