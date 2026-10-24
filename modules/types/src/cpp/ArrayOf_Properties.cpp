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
#define caseMacroReal(caseLabel, type)                                                             \
    case caseLabel:                                                                                \
        retval = (*((const type*)x_dp) == *((const type*)y_dp));                                   \
        break;
//=============================================================================
#define caseMacroComplex(caseLabel, type)                                                          \
    case caseLabel:                                                                                \
        retval = (((const type*)x_dp)[0] == ((const type*)y_dp)[0])                                \
            && (((const type*)x_dp)[1] == ((const type*)y_dp)[1]);                                 \
        break;
//=============================================================================
bool
ArrayOf::testCaseMatchScalar(ArrayOf x) const
{
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported", L"%s not supported for sparse arrays.",
            L"isPositive");
    }
    // Character row vectors and scalar strings are interchangeable for switch/case matching.
    if (isRowVectorCharacterArray() || isScalarStringArray()) {
        if (!x.isRowVectorCharacterArray() && !x.isScalarStringArray()) {
            return false;
        }
        std::wstring s1 = asWideString();
        std::wstring s2 = x.asWideString();
        bool retval = (s1.compare(s2) == 0);
        return retval;
    }
    if (!x.isScalar()) {
        return false;
    }
    // Not a string: compare numerically after promoting both to the same type.
    ArrayOf y = *this;
    if (x.dataClass() > y.dataClass()) {
        y.promoteType(x.dataClass());
    } else {
        x.promoteType(y.dataClass());
    }
    const void* x_dp = x.dataPointer();
    const void* y_dp = y.dataPointer();
    bool retval = false;
    switch (x.dataClass()) {
    default: {
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CHAR:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
        retval = false;
        break;
        caseMacroReal(NLS_LOGICAL, logical);
        caseMacroReal(NLS_UINT8, uint8);
        caseMacroReal(NLS_INT8, int8);
        caseMacroReal(NLS_UINT16, uint16);
        caseMacroReal(NLS_INT16, int16);
        caseMacroReal(NLS_UINT32, uint32);
        caseMacroReal(NLS_INT32, int32);
        caseMacroReal(NLS_UINT64, uint64);
        caseMacroReal(NLS_INT64, int64);
        caseMacroReal(NLS_SINGLE, single);
        caseMacroReal(NLS_DOUBLE, double);
        caseMacroComplex(NLS_SCOMPLEX, single);
        caseMacroComplex(NLS_DCOMPLEX, double);
    }
    return retval;
}
#undef caseMacroReal
#undef caseMacroComplex
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
