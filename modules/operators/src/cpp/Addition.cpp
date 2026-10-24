//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <complex>
#include <vector>
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <mkl_vml.h>
#endif
#include "Addition.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringArrayAddition.hpp"
#include "AdditionReal.hpp"
#include "AdditionComplex.hpp"
#include "AdditionInteger.hpp"
#include "IntegerDoubleArithmetic.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
#include "Decomplexify.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
tryIntegerDoubleAddition(const ArrayOf& A, const ArrayOf& B, ArrayOf& res);
//=============================================================================
template <class T>
static T
sparseAdditionValueAtIndex(
    const ArrayOf& A, const NelsonSparseMatrix<T>* sparseA, const T* denseA, indexType linearIndex)
{
    if (A.isSparse()) {
        indexType row = linearIndex % A.rows();
        indexType col = linearIndex / A.rows();
        return sparseA->coeff(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
    }
    return denseA[linearIndex];
}
//=============================================================================
template <class T>
static T
sparseAdditionValue(const ArrayOf& A, const NelsonSparseMatrix<T>* sparseA, const T* denseA,
    indexType row, indexType col, indexType linearIndex)
{
    if (A.isSparse()) {
        signedIndexType sparseRow = A.isScalar() ? 0 : static_cast<signedIndexType>(row);
        signedIndexType sparseCol = A.isScalar() ? 0 : static_cast<signedIndexType>(col);
        return sparseA->coeff(sparseRow, sparseCol);
    }
    return denseA[A.isScalar() ? 0 : linearIndex];
}
//=============================================================================
template <class T>
static ArrayOf
sparseScalarExpansionAddition(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, const Dimensions& dimensions)
{
    indexType rows = dimensions.rows();
    indexType cols = dimensions.columns();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(dimensions.elementCount()));
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            indexType idx = row + col * rows;
            T value = sparseAdditionValue<T>(A, sparseA, nullptr, row, col, idx)
                + sparseAdditionValue<T>(B, sparseB, nullptr, row, col, idx);
            if (value != T(0)) {
                triplets.emplace_back(
                    static_cast<signedIndexType>(row), static_cast<signedIndexType>(col), value);
            }
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, cols);
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, dimensions, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
denseSparseAddition(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "+");
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, info.dimensions.elementCount()));
    const auto* sparseA = A.isSparse() ? getSparseMatrixPointer<T>(A) : nullptr;
    const auto* sparseB = B.isSparse() ? getSparseMatrixPointer<T>(B) : nullptr;
    const T* denseA = A.isSparse() ? nullptr : static_cast<const T*>(A.dataPointer());
    const T* denseB = B.isSparse() ? nullptr : static_cast<const T*>(B.dataPointer());
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T a = sparseAdditionValueAtIndex<T>(A, sparseA, denseA, indexA);
        T b = sparseAdditionValueAtIndex<T>(B, sparseB, denseB, indexB);
        ptrC[idx] = a + b;
    }
    return ArrayOf(dataClass, info.dimensions, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
sparseSparseBroadcastAddition(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, "+");
    indexType rows = info.dimensions.rows();
    indexType cols = info.dimensions.columns();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(info.dimensions.elementCount()));
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T value = sparseAdditionValueAtIndex<T>(A, sparseA, nullptr, indexA)
            + sparseAdditionValueAtIndex<T>(B, sparseB, nullptr, indexB);
        if (value != T(0)) {
            triplets.emplace_back(static_cast<signedIndexType>(idx % rows),
                static_cast<signedIndexType>(idx / rows), value);
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, cols);
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, info.dimensions, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
sparseAddition(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isSparse() || !B.isSparse()) {
        return denseSparseAddition<T>(dataClass, A, B);
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        return sameSizeSparseOperatorAddition<T>(dataClass, dimsA, A, B);
    }
    if (A.isScalar()) {
        T scalarValue = getSparseScalarValue<T>(A);
        if (scalarValue == T(0)) {
            return copySparseOperatorResult<T>(dataClass, dimsB, B);
        }
        return sparseScalarExpansionAddition<T>(dataClass, A, B, dimsB);
    }
    if (B.isScalar()) {
        T scalarValue = getSparseScalarValue<T>(B);
        if (scalarValue == T(0)) {
            return copySparseOperatorResult<T>(dataClass, dimsA, A);
        }
        return sparseScalarExpansionAddition<T>(dataClass, A, B, dimsA);
    }
    return sparseSparseBroadcastAddition<T>(dataClass, A, B);
}
//=============================================================================
static ArrayOf
sparseAddition(const ArrayOf& A, const ArrayOf& B)
{
    if ((!A.isSparse() && !B.isSparse()) || A.dataClass() != B.dataClass()) {
        Error("Nelson:operators:typeMismatch", "Same types expected.");
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return sparseAddition<double>(NLS_DOUBLE, A, B);
    case NLS_SINGLE:
        return sparseAddition<single>(NLS_SINGLE, A, B);
    case NLS_DCOMPLEX:
        return sparseAddition<doublecomplex>(NLS_DCOMPLEX, A, B);
    case NLS_SCOMPLEX:
        return sparseAddition<singlecomplex>(NLS_SCOMPLEX, A, B);
    default:
        Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
    }
    return {};
}
//=============================================================================
static bool
isPatternObject(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == "pattern" && value.isField("Expression");
}
//=============================================================================
static std::wstring
escapeRegex(const std::wstring& input)
{
    const std::wstring special = L"\\.^$|()[]{}*+?";
    std::wstring out;
    for (wchar_t c : input) {
        if (special.find(c) != std::wstring::npos) {
            out.push_back(L'\\');
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
static std::wstring
patternRegexOf(const ArrayOf& value)
{
    if (isPatternObject(value)) {
        return value.field("Expression").asWideString();
    }
    if (value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar())) {
        return escapeRegex(value.asWideString());
    }
    Error(L"Nelson:operators:invalidPatternType",
        L"Pattern, string scalar, or character vector expected.");
    return L"";
}
//=============================================================================
static ArrayOf
patternAddition(const ArrayOf& A, const ArrayOf& B)
{
    std::wstring expression = L"(?:" + patternRegexOf(A) + L")(?:" + patternRegexOf(B) + L")";
    ArrayOfVector values;
    values.push_back(ArrayOf::characterArray(expression));
    values.push_back(ArrayOf::characterArray(L""));
    return ArrayOf::classdefArray("pattern", { "Expression", "DisplayName" }, values);
}
//=============================================================================
ArrayOf
Addition(const ArrayOf& A, const ArrayOf& B)
{
    if (isPatternObject(A) || isPatternObject(B)) {
        return patternAddition(A, B);
    }
    if (A.isSparse() || B.isSparse()) {
        // A sparse sum drops an all-zero imaginary part, including the dense
        // and empty result paths that do not pass through makeSparseOperatorResult.
        return decomplexify(sparseAddition(A, B));
    }
    if (A.dataClass() != B.dataClass()) {
        ArrayOf mixed;
        if (tryIntegerDoubleAddition(A, B, mixed)) {
            return mixed;
        }
        Error("Nelson:operators:typeMismatch", "Same types expected.");
    }
    NelsonType commonClass = A.dataClass();
    ArrayOf res;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = B;
            } else {
                res = A;
            }
        } else {
            res = binaryOperatorEmptyMatrixEmptyMatrix(A, B, commonClass, "+");
        }
        if (res.dataClass() == NLS_DCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
        if (res.dataClass() == NLS_SCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
        return res;
    }
    switch (commonClass) {
    case NLS_INT8:
        return integer_addition<int8>(NLS_INT8, A, B);
    case NLS_UINT8:
        return integer_addition<uint8>(NLS_UINT8, A, B);
    case NLS_INT16:
        return integer_addition<int16>(NLS_INT16, A, B);
    case NLS_UINT16:
        return integer_addition<uint16>(NLS_UINT16, A, B);
    case NLS_INT32:
        return integer_addition<int32>(NLS_INT32, A, B);
    case NLS_UINT32:
        return integer_addition<uint32>(NLS_UINT32, A, B);
    case NLS_INT64:
        return integer_addition<int64>(NLS_INT64, A, B);
    case NLS_UINT64:
        return integer_addition<uint64>(NLS_UINT64, A, B);
    case NLS_STRING_ARRAY: {
        return stringArray_plus_stringArray(A, B);
    } break;
    case NLS_SINGLE: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_real_addition<single>(NLS_SINGLE, A, B);
        } else {
            Dimensions dimsA = A.dimensions();
            Dimensions dimsB = B.dimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
#if defined(_NLS_WITH_VML)
                Dimensions dimsC = A.dimensions();
                indexType Clen = dimsC.elementCount();
                single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, Clen);
                const single* ptrA = (const single*)A.dataPointer();
                const single* ptrB = (const single*)B.dataPointer();
                res = ArrayOf(NLS_SINGLE, dimsC, ptrC, false);
                vsAdd((MKL_INT)Clen, ptrA, ptrB, ptrC);
#else
                res = matrix_matrix_real_addition<single>(NLS_SINGLE, A, B);
#endif
            } else {
                res = real_addition<single>(NLS_SINGLE, A, B);
            }
        }
    } break;
    case NLS_DOUBLE: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_real_addition<double>(NLS_DOUBLE, A, B);
        } else {
            Dimensions dimsA = A.dimensions();
            Dimensions dimsB = B.dimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
#if defined(_NLS_WITH_VML)
                Dimensions dimsC = A.dimensions();
                indexType Clen = dimsC.elementCount();
                double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, Clen);
                const double* ptrA = (const double*)A.dataPointer();
                const double* ptrB = (const double*)B.dataPointer();
                res = ArrayOf(NLS_DOUBLE, dimsC, ptrC, false);
                vdAdd((MKL_INT)Clen, ptrA, ptrB, ptrC);
#else
                res = matrix_matrix_real_addition<double>(NLS_DOUBLE, A, B);
#endif
            } else {
                res = real_addition<double>(NLS_DOUBLE, A, B);
            }
        }
    } break;
    case NLS_SCOMPLEX: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_complex_addition<single>(NLS_SCOMPLEX, A, B);
        } else {
            Dimensions dimsA = A.dimensions();
            Dimensions dimsB = B.dimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
#if defined(_NLS_WITH_VML)
                Dimensions dimsC = A.dimensions();
                indexType Clen = dimsC.elementCount();
                single* ptrC = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, Clen);
                res = ArrayOf(NLS_SCOMPLEX, dimsC, ptrC, false);
                single* ptrA = (single*)A.dataPointer();
                single* ptrB = (single*)B.dataPointer();
                MKL_Complex8* ptrAz = reinterpret_cast<MKL_Complex8*>(ptrA);
                MKL_Complex8* ptrBz = reinterpret_cast<MKL_Complex8*>(ptrB);
                MKL_Complex8* ptrCz = reinterpret_cast<MKL_Complex8*>(ptrC);
                vcAdd((MKL_INT)Clen, ptrAz, ptrBz, ptrCz);
#else
                res = matrix_matrix_complex_addition<single>(NLS_SCOMPLEX, A, B);
#endif
            } else {
                res = complex_addition<single>(NLS_SCOMPLEX, A, B);
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (A.isScalar() && B.isScalar()) {
            res = scalar_scalar_complex_addition<double>(NLS_DCOMPLEX, A, B);
        } else {
            Dimensions dimsA = A.dimensions();
            Dimensions dimsB = B.dimensions();
            if (SameSizeCheck(dimsA, dimsB)) {
#if defined(_NLS_WITH_VML)
                Dimensions dimsC = A.dimensions();
                indexType Clen = dimsC.elementCount();
                double* ptrC = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, Clen);
                res = ArrayOf(NLS_DCOMPLEX, dimsC, ptrC, false);
                double* ptrA = (double*)A.dataPointer();
                double* ptrB = (double*)B.dataPointer();
                MKL_Complex16* ptrAz = reinterpret_cast<MKL_Complex16*>(ptrA);
                MKL_Complex16* ptrBz = reinterpret_cast<MKL_Complex16*>(ptrB);
                MKL_Complex16* ptrCz = reinterpret_cast<MKL_Complex16*>(ptrC);
                vzAdd((MKL_INT)Clen, ptrAz, ptrBz, ptrCz);
#else
                res = matrix_matrix_complex_addition<double>(NLS_DCOMPLEX, A, B);
#endif
            } else {
                res = complex_addition<double>(NLS_DCOMPLEX, A, B);
            }
        }
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default: {
        Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
    } break;
    }
    return res;
}
//=============================================================================
static bool
tryIntegerDoubleAddition(const ArrayOf& A, const ArrayOf& B, ArrayOf& res)
{
    bool integerIsLeft = false;
    if (!isIntegerDoubleScalarPair(A, B, integerIsLeft)) {
        return false;
    }
    const ArrayOf& I = integerIsLeft ? A : B;
    const ArrayOf& D = integerIsLeft ? B : A;
    const IntegerDoubleOperation op = IntegerDoubleOperation::Add;
    switch (I.dataClass()) {
    case NLS_INT8:
        res = integerDoubleArithmetic<int8>(
            NLS_INT8, I, D, integerIsLeft, op, integer_addition<int8>);
        break;
    case NLS_UINT8:
        res = integerDoubleArithmetic<uint8>(
            NLS_UINT8, I, D, integerIsLeft, op, integer_addition<uint8>);
        break;
    case NLS_INT16:
        res = integerDoubleArithmetic<int16>(
            NLS_INT16, I, D, integerIsLeft, op, integer_addition<int16>);
        break;
    case NLS_UINT16:
        res = integerDoubleArithmetic<uint16>(
            NLS_UINT16, I, D, integerIsLeft, op, integer_addition<uint16>);
        break;
    case NLS_INT32:
        res = integerDoubleArithmetic<int32>(
            NLS_INT32, I, D, integerIsLeft, op, integer_addition<int32>);
        break;
    case NLS_UINT32:
        res = integerDoubleArithmetic<uint32>(
            NLS_UINT32, I, D, integerIsLeft, op, integer_addition<uint32>);
        break;
    case NLS_INT64:
        res = integerDoubleArithmetic<int64>(
            NLS_INT64, I, D, integerIsLeft, op, integer_addition<int64>);
        break;
    case NLS_UINT64:
        res = integerDoubleArithmetic<uint64>(
            NLS_UINT64, I, D, integerIsLeft, op, integer_addition<uint64>);
        break;
    default:
        return false;
    }
    return true;
}
//=============================================================================
template <class T>
static ArrayOf
complexRealAddKernel(NelsonType classDestination, const ArrayOf& complexArray,
    const ArrayOf& realArray, bool complexIsMatrix, bool realIsMatrix)
{
    Dimensions dimsC = complexIsMatrix ? complexArray.dimensions() : realArray.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    const std::complex<T>* Az
        = reinterpret_cast<const std::complex<T>*>(complexArray.dataPointer());
    const T* Br = reinterpret_cast<const T*>(realArray.dataPointer());
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        const std::complex<T>& a = complexIsMatrix ? Az[k] : Az[0];
        T c = realIsMatrix ? Br[k] : Br[0];
        Cz[k] = std::complex<T>(a.real() + c, a.imag());
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static bool
complex_real_add_scaling(NelsonType classDestination, const ArrayOf& complexArray,
    const ArrayOf& realArray, ArrayOf& result)
{
    bool matrixMatrix = complexArray.dimensions().equals(realArray.dimensions());
    bool matrixScalar = !matrixMatrix && realArray.isScalar();
    bool scalarMatrix = !matrixMatrix && !matrixScalar && complexArray.isScalar();
    if (!matrixMatrix && !matrixScalar && !scalarMatrix) {
        return false;
    }
    bool complexIsMatrix = matrixMatrix || matrixScalar;
    bool realIsMatrix = matrixMatrix || scalarMatrix;
    result = complexRealAddKernel<T>(
        classDestination, complexArray, realArray, complexIsMatrix, realIsMatrix);
    return true;
}
//=============================================================================
bool
dotPlusComplexRealScaling(const ArrayOf& A, const ArrayOf& B, ArrayOf& result)
{
    if (A.isSparse() || B.isSparse() || A.isEmpty() || B.isEmpty()) {
        return false;
    }
    NelsonType classA = A.dataClass();
    NelsonType classB = B.dataClass();
    const ArrayOf* complexArray = nullptr;
    const ArrayOf* realArray = nullptr;
    NelsonType classDestination = NLS_DCOMPLEX;
    if (classA == NLS_DCOMPLEX && classB == NLS_DOUBLE) {
        complexArray = &A;
        realArray = &B;
        classDestination = NLS_DCOMPLEX;
    } else if (classA == NLS_DOUBLE && classB == NLS_DCOMPLEX) {
        complexArray = &B;
        realArray = &A;
        classDestination = NLS_DCOMPLEX;
    } else if (classA == NLS_SCOMPLEX && classB == NLS_SINGLE) {
        complexArray = &A;
        realArray = &B;
        classDestination = NLS_SCOMPLEX;
    } else if (classA == NLS_SINGLE && classB == NLS_SCOMPLEX) {
        complexArray = &B;
        realArray = &A;
        classDestination = NLS_SCOMPLEX;
    } else {
        return false;
    }
    bool handled = false;
    if (classDestination == NLS_DCOMPLEX) {
        handled = complex_real_add_scaling<double>(NLS_DCOMPLEX, *complexArray, *realArray, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_DOUBLE);
        }
    } else {
        handled = complex_real_add_scaling<single>(NLS_SCOMPLEX, *complexArray, *realArray, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_SINGLE);
        }
    }
    return handled;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
