//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4146)
#endif
//=============================================================================
#include <limits>
#include <complex>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "IntegerOperations.hpp"
#include "DotPower.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "MatrixCheck.hpp"
#include "complex_abs.hpp"
#include "IEEEFP.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SparseOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
getScalarPowerExponent(const ArrayOf& B)
{
    if (B.isSparse()) {
        return getSparseScalarValue<T>(B);
    }
    return static_cast<const T*>(B.dataPointer())[0];
}
//=============================================================================
template <class T>
static ArrayOf
sparseScalarExponentFullPatternPower(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dims = A.dimensions();
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    T exponent = getScalarPowerExponent<T>(B);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(dims.elementCount()));
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            signedIndexType sparseRow = static_cast<signedIndexType>(row);
            signedIndexType sparseCol = static_cast<signedIndexType>(col);
            T value = std::pow(sparseA->coeff(sparseRow, sparseCol), exponent);
            if (value != T(0)) {
                triplets.emplace_back(sparseRow, sparseCol, value);
            }
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, cols);
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, dims, sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
sparseScalarExponentPower(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    T exponent = getScalarPowerExponent<T>(B);
    if (exponent == T(0)) {
        return sparseScalarExponentFullPatternPower<T>(dataClass, A, B);
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(*getSparseMatrixPointer<T>(A));
    for (signedIndexType k = 0; k < sparseResult->outerSize(); ++k) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseResult, k); it; ++it) {
            it.valueRef() = std::pow(it.value(), exponent);
        }
    }
    return makeSparseOperatorResult<T>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
template <>
ArrayOf
sparseScalarExponentPower<double>(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    double exponent = getScalarPowerExponent<double>(B);
    if (exponent <= 0) {
        return sparseScalarExponentFullPatternPower<double>(dataClass, A, B);
    }
    auto* sparseResult = new NelsonSparseMatrix<double>(*getSparseMatrixPointer<double>(A));
    for (signedIndexType k = 0; k < sparseResult->outerSize(); ++k) {
        for (NelsonSparseMatrix<double>::InnerIterator it(*sparseResult, k); it; ++it) {
            it.valueRef() = std::pow(it.value(), exponent);
        }
    }
    return makeSparseOperatorResult<double>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
template <>
ArrayOf
sparseScalarExponentPower<single>(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    single exponent = getScalarPowerExponent<single>(B);
    if (exponent <= single(0)) {
        return sparseScalarExponentFullPatternPower<single>(dataClass, A, B);
    }
    auto* sparseResult = new NelsonSparseMatrix<single>(*getSparseMatrixPointer<single>(A));
    for (signedIndexType k = 0; k < sparseResult->outerSize(); ++k) {
        for (NelsonSparseMatrix<single>::InnerIterator it(*sparseResult, k); it; ++it) {
            it.valueRef() = std::pow(it.value(), exponent);
        }
    }
    return makeSparseOperatorResult<single>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
template <>
ArrayOf
sparseScalarExponentPower<doublecomplex>(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    doublecomplex exponent = getScalarPowerExponent<doublecomplex>(B);
    if (exponent.imag() != 0 || exponent.real() <= 0) {
        return sparseScalarExponentFullPatternPower<doublecomplex>(dataClass, A, B);
    }
    auto* sparseResult
        = new NelsonSparseMatrix<doublecomplex>(*getSparseMatrixPointer<doublecomplex>(A));
    for (signedIndexType k = 0; k < sparseResult->outerSize(); ++k) {
        for (NelsonSparseMatrix<doublecomplex>::InnerIterator it(*sparseResult, k); it; ++it) {
            it.valueRef() = std::pow(it.value(), exponent);
        }
    }
    return makeSparseOperatorResult<doublecomplex>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
template <>
ArrayOf
sparseScalarExponentPower<singlecomplex>(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isScalar()) {
        needToOverload = true;
        return {};
    }
    singlecomplex exponent = getScalarPowerExponent<singlecomplex>(B);
    if (exponent.imag() != 0 || exponent.real() <= single(0)) {
        return sparseScalarExponentFullPatternPower<singlecomplex>(dataClass, A, B);
    }
    auto* sparseResult
        = new NelsonSparseMatrix<singlecomplex>(*getSparseMatrixPointer<singlecomplex>(A));
    for (signedIndexType k = 0; k < sparseResult->outerSize(); ++k) {
        for (NelsonSparseMatrix<singlecomplex>::InnerIterator it(*sparseResult, k); it; ++it) {
            it.valueRef() = std::pow(it.value(), exponent);
        }
    }
    return makeSparseOperatorResult<singlecomplex>(dataClass, A.dimensions(), sparseResult);
}
//=============================================================================
template <class T>
static ArrayOf
sparseBroadcastPower(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B);
//=============================================================================
static ArrayOf
sparseDotPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (A.dataClass() != B.dataClass()) {
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            bool isComplex = A.isComplex() || B.isComplex();
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return sparseDotPower(AA, BB, needToOverload);
        }
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        if (!B.isScalar() && A.isSparse() && B.isSparse()) {
            needToOverload = false;
            return sparseBroadcastPower<double>(NLS_DOUBLE, A, B);
        }
        return sparseScalarExponentPower<double>(NLS_DOUBLE, A, B, needToOverload);
    case NLS_SINGLE:
        if (!B.isScalar() && A.isSparse() && B.isSparse()) {
            needToOverload = false;
            return sparseBroadcastPower<single>(NLS_SINGLE, A, B);
        }
        return sparseScalarExponentPower<single>(NLS_SINGLE, A, B, needToOverload);
    case NLS_DCOMPLEX:
        if (!B.isScalar() && A.isSparse() && B.isSparse()) {
            needToOverload = false;
            return sparseBroadcastPower<doublecomplex>(NLS_DCOMPLEX, A, B);
        }
        return sparseScalarExponentPower<doublecomplex>(NLS_DCOMPLEX, A, B, needToOverload);
    case NLS_SCOMPLEX:
        if (!B.isScalar() && A.isSparse() && B.isSparse()) {
            needToOverload = false;
            return sparseBroadcastPower<singlecomplex>(NLS_SCOMPLEX, A, B);
        }
        return sparseScalarExponentPower<singlecomplex>(NLS_SCOMPLEX, A, B, needToOverload);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseBroadcastPower(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, ".^");
    indexType rows = info.dimensions.rows();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(info.dimensions.elementCount()));
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T base = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
            static_cast<signedIndexType>(indexA / A.rows()));
        T exponent = sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
            static_cast<signedIndexType>(indexB / B.rows()));
        T value = std::pow(base, exponent);
        if (value != T(0)) {
            triplets.emplace_back(static_cast<signedIndexType>(idx % rows),
                static_cast<signedIndexType>(idx / rows), value);
        }
    }
    auto* sparseResult = new NelsonSparseMatrix<T>(rows, info.dimensions.columns());
    sparseResult->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseOperatorResult<T>(dataClass, info.dimensions, sparseResult);
}
//=============================================================================
#include "DotPowerKernels.hpp"
inline ArrayOf
doPowerBroadcastAssist(
    ArrayOf A, NelsonType AClass, ArrayOf B, NelsonType BClass, NelsonType CClass, vvfun exec)
{
    A.promoteType(AClass);
    B.promoteType(BClass);
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, ".^");
    indexType Clen = info.dimensions.elementCount();
    ArrayOf C(CClass, info.dimensions, nullptr);
    char* Cp = static_cast<char*>(ArrayOf::allocateArrayOf(CClass, Clen, stringVector(), false));
    const char* ptrA = static_cast<const char*>(A.dataPointer());
    const char* ptrB = static_cast<const char*>(B.dataPointer());
    for (indexType i = 0; i < Clen; ++i) {
        indexType indexA = binaryOperatorBroadcastIndex(info, i, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, i, info.bStrides);
        exec(1, Cp + i * C.elementSize(), ptrA + indexA * A.elementSize(), 1,
            ptrB + indexB * B.elementSize(), 1);
    }
    C.setDataPointer(Cp);
    return C;
}
//=============================================================================
inline ArrayOf
doPowerAssist(
    ArrayOf A, NelsonType AClass, ArrayOf B, NelsonType BClass, NelsonType CClass, vvfun exec)
{
    ArrayOf C;
    A.promoteType(AClass);
    B.promoteType(BClass);
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (!SameSizeCheck(dimsA, dimsB) && !A.isScalar() && !B.isScalar()) {
        return doPowerBroadcastAssist(A, AClass, B, BClass, CClass, exec);
    }
    if (A.isScalar()) {
        indexType Blen(B.elementCount());
        C = ArrayOf(CClass, B.dimensions(), nullptr);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Blen, stringVector(), false);
        exec(Blen, Cp, A.dataPointer(), 0, B.dataPointer(), 1);
        C.setDataPointer(Cp);
    } else if (B.isScalar()) {
        indexType Alen(A.elementCount());
        C = ArrayOf(CClass, A.dimensions(), nullptr);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Alen, stringVector(), false);
        exec(Alen, Cp, A.dataPointer(), 1, B.dataPointer(), 0);
        C.setDataPointer(Cp);
    } else {
        indexType Alen(A.elementCount());
        C = ArrayOf(CClass, A.dimensions(), nullptr);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Alen, stringVector(), false);
        exec(Alen, Cp, A.dataPointer(), 1, B.dataPointer(), 1);
        C.setDataPointer(Cp);
    }
    return C;
}
//=============================================================================
#define OPCASE(t, o)                                                                               \
    case t:                                                                                        \
        opType = o;                                                                                \
        break;
#define MAPOP(o, a, b, c, f)                                                                       \
    case o:                                                                                        \
        return doPowerAssist(A, a, B, b, c, f);
//=============================================================================
ArrayOf
DoPowerTwoArgFunction(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf C;
    bool Anegative;
    NelsonType AClass;
    NelsonType BClass;
    int opType;

    if (A.isEmpty() || B.isEmpty()) {
        BinaryBroadcast info = binaryOperatorBroadcast(A, B, ".^");
        return ArrayOf::empty(info.dimensions);
    }
    CheckNumeric(A, B, "^");
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    bool sameSizeOrScalar = SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar();
    if (!sameSizeOrScalar) {
        binaryOperatorBroadcast(A, B, ".^");
    }
    AClass = A.dataClass();
    BClass = B.dataClass();
    if ((A.isIntegerType() || A.isNdArrayIntegerType())
        && (B.isIntegerType() || B.isNdArrayIntegerType())) {
        if (!B.isPositive()) {
            Error(L"Nelson:operators:invalidBitInput:onlyPositiveIntegersExpected",
                L"Only positive integers expected.");
        }
        if (!sameSizeOrScalar) {
            switch (AClass) {
            case NLS_INT8:
                return binaryOperatorBroadcastNumeric<int8>(
                    A, B, AClass, ".^", [](int8 a, int8 b) { return powi<int8>(a, b); });
            case NLS_UINT8:
                return binaryOperatorBroadcastNumeric<uint8>(
                    A, B, AClass, ".^", [](uint8 a, uint8 b) { return powi<uint8>(a, b); });
            case NLS_INT16:
                return binaryOperatorBroadcastNumeric<int16>(
                    A, B, AClass, ".^", [](int16 a, int16 b) { return powi<int16>(a, b); });
            case NLS_UINT16:
                return binaryOperatorBroadcastNumeric<uint16>(
                    A, B, AClass, ".^", [](uint16 a, uint16 b) { return powi<uint16>(a, b); });
            case NLS_INT32:
                return binaryOperatorBroadcastNumeric<int32>(
                    A, B, AClass, ".^", [](int32 a, int32 b) { return powi<int32>(a, b); });
            case NLS_UINT32:
                return binaryOperatorBroadcastNumeric<uint32>(
                    A, B, AClass, ".^", [](uint32 a, uint32 b) { return powi<uint32>(a, b); });
            case NLS_INT64:
                return binaryOperatorBroadcastNumeric<int64>(
                    A, B, AClass, ".^", [](int64 a, int64 b) { return powi<int64>(a, b); });
            case NLS_UINT64:
                return binaryOperatorBroadcastNumeric<uint64>(
                    A, B, AClass, ".^", [](uint64 a, uint64 b) { return powi<uint64>(a, b); });
            default:
                break;
            }
        }
        void* Cp = nullptr;
        indexType stride1 = 0;
        indexType stride2 = 0;
        indexType n = 0;
        if (A.isScalar()) {
            indexType Blen(B.elementCount());
            C = ArrayOf(AClass, B.dimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Blen * C.elementSize(), stringVector(), false);
            stride1 = 0;
            stride2 = 1;
            n = Blen;
        } else if (B.isScalar()) {
            indexType Alen(A.elementCount());
            C = ArrayOf(AClass, A.dimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Alen * C.elementSize(), stringVector(), false);
            stride1 = 1;
            stride2 = 0;
            n = Alen;
        } else {
            indexType Alen(A.elementCount());
            C = ArrayOf(AClass, A.dimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Alen * C.elementSize(), stringVector(), false);
            stride1 = 1;
            stride2 = 1;
            n = Alen;
        }
        indexType m = 0;
        indexType p = 0;
        switch (AClass) {
        case NLS_INT8: {
            powerInteger<int8>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT8: {
            powerInteger<uint8>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT16: {
            powerInteger<int16>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT16: {
            powerInteger<uint16>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT32: {
            powerInteger<int32>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT32: {
            powerInteger<uint32>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT64: {
            powerInteger<int64>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT64: {
            powerInteger<uint64>(A, B, n, stride1, stride2, Cp);
        } break;
        default: {
            // never here
        } break;
        }
        C.setDataPointer(Cp);
        return C;
    }
    if (AClass != BClass) {
        if (AClass < NLS_SCOMPLEX) {
            AClass = NLS_DCOMPLEX;
        }
    }

    Anegative = !(A.isPositive());
    opType = 0;
    if (AClass == NLS_SCOMPLEX) {
        switch (BClass) {
        default: {
            Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        } break;
            OPCASE(NLS_INT64, 1);
            OPCASE(NLS_SINGLE, 2);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DCOMPLEX) {
        switch (BClass) {
        default: {
            Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        } break;
            OPCASE(NLS_INT64, 6);
            OPCASE(NLS_SINGLE, 3);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && Anegative) {
        switch (BClass) {
        default: {
            Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        } break;
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 5);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && (!Anegative)) {
        switch (BClass) {
        default: {
            Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        } break;
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 8);
            OPCASE(NLS_DOUBLE, 8);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && Anegative) {
        switch (BClass) {
        default: {
            Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        } break;
            OPCASE(NLS_INT64, 9);
            OPCASE(NLS_SINGLE, 4);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && (!Anegative)) {
        switch (BClass) {
        default: {
            Error("Nelson:operators:typeNotManaged", "Type not supported.");
        } break;
            OPCASE(NLS_INT64, 9);
            OPCASE(NLS_SINGLE, 10);
            OPCASE(NLS_DOUBLE, 8);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    }
    switch (opType) {
    default:
        Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        MAPOP(1, NLS_SCOMPLEX, NLS_INT64, NLS_SCOMPLEX, (vvfun)cicpower);
        MAPOP(2, NLS_SCOMPLEX, NLS_SINGLE, NLS_SCOMPLEX, (vvfun)cfcpower);
        MAPOP(3, NLS_DCOMPLEX, NLS_DOUBLE, NLS_DCOMPLEX, (vvfun)zdzpower);
        MAPOP(4, NLS_SCOMPLEX, NLS_SCOMPLEX, NLS_SCOMPLEX, (vvfun)cccpower);
        MAPOP(5, NLS_DCOMPLEX, NLS_DCOMPLEX, NLS_DCOMPLEX, (vvfun)zzzpower);
        MAPOP(6, NLS_DCOMPLEX, NLS_INT64, NLS_DCOMPLEX, (vvfun)zizpower);
        MAPOP(7, NLS_DOUBLE, NLS_INT64, NLS_DOUBLE, (vvfun)didpower);
        MAPOP(8, NLS_DOUBLE, NLS_DOUBLE, NLS_DOUBLE, (vvfun)dddpower);
        MAPOP(9, NLS_SINGLE, NLS_INT64, NLS_SINGLE, (vvfun)fifpower);
        MAPOP(10, NLS_SINGLE, NLS_SINGLE, NLS_SINGLE, (vvfun)fffpower);
    }
    return {};
}
//=============================================================================
ArrayOf
DotPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse() || B.isSparse()) {
        return sparseDotPower(A, B, needToOverload);
    }

    if (A.dataClass() != B.dataClass()) {
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            bool isComplex = A.isComplex() || B.isComplex();
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return DotPower(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(B);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (B.dataClass() == NLS_CHAR) {
                ArrayOf BB = B;
                BB.promoteType(A.dataClass());
                return DotPower(A, BB, needToOverload);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = DotPower(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.dataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(A);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (A.dataClass() == NLS_CHAR) {
                ArrayOf AA = A;
                AA.promoteType(B.dataClass());
                return DotPower(AA, B, needToOverload);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = DotPower(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }

    switch (A.dataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        return DoPowerTwoArgFunction(A, B);
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    } break;
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    } break;
    case NLS_CHAR: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        res.promoteType(NLS_DOUBLE);
        return res;
    } break;
    case NLS_LOGICAL:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
