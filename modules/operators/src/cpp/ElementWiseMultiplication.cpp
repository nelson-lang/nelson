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
#include <limits>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ElementWiseMultiplication.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "IntegerOperations.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
#include "Decomplexify.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> struct WiderIntFor
{
    using type = T; // fallback (64-bit and non-integer): the wider path is unused
};
//=============================================================================
template <> struct WiderIntFor<int8>
{
    using type = int32;
};
//=============================================================================
template <> struct WiderIntFor<int16>
{
    using type = int32;
};
//=============================================================================
template <> struct WiderIntFor<int32>
{
    using type = int64;
};
//=============================================================================
template <> struct WiderIntFor<uint8>
{
    using type = uint32;
};
//=============================================================================
template <> struct WiderIntFor<uint16>
{
    using type = uint32;
};
//=============================================================================
template <> struct WiderIntFor<uint32>
{
    using type = uint64;
};
//=============================================================================
// Saturating integer multiply. For 8/16/32-bit types the exact product fits in a
// wider integer, so it is computed exactly and clamped, which auto-vectorizes. 64-bit
// types have no portable wider integer and use the overflow-checked helper. The value
// equals scalar_scalar_integer_times for every input.
template <class T>
static inline T
saturating_integer_times(T a, T b)
{
    if constexpr (sizeof(T) <= 4) {
        using W = typename WiderIntFor<T>::type;
        W p = (W)a * (W)b;
        W lo = (W)std::numeric_limits<T>::min();
        W hi = (W)std::numeric_limits<T>::max();
        p = (p < lo) ? lo : p;
        p = (p > hi) ? hi : p;
        return (T)p;
    } else {
        return scalar_scalar_integer_times<T>(a, b);
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseSparseElementWiseMultiplicationBroadcast(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    BinaryBroadcast info = binaryOperatorBroadcast(A, B, ".*");
    indexType rows = info.dimensions.rows();
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    const auto* sparseB = getSparseMatrixPointer<T>(B);
    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    triplets.reserve(static_cast<size_t>(info.dimensions.elementCount()));
    for (indexType idx = 0; idx < info.dimensions.elementCount(); ++idx) {
        indexType indexA = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
        T value = sparseA->coeff(static_cast<signedIndexType>(indexA % A.rows()),
                      static_cast<signedIndexType>(indexA / A.rows()))
            * sparseB->coeff(static_cast<signedIndexType>(indexB % B.rows()),
                static_cast<signedIndexType>(indexB / B.rows()));
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
template <class T>
static ArrayOf
sparseElementWiseMultiplication(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        return sameSizeSparseOperatorMultiplication<T>(dataClass, dimsA, A, B);
    }
    if (A.isScalar()) {
        return scalarSparseOperatorProduct<T>(dataClass, dimsB, B, getSparseScalarValue<T>(A));
    }
    if (B.isScalar()) {
        return scalarSparseOperatorProduct<T>(dataClass, dimsA, A, getSparseScalarValue<T>(B));
    }
    return sparseSparseElementWiseMultiplicationBroadcast<T>(dataClass, A, B);
}
//=============================================================================
static ArrayOf
sparseElementWiseMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSparse() || !B.isSparse() || A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return sparseElementWiseMultiplication<logical>(NLS_LOGICAL, A, B);
    case NLS_DOUBLE:
        return sparseElementWiseMultiplication<double>(NLS_DOUBLE, A, B);
    case NLS_SINGLE:
        return sparseElementWiseMultiplication<single>(NLS_SINGLE, A, B);
    case NLS_DCOMPLEX:
        return sparseElementWiseMultiplication<doublecomplex>(NLS_DCOMPLEX, A, B);
    case NLS_SCOMPLEX:
        return sparseElementWiseMultiplication<singlecomplex>(NLS_SCOMPLEX, A, B);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
matrix_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    ArrayOf res = ArrayOf(classDestination, dimsC, Cp, false);
    // Integers use exact saturating multiplication (a floating-point accumulator
    // both truncates instead of saturating and loses precision for 64-bit
    // products). With OpenMP the multithreaded scalar loop is fastest; without
    // it, Eigen (widen, multiply, clamp, narrow) is used.
    const T* ptrA = (const T*)a.dataPointer();
    const T* ptrB = (const T*)b.dataPointer();
    T* ptrC = (T*)Cp;
#if WITH_OPENMP
    if (IS_INTEGER_TYPE(classDestination)) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            ptrC[k] = saturating_integer_times<T>(ptrA[k], ptrB[k]);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            ptrC[k] = ptrA[k] * ptrB[k];
        }
    }
#else
    if (IS_INTEGER_TYPE(classDestination)) {
        if constexpr (sizeof(T) <= 4) {
            using W = typename WiderIntFor<T>::type;
            Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> arrA(ptrA, Clen);
            Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> arrB(ptrB, Clen);
            Eigen::Map<Eigen::Array<T, Eigen::Dynamic, 1>> arrC(ptrC, Clen);
            const W lo = (W)std::numeric_limits<T>::min();
            const W hi = (W)std::numeric_limits<T>::max();
            arrC = (arrA.template cast<W>() * arrB.template cast<W>())
                       .max(lo)
                       .min(hi)
                       .template cast<T>();
        } else {
            for (indexType k = 0; k < Clen; k++) {
                ptrC[k] = scalar_scalar_integer_times<T>(ptrA[k], ptrB[k]);
            }
        }
    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
            (T*)a.dataPointer(), 1, Clen);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            (T*)b.dataPointer(), 1, Clen);
        matC = matA.cwiseProduct(matB);
    }
#endif
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
complex_matrix_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(Cz, 1, Clen);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)a.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(Az, 1, Clen);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)b.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(Bz, 1, Clen);
    matC = matA.cwiseProduct(matB);
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
scalar_matrix_elementWiseMultiplication(NelsonType classDestination, ArrayOf& a, ArrayOf& b)
{
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrA = (T*)a.dataPointer();
    const T* ptrB = (const T*)b.dataPointer();
    T* ptrC = (T*)Cp;
#if WITH_OPENMP
    if (IS_INTEGER_TYPE(classDestination)) {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            ptrC[k] = saturating_integer_times<T>(ptrA[0], ptrB[k]);
        }
    } else {
        OMP_PARALLEL_FOR_LOOP(Clen)
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            ptrC[k] = ptrA[0] * ptrB[k];
        }
    }
#else
    if (IS_INTEGER_TYPE(classDestination)) {
        if constexpr (sizeof(T) <= 4) {
            using W = typename WiderIntFor<T>::type;
            Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> arrB(ptrB, Clen);
            Eigen::Map<Eigen::Array<T, Eigen::Dynamic, 1>> arrC(ptrC, Clen);
            const W lo = (W)std::numeric_limits<T>::min();
            const W hi = (W)std::numeric_limits<T>::max();
            arrC = (((W)ptrA[0]) * arrB.template cast<W>()).max(lo).min(hi).template cast<T>();
        } else {
            for (indexType k = 0; k < Clen; k++) {
                ptrC[k] = scalar_scalar_integer_times<T>(ptrA[0], ptrB[k]);
            }
        }
    } else {
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
        Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
            (T*)b.dataPointer(), 1, Clen);
        matC = ptrA[0] * matB.array();
    }
#endif
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_scalar_matrix_elementWiseMultiplication(NelsonType classDestination, ArrayOf& a, ArrayOf& b)
{
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* da = (T*)a.dataPointer();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(da);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(Cz, 1, Clen);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)b.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(Bz, 1, Clen);
    matC = Az[0] * matB.array();
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static void
vector_elementWiseMultiplication(
    NelsonType classDestination, T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    indexType m = 0;
    if (IS_INTEGER_TYPE(classDestination)) {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                C[m] = saturating_integer_times<T>(A[i], B[j]);
                m++;
            }
        }
    } else {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                C[m] = A[i] * B[j];
                m++;
            }
        }
    }
}
//=============================================================================
template <class T>
static void
complex_vector_elementWiseMultiplication(T* C, T* A, indexType NA, T* B, indexType NB)
{
    indexType m = 0;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(A);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(B);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            Cz[m] = Az[i] * Bz[j];
            m++;
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
vector_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.dataPointer();
    const T* ptrB = (const T*)b.dataPointer();
    indexType q = 0;
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;

    if (IS_INTEGER_TYPE(classDestination)) {
        for (indexType i = 0; i < dimsC.rows(); i++) {
            for (indexType j = 0; j < dimsC.columns(); j++) {
                indexType m = i + j * a.rows();
                C[m] = saturating_integer_times<T>(ptrB[m], ptrA[q]);
            }
            q++;
        }
    } else {
        for (indexType i = 0; i < dimsC.rows(); i++) {
            for (indexType j = 0; j < dimsC.columns(); j++) {
                indexType m = i + j * a.rows();
                C[m] = ptrB[m] * ptrA[q];
            }
            q++;
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_vector_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.dimensions();
    indexType q = 0;
    indexType Clen = dimsC.elementCount();
    T* ptrA = (T*)a.dataPointer();
    T* ptrB = (T*)b.dataPointer();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* C = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * a.rows();
            Cz[m] = Bz[m] * Az[q];
        }
        q++;
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
vector_column_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.dataPointer();
    const T* ptrB = (const T*)b.dataPointer();
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    if (IS_INTEGER_TYPE(classDestination)) {
        for (indexType i = 0; i < dimsC.rows(); i++) {
            for (indexType j = 0; j < dimsC.columns(); j++) {
                indexType m = i + j * b.rows();
                C[m] = saturating_integer_times<T>(ptrB[m], ptrA[j]);
            }
        }
    } else {
        for (indexType i = 0; i < dimsC.rows(); i++) {
            for (indexType j = 0; j < dimsC.columns(); j++) {
                indexType m = i + j * b.rows();
                C[m] = ptrB[m] * ptrA[j];
            }
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_vector_column_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    indexType q = 0;
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrA = (T*)a.dataPointer();
    T* ptrB = (T*)b.dataPointer();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* C = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * b.rows();
            Cz[m] = Bz[m] * Az[j];
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
elementWiseMultiplication(NelsonType classDestination, ArrayOf a, ArrayOf b)
{
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        T* ptrA = (T*)a.dataPointer();
        T* ptrB = (T*)b.dataPointer();
        void* Cp = ArrayOf::allocateArrayOf(classDestination, 1);
        T* C = (T*)Cp;
        if (IS_INTEGER_TYPE(classDestination)) {
            C[0] = saturating_integer_times<T>(ptrA[0], ptrB[0]);
        } else {
            C[0] = ptrA[0] * ptrB[0];
        }
        Dimensions dimsC(1, 1);
        return ArrayOf(classDestination, dimsC, Cp, false);
    }
    if (a.isEmpty() || b.isEmpty()) {
        BinaryBroadcast info = binaryOperatorBroadcast(a, b, ".*");
        ArrayOf res = ArrayOf::empty(info.dimensions);
        res.promoteType(classDestination);
        return res;
    }
    Dimensions dimsA = a.dimensions();
    Dimensions dimsB = b.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (a.isScalar()) {
            T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(classDestination, 1));
            T va = ((T*)a.dataPointer())[0];
            T vb = ((T*)b.dataPointer())[0];
            if (IS_INTEGER_TYPE(classDestination)) {
                ptrC[0] = saturating_integer_times<T>(va, vb);
            } else {
                ptrC[0] = va * vb;
            }
            return ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
        }
        return matrix_matrix_elementWiseMultiplication<T>(classDestination, a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return scalar_matrix_elementWiseMultiplication<T>(classDestination, a, b);
        } // b.isScalar()
        return scalar_matrix_elementWiseMultiplication<T>(classDestination, b, a);
    }
    return binaryOperatorBroadcastNumeric<T>(
        a, b, classDestination, ".*", [classDestination](T lhs, T rhs) {
            if (IS_INTEGER_TYPE(classDestination)) {
                return saturating_integer_times<T>(lhs, rhs);
            }
            return static_cast<T>(lhs * rhs);
        });
}
//=============================================================================
template <class T>
ArrayOf
complex_elementWiseMultiplication(NelsonType classDestination, ArrayOf a, ArrayOf b)
{
    a.promoteType(classDestination);
    b.promoteType(classDestination);
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        T* ptrA = (T*)a.dataPointer();
        T* ptrB = (T*)b.dataPointer();
        std::complex<T> ca(ptrA[0], ptrA[1]);
        std::complex<T> cb(ptrB[0], ptrB[1]);
        std::complex<T> res = ca * cb;
        if (classDestination == NLS_DCOMPLEX) {
            return ArrayOf::dsingleComplexScalar((double)res.real(), (double)res.imag());
        }
        return ArrayOf::singleComplexScalar((single)res.real(), (single)res.imag());
    }
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        BinaryBroadcast info = binaryOperatorBroadcast(a, b, ".*");
        ArrayOf res = ArrayOf::empty(info.dimensions);
        res.promoteType(classDestination);
        return res;
    }
    Dimensions dimsA = a.dimensions();
    Dimensions dimsB = b.dimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        return complex_matrix_matrix_elementWiseMultiplication<T>(classDestination, a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return complex_scalar_matrix_elementWiseMultiplication<T>(classDestination, a, b);
        } // b.isScalar()
        return complex_scalar_matrix_elementWiseMultiplication<T>(classDestination, b, a);
    }
    return binaryOperatorBroadcastComplex<T>(a, b, classDestination, ".*",
        [](std::complex<T> lhs, std::complex<T> rhs) { return lhs * rhs; });
}
//=============================================================================
ArrayOf
double_elementWiseMultiplication_double(const ArrayOf& a, const ArrayOf& b)
{
    if (a.isComplex() || b.isComplex()) {
        ArrayOf res = complex_elementWiseMultiplication<double>(NLS_DCOMPLEX, a, b);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    }
    return elementWiseMultiplication<double>(NLS_DOUBLE, a, b);
}
//=============================================================================
ArrayOf
single_elementWiseMultiplication_single(const ArrayOf& a, const ArrayOf& b)
{
    if (a.isComplex() || b.isComplex()) {
        ArrayOf res = complex_elementWiseMultiplication<single>(NLS_SCOMPLEX, a, b);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    }
    return elementWiseMultiplication<single>(NLS_SINGLE, a, b);
}
//=============================================================================
ArrayOf
integer_elementWiseMultiplication_integer(const ArrayOf& a, const ArrayOf& b)
{
    switch (a.dataClass()) {
    case NLS_INT8:
        return elementWiseMultiplication<int8>(NLS_INT8, a, b);
    case NLS_UINT8:
        return elementWiseMultiplication<uint8>(NLS_UINT8, a, b);
    case NLS_INT16:
        return elementWiseMultiplication<int16>(NLS_INT16, a, b);
    case NLS_UINT16:
        return elementWiseMultiplication<uint16>(NLS_UINT16, a, b);
    case NLS_INT32:
        return elementWiseMultiplication<int32>(NLS_INT32, a, b);
    case NLS_UINT32:
        return elementWiseMultiplication<uint32>(NLS_UINT32, a, b);
    case NLS_INT64:
        return elementWiseMultiplication<int64>(NLS_INT64, a, b);
    case NLS_UINT64:
        return elementWiseMultiplication<uint64>(NLS_UINT64, a, b);
    default:
        Error(L"Nelson:operators:typeNotManaged", L"Type not supported.");
        break;
    }
    return {};
}
//=============================================================================
ArrayOf
elementWiseMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse() || B.isSparse()) {
        // an empty product holds no imaginary part: it is real
        ArrayOf res = sparseElementWiseMultiplication(A, B, needToOverload);
        return (!needToOverload && res.isEmpty()) ? decomplexify(res) : res;
    }
    if (A.dataClass() == B.dataClass()) {
        switch (A.dataClass()) {
        case NLS_INT8:
            return elementWiseMultiplication<int8>(NLS_INT8, A, B);
        case NLS_UINT8:
            return elementWiseMultiplication<uint8>(NLS_UINT8, A, B);
        case NLS_INT16:
            return elementWiseMultiplication<int16>(NLS_INT16, A, B);
        case NLS_UINT16:
            return elementWiseMultiplication<uint16>(NLS_UINT16, A, B);
        case NLS_INT32:
            return elementWiseMultiplication<int32>(NLS_INT32, A, B);
        case NLS_UINT32:
            return elementWiseMultiplication<uint32>(NLS_UINT32, A, B);
        case NLS_INT64:
            return elementWiseMultiplication<int64>(NLS_INT64, A, B);
        case NLS_UINT64:
            return elementWiseMultiplication<uint64>(NLS_UINT64, A, B);
        case NLS_LOGICAL: {
            if (A.isSparse() || B.isSparse()) {
                needToOverload = true;
                return {};
            }
            return elementWiseMultiplication<logical>(NLS_LOGICAL, A, B);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
            if (A.isSparse() || B.isSparse()) {
                needToOverload = true;
                return {};
            }
            return double_elementWiseMultiplication_double(A, B);
        case NLS_SINGLE:
        case NLS_SCOMPLEX:
            return single_elementWiseMultiplication_single(A, B);
        default: {
            needToOverload = true;
        } break;
        }
    } else {
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
            return elementWiseMultiplication(AA, BB, needToOverload);
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
                return elementWiseMultiplication(A, BB, needToOverload);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = elementWiseMultiplication(AA, B, needToOverload);
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
                return elementWiseMultiplication(AA, B, needToOverload);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = elementWiseMultiplication(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }
    return {};
}
//=============================================================================
// Fast path for complex-by-real element-wise multiplication: scaling
// (a + bi) * c = (a * c) + (b * c)i avoids promoting the real operand to a complex
// array. It is bit-identical to the generic product for finite operands, so the
// kernels below run only when every value is finite. Non-finite operands go to the
// generic path: its cross terms turn an infinite imaginary part into NaN, which
// scaling would not reproduce.
template <class T>
static bool
complexRealTimesOperandsAreClean(const std::complex<T>* complexData, indexType complexCount,
    const T* realData, indexType realCount)
{
    for (indexType k = 0; k < realCount; ++k) {
        if (!std::isfinite(realData[k])) {
            return false;
        }
    }
    for (indexType k = 0; k < complexCount; ++k) {
        if (!std::isfinite(complexData[k].real()) || !std::isfinite(complexData[k].imag())) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static ArrayOf
matrix_matrix_complex_real_times(
    NelsonType classDestination, const ArrayOf& complexArray, const ArrayOf& realArray)
{
    Dimensions dimsC = complexArray.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    const std::complex<T>* Az
        = reinterpret_cast<const std::complex<T>*>(complexArray.dataPointer());
    const T* Br = reinterpret_cast<const T*>(realArray.dataPointer());
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(Az[k].real() * Br[k], Az[k].imag() * Br[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
matrix_scalar_complex_real_times(
    NelsonType classDestination, const ArrayOf& complexArray, const ArrayOf& realArray)
{
    Dimensions dimsC = complexArray.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    const std::complex<T>* Az
        = reinterpret_cast<const std::complex<T>*>(complexArray.dataPointer());
    T c = reinterpret_cast<const T*>(realArray.dataPointer())[0];
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(Az[k].real() * c, Az[k].imag() * c);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
scalar_matrix_complex_real_times(
    NelsonType classDestination, const ArrayOf& complexArray, const ArrayOf& realArray)
{
    Dimensions dimsC = realArray.dimensions();
    indexType Clen = dimsC.elementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    std::complex<T> a = reinterpret_cast<const std::complex<T>*>(complexArray.dataPointer())[0];
    const T* Br = reinterpret_cast<const T*>(realArray.dataPointer());
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    for (indexType k = 0; k < Clen; ++k) {
        Cz[k] = std::complex<T>(a.real() * Br[k], a.imag() * Br[k]);
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static bool
complex_real_times_scaling(NelsonType classDestination, const ArrayOf& complexArray,
    const ArrayOf& realArray, ArrayOf& result)
{
    bool matrixMatrix = complexArray.dimensions().equals(realArray.dimensions());
    bool matrixScalar = !matrixMatrix && realArray.isScalar();
    bool scalarMatrix = !matrixMatrix && !matrixScalar && complexArray.isScalar();
    if (!matrixMatrix && !matrixScalar && !scalarMatrix) {
        // Broadcasting shapes (row/column, row/matrix, ...) keep the generic path.
        return false;
    }
    const std::complex<T>* Az
        = reinterpret_cast<const std::complex<T>*>(complexArray.dataPointer());
    const T* Br = reinterpret_cast<const T*>(realArray.dataPointer());
    indexType complexCount = complexArray.isScalar() ? 1 : complexArray.dimensions().elementCount();
    indexType realCount = realArray.isScalar() ? 1 : realArray.dimensions().elementCount();
    if (!complexRealTimesOperandsAreClean<T>(Az, complexCount, Br, realCount)) {
        return false;
    }
    if (matrixMatrix) {
        result = matrix_matrix_complex_real_times<T>(classDestination, complexArray, realArray);
    } else if (matrixScalar) {
        result = matrix_scalar_complex_real_times<T>(classDestination, complexArray, realArray);
    } else {
        result = scalar_matrix_complex_real_times<T>(classDestination, complexArray, realArray);
    }
    return true;
}
//=============================================================================
bool
dotTimesComplexRealScaling(const ArrayOf& A, const ArrayOf& B, ArrayOf& result)
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
        handled
            = complex_real_times_scaling<double>(NLS_DCOMPLEX, *complexArray, *realArray, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_DOUBLE);
        }
    } else {
        handled
            = complex_real_times_scaling<single>(NLS_SCOMPLEX, *complexArray, *realArray, result);
        if (handled && result.allReal()) {
            result.promoteType(NLS_SINGLE);
        }
    }
    return handled;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
