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
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Prod.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static bool
prodValueIsNaN(const T& value)
{
    return std::isnan(value);
}
//=============================================================================
template <>
bool
prodValueIsNaN<doublecomplex>(const doublecomplex& value)
{
    return std::isnan(value.real()) || std::isnan(value.imag());
}
//=============================================================================
template <class T>
static void
prodMultiply(T& accum, const T& value)
{
    accum *= value;
}
//=============================================================================
template <class T>
static ArrayOf
SparseProdTemplate(const ArrayOf& A, indexType workDim, const std::wstring& strtype, bool withnan,
    bool reduceAsVector)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    Dimensions outDims = workDim == 0 ? Dimensions(1, cols) : Dimensions(rows, 1);
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());

    if (reduceAsVector && A.isVector()) {
        T accum = T(1);
        indexType count = 0;
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                ++count;
                if (withnan || !prodValueIsNaN<T>(it.value())) {
                    prodMultiply<T>(accum, it.value());
                }
            }
        }
        auto* spmatDST = new Eigen::SparseMatrix<T, 0, signedIndexType>(1, 1);
        if (count == A.elementCount() && accum != T(0)) {
            spmatDST->coeffRef(0, 0) = accum;
        }
        indexType nz = spmatDST->nonZeros();
        ArrayOf res = makeSparseResult(A.dataClass(), Dimensions(1, 1), spmatDST);
        if (nz == 0) {
            res.setSparseNzmax(1);
        }
        return res;
    }

    if (workDim == 0) {
        auto* spmatDST = new Eigen::SparseMatrix<T, 0, signedIndexType>(1, cols);
        for (indexType col = 0; col < cols; ++col) {
            T accum = T(1);
            indexType count = 0;
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                ++count;
                if (withnan || !prodValueIsNaN<T>(it.value())) {
                    prodMultiply<T>(accum, it.value());
                }
            }
            if (count == rows && accum != T(0)) {
                spmatDST->coeffRef(0, col) = accum;
            }
        }
        indexType nz = spmatDST->nonZeros();
        ArrayOf res = makeSparseResult(A.dataClass(), outDims, spmatDST);
        if (nz == 0) {
            res.setSparseNzmax(1);
        }
        return res;
    }

    std::vector<T> accum(rows, T(1));
    std::vector<indexType> count(rows, 0);
    for (indexType col = 0; col < cols; ++col) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmatSRC, col);
            it; ++it) {
            ++count[it.row()];
            if (withnan || !prodValueIsNaN<T>(it.value())) {
                prodMultiply<T>(accum[it.row()], it.value());
            }
        }
    }
    auto* spmatDST = new Eigen::SparseMatrix<T, 0, signedIndexType>(rows, 1);
    for (indexType row = 0; row < rows; ++row) {
        if (count[row] == cols && accum[row] != T(0)) {
            spmatDST->coeffRef(row, 0) = accum[row];
        }
    }
    indexType nz = spmatDST->nonZeros();
    ArrayOf res = makeSparseResult(A.dataClass(), outDims, spmatDST);
    if (nz == 0) {
        res.setSparseNzmax(1);
    }
    return res;
}
//=============================================================================
static ArrayOf
SparseLogicalProd(
    const ArrayOf& A, indexType workDim, const std::wstring& strtype, bool reduceAsVector)
{
    ArrayOf asDouble(A);
    asDouble.promoteType(strtype == L"native" ? NLS_LOGICAL : NLS_DOUBLE);
    if (asDouble.dataClass() == NLS_LOGICAL) {
        return SparseProdTemplate<logical>(asDouble, workDim, strtype, false, reduceAsVector);
    }
    return SparseProdTemplate<double>(asDouble, workDim, strtype, false, reduceAsVector);
}
//=============================================================================
static ArrayOf
SparseProd(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    Dimensions dimsA = A.dimensions();
    indexType workDim = 0;
    if (d == 0) {
        workDim = dimsA.rows() == 1 ? 1 : 0;
    } else {
        workDim = d - 1;
    }
    if (workDim > 1) {
        Error(L"Nelson:data_analysis:sparseMatrixSupportsProdAlong",
            L"Sparse matrix supports %s along dimensions 1 or 2.", L"prod");
    }
    bool reduceAsVector = d == 0;
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return SparseLogicalProd(A, workDim, strtype, reduceAsVector);
    case NLS_DOUBLE:
        return SparseProdTemplate<double>(A, workDim, strtype, withnan, reduceAsVector);
    case NLS_DCOMPLEX:
        return SparseProdTemplate<doublecomplex>(A, workDim, strtype, withnan, reduceAsVector);
    default: {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:data_analysis:functionSUndefined",
                                    L"function %s undefined."),
                classname + L"_prod");
        Error(L"Nelson:prod:typeNotSupported", msg);
    } break;
    }
    return {};
}
//=============================================================================
template <class T>
void
RealProdT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    T accum = 1;
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)elementCount; k++) {
        T val = sp[k];
        if (!withnan) {
            if (!std::isnan(val)) {
                accum *= val;
            }
        } else {
            accum *= val;
        }
    }
    dp[0] = accum;
}
//=============================================================================
template <class T>
void
RealProdT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    ompIndexType totalElements = planes * planesize;
    OMP_PARALLEL_FOR_LOOP(totalElements)
    for (ompIndexType idx = 0; idx < totalElements; idx++) {
        indexType i = idx / planesize;
        indexType j = idx % planesize;

        T accum = 1;
        for (indexType k = 0; k < linesize; k++) {
            T val = sp[i * planesize * linesize + j + k * planesize];
            if (!withnan) {
                if (!std::isnan(val)) {
                    accum *= val;
                }
            } else {
                accum *= val;
            }
        }
        dp[i * planesize + j] = accum;
    }
}
//=============================================================================
// Integer prod accumulates the native buffer directly into a double result,
// without a promoteType(NLS_DOUBLE) copy. Integers never hold NaN.
template <class T>
static void
IntegerProdToDouble(const T* sp, double* dp, indexType elementCount)
{
    double accum = 1.0;
#if WITH_OPENMP
#pragma omp parallel for reduction(* : accum)
#endif
    for (ompIndexType k = 0; k < (ompIndexType)elementCount; k++) {
        accum *= (double)sp[k];
    }
    dp[0] = accum;
}
//=============================================================================
template <class T>
static void
IntegerProdToDouble(
    const T* sp, double* dp, indexType planes, indexType planesize, indexType linesize)
{
    ompIndexType totalElements = (ompIndexType)(planes * planesize);
    OMP_PARALLEL_FOR_LOOP(totalElements)
    for (ompIndexType idx = 0; idx < totalElements; idx++) {
        indexType i = idx / planesize;
        indexType j = idx % planesize;
        double accum = 1.0;
        for (indexType k = 0; k < linesize; k++) {
            accum *= (double)sp[i * planesize * linesize + j + k * planesize];
        }
        dp[i * planesize + j] = accum;
    }
}
//=============================================================================
static void
integerProdToDouble(const ArrayOf& A, double* ptr, bool isVector, indexType elementCount,
    indexType planecount, indexType planesize, indexType linesize)
{
    const void* p = A.dataPointer();
#define NLS_PROD_INT_CASE(NLSTYPE, CTYPE)                                                          \
    case NLSTYPE:                                                                                  \
        if (isVector) {                                                                            \
            IntegerProdToDouble<CTYPE>((const CTYPE*)p, ptr, elementCount);                        \
        } else {                                                                                   \
            IntegerProdToDouble<CTYPE>((const CTYPE*)p, ptr, planecount, planesize, linesize);     \
        }                                                                                          \
        break;
    switch (A.dataClass()) {
        NLS_PROD_INT_CASE(NLS_LOGICAL, logical)
        NLS_PROD_INT_CASE(NLS_UINT8, uint8)
        NLS_PROD_INT_CASE(NLS_INT8, int8)
        NLS_PROD_INT_CASE(NLS_UINT16, uint16)
        NLS_PROD_INT_CASE(NLS_INT16, int16)
        NLS_PROD_INT_CASE(NLS_UINT32, uint32)
        NLS_PROD_INT_CASE(NLS_INT32, int32)
        NLS_PROD_INT_CASE(NLS_UINT64, uint64)
        NLS_PROD_INT_CASE(NLS_INT64, int64)
    default:
        break;
    }
#undef NLS_PROD_INT_CASE
}
//=============================================================================
template <class T>
void
ComplexProdT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    // For complex numbers, use a sequential approach to avoid OpenMP reduction issues
    T accum_r = 1;
    T accum_i = 0;

    for (indexType k = 0; k < elementCount; k++) {
        T vr = sp[2 * k];
        T vi = sp[(2 * k) + 1];
        if (!withnan) {
            if (!std::isnan(vr) && !std::isnan(vi)) {
                T t1 = accum_r * vr - accum_i * vi;
                T t2 = accum_r * vi + accum_i * vr;
                accum_r = t1;
                accum_i = t2;
            }
        } else {
            T t1 = accum_r * vr - accum_i * vi;
            T t2 = accum_r * vi + accum_i * vr;
            accum_r = t1;
            accum_i = t2;
        }
    }
    dp[0] = accum_r;
    dp[1] = accum_i;
}
//=============================================================================
template <class T>
void
ComplexProdT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    ompIndexType totalElements = planes * planesize;
    OMP_PARALLEL_FOR_LOOP(totalElements)
    for (ompIndexType idx = 0; idx < totalElements; idx++) {
        indexType i = idx / planesize;
        indexType j = idx % planesize;

        T accum_r = 1;
        T accum_i = 0;

        for (indexType k = 0; k < linesize; k++) {
            T vr = sp[2 * (i * planesize * linesize + j + k * planesize)];
            T vi = sp[2 * (i * planesize * linesize + j + k * planesize) + 1];

            if (!withnan) {
                if (!std::isnan(vr) && !std::isnan(vi)) {
                    T t1 = accum_r * vr - accum_i * vi;
                    T t2 = accum_r * vi + accum_i * vr;
                    accum_r = t1;
                    accum_i = t2;
                }
            } else {
                T t1 = accum_r * vr - accum_i * vi;
                T t2 = accum_r * vi + accum_i * vr;
                accum_r = t1;
                accum_i = t2;
            }
        }

        dp[2 * (i * planesize + j)] = accum_r;
        dp[2 * (i * planesize + j) + 1] = accum_i;
    }
}
//=============================================================================
ArrayOf
Prod(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    ArrayOf res;
    NelsonType classA = A.dataClass();
    if (A.isSparse()) {
        return SparseProd(A, d, strtype, withnan);
    }
    if (classA == NLS_CHAR) {
        Error(L"Nelson:data_analysis:PRODWorkCharInputs", L"PROD does not work with char inputs.");
    }
    if (classA > NLS_LOGICAL) {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:data_analysis:functionSUndefined",
                                    L"function %s undefined."),
                classname + L"_prod");
        Error(L"Nelson:prod:typeNotSupported", msg);
    }
    bool allElements = d == static_cast<indexType>(-1);
    if (allElements) {
        if (!A.isScalar() && !A.isEmpty(true)) {
            Dimensions vectorDims(A.elementCount(), 1);
            A.reshape(vectorDims);
        }
        d = 1;
    }
    // prod([]) is 1; with a dimension, [] follows the reduction rule
    if (A.isEmpty(true) && A.is2D() && (d == 0 || allElements)) {
        res = ArrayOf::doubleScalar(1);
    } else if (A.isScalar()) {
        res = A;
    } else {
        Dimensions dimsA = A.dimensions();
        indexType workDim;
        if (d == 0) {
            indexType l = 0;
            while (dimsA.at(l) == 1) {
                l++;
            }
            workDim = l;
        } else {
            workDim = d - 1;
        }
        Dimensions dimsRes = dimsA;
        dimsRes.setDimensionLength(workDim, 1);
        dimsRes.simplify();
        indexType planecount;
        indexType planesize = 1;
        indexType linesize = dimsA[workDim];
        bool reduceAsVector = d == 0 && A.isVector();
        for (indexType l = 0; l < workDim; l++) {
            planesize *= dimsA[l];
        }
        planecount = 1;
        for (indexType l = workDim + 1; l < dimsA.length(); l++) {
            planecount *= dimsA[l];
        }
        switch (classA) {
        case NLS_LOGICAL:
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount(), stringVector(), true));
            integerProdToDouble(
                A, ptr, reduceAsVector, dimsA.elementCount(), planecount, planesize, linesize);
            // A negative factor times a zero factor gives -0.0, which would display as "-0".
            indexType resCount = dimsRes.elementCount();
            for (indexType m = 0; m < resCount; ++m) {
                if (ptr[m] == 0.0) {
                    ptr[m] = 0.0;
                }
            }
            // Keep the double accumulator: the output-class logic below promotes to the final
            // type. Promoting to the integer class here would saturate too early
            // (prod(int8([10 10 10])) -> 127).
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
        } break;
        case NLS_SINGLE: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (reduceAsVector) {
                RealProdT<single>(static_cast<const single*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                RealProdT<single>(static_cast<const single*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DOUBLE: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (reduceAsVector) {
                RealProdT<double>(static_cast<const double*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                RealProdT<double>(static_cast<const double*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_SCOMPLEX: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (reduceAsVector) {
                ComplexProdT<single>(static_cast<const single*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                ComplexProdT<single>(static_cast<const single*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DCOMPLEX: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (reduceAsVector) {
                ComplexProdT<double>(static_cast<const double*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                ComplexProdT<double>(static_cast<const double*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        default: {
            std::wstring classname;
            ClassName(A, classname);
            std::wstring msg = formatI18nMessage(
                resolveMessageTemplate(
                    L"Nelson:data_analysis:functionSUndefined", L"function %s undefined."),
                classname + L"_prod");
            Error(L"Nelson:prod:typeNotSupported", msg);
        } break;
        }
    }
    NelsonType outputClass = classA;
    if (strtype == L"default") {
        if (classA == NLS_DOUBLE || classA == NLS_SINGLE || classA == NLS_DCOMPLEX
            || classA == NLS_SCOMPLEX) {
            outputClass = classA;
        } else {
            outputClass = NLS_DOUBLE;
        }
    } else if (strtype == L"native") {
        outputClass = classA;
    } else if (strtype == L"double") {
        if (res.dataClass() == NLS_SCOMPLEX || res.dataClass() == NLS_DCOMPLEX) {
            outputClass = NLS_DCOMPLEX;
        } else {
            outputClass = NLS_DOUBLE;
        }
    }
    res.promoteType(outputClass);
    switch (outputClass) {
    case NLS_SCOMPLEX: {
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
