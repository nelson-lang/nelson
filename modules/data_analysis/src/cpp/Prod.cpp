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
#include "ReduceLines.hpp"
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
    return { };
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
    if (withnan) {
        reduceLinesInOrder(
            sp, dp, planes, planesize, linesize, (T)1, [](T a, T v) { return a * v; });
    } else {
        reduceLinesInOrder(sp, dp, planes, planesize, linesize, (T)1,
            [](T a, T v) { return std::isnan(v) ? a : a * v; });
    }
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
    using C = ComplexPair<T>;
    auto times = [](C a, C v) {
        T t1 = a.re * v.re - a.im * v.im;
        T t2 = a.re * v.im + a.im * v.re;
        return C { t1, t2 };
    };
    const C* csp = reinterpret_cast<const C*>(sp);
    C* cdp = reinterpret_cast<C*>(dp);
    const C one { (T)1, (T)0 };
    if (withnan) {
        reduceLinesInOrder(csp, cdp, planes, planesize, linesize, one, times);
    } else {
        reduceLinesInOrder(csp, cdp, planes, planesize, linesize, one,
            [times](C a, C v) { return (std::isnan(v.re) || std::isnan(v.im)) ? a : times(a, v); });
    }
}
//=============================================================================
ArrayOf
Prod(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan, indexType lastDim)
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
    // prod([]) is 1; with a dimension, [] follows the reduction rule
    if (A.isEmpty(true) && A.is2D() && (d == 0 || allElements)) {
        res = ArrayOf::doubleScalar(1);
    } else if (A.isScalar()) {
        res = A;
    } else {
        Dimensions dimsA = A.dimensions();
        indexType workDim;
        if (allElements) {
            workDim = 0;
        } else if (d == 0) {
            indexType l = 0;
            while (dimsA.at(l) == 1) {
                l++;
            }
            workDim = l;
        } else {
            workDim = d - 1;
        }
        indexType lastWorkDim = workDim;
        if (allElements) {
            lastWorkDim = dimsA.length() - 1;
        } else if (d != 0 && lastDim > d) {
            lastWorkDim = lastDim - 1;
        }
        ReductionLines lines = reductionLines(dimsA, workDim, lastWorkDim);
        Dimensions dimsRes = lines.dimsRes;
        indexType planecount = lines.planecount;
        indexType planesize = lines.planesize;
        indexType linesize = lines.linesize;
        bool reduceAsVector = d == 0 && A.isVector();
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
            if (strtype == L"native" && classA != NLS_LOGICAL) {
                res = NativeIntegerReduce<true>(A, dimsRes, planecount, planesize, linesize);
                break;
            }
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount(), stringVector(), true));
            IntegerReduceToDouble<true>(
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
