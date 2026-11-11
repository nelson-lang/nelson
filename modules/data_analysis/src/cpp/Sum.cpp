//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include <algorithm>
#include "Sum.hpp"
#include "ReduceLines.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static bool
sumValueIsNaN(const T& value)
{
    return std::isnan(value);
}
//=============================================================================
template <>
bool
sumValueIsNaN<doublecomplex>(const doublecomplex& value)
{
    return std::isnan(value.real()) || std::isnan(value.imag());
}
//=============================================================================
template <>
bool
sumValueIsNaN<singlecomplex>(const singlecomplex& value)
{
    return std::isnan(value.real()) || std::isnan(value.imag());
}
//=============================================================================
template <class T>
static ArrayOf
SparseSumTemplate(const ArrayOf& A, indexType workDim, bool withnan, bool reduceAsVector)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    Dimensions outDims = workDim == 0 ? Dimensions(1, cols) : Dimensions(rows, 1);
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());

    if (reduceAsVector && A.isVector()) {
        T accum = T(0);
        for (indexType k = 0; k < (indexType)spmatSRC->outerSize(); ++k) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, k);
                it; ++it) {
                if (withnan || !sumValueIsNaN<T>(it.value())) {
                    accum += it.value();
                }
            }
        }
        auto* spmatDST = new Eigen::SparseMatrix<T, 0, signedIndexType>(1, 1);
        if (accum != T(0)) {
            spmatDST->coeffRef(0, 0) = accum;
        }
        ArrayOf res = makeSparseResult(A.dataClass(), Dimensions(1, 1), spmatDST);
        if (accum == T(0)) {
            res.setSparseNzmax(1);
        }
        return res;
    }

    std::vector<T> accum(workDim == 0 ? cols : rows, T(0));
    if (workDim == 0) {
        for (indexType col = 0; col < cols; ++col) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                if (withnan || !sumValueIsNaN<T>(it.value())) {
                    accum[col] += it.value();
                }
            }
        }
    } else {
        for (indexType col = 0; col < cols; ++col) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(
                     *spmatSRC, col);
                it; ++it) {
                if (withnan || !sumValueIsNaN<T>(it.value())) {
                    accum[it.row()] += it.value();
                }
            }
        }
    }

    auto* spmatDST
        = new Eigen::SparseMatrix<T, 0, signedIndexType>(outDims.rows(), outDims.columns());
    for (indexType k = 0; k < (indexType)accum.size(); ++k) {
        if (accum[k] != T(0)) {
            if (workDim == 0) {
                spmatDST->coeffRef(0, k) = accum[k];
            } else {
                spmatDST->coeffRef(k, 0) = accum[k];
            }
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
SparseLogicalSum(ArrayOf A, indexType workDim, const std::wstring& strtype, bool reduceAsVector)
{
    A.promoteType(strtype == L"native" ? NLS_LOGICAL : NLS_DOUBLE);
    if (A.dataClass() == NLS_LOGICAL) {
        return SparseSumTemplate<logical>(A, workDim, true, reduceAsVector);
    }
    return SparseSumTemplate<double>(A, workDim, true, reduceAsVector);
}
//=============================================================================
static ArrayOf
SparseSum(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan)
{
    Dimensions dimsA = A.dimensions();
    indexType workDim = 0;
    bool allElements = d == static_cast<indexType>(-1);
    if (allElements) {
        workDim = 0;
    } else if (d == 0) {
        workDim = dimsA.rows() == 1 ? 1 : 0;
    } else {
        workDim = d - 1;
    }
    if (workDim > 1) {
        Error(L"Nelson:data_analysis:sparseMatrixSupportsSumAlong",
            L"Sparse matrix supports %s along dimensions 1 or 2.", L"sum");
    }
    if (allElements && !A.isVector()) {
        Dimensions vectorDims(A.elementCount(), 1);
        A.reshape(vectorDims);
    }
    bool reduceAsVector = allElements || d == 0;
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return SparseLogicalSum(A, workDim, strtype, reduceAsVector);
    case NLS_DOUBLE:
        return SparseSumTemplate<double>(A, workDim, withnan, reduceAsVector);
    case NLS_DCOMPLEX:
        return SparseSumTemplate<doublecomplex>(A, workDim, withnan, reduceAsVector);
    case NLS_SINGLE:
        return SparseSumTemplate<single>(A, workDim, withnan, reduceAsVector);
    case NLS_SCOMPLEX:
        return SparseSumTemplate<singlecomplex>(A, workDim, withnan, reduceAsVector);
    default: {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:data_analysis:functionSUndefined",
                                    L"function %s undefined."),
                classname + L"_sum");
        Error(L"Nelson:sum:typeNotSupported", msg);
    } break;
    }
    return {};
}
//=============================================================================
template <class T>
void
RealSumT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    if (dp == nullptr) {
        return;
    }
    T sum = (T)0;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sum)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        T val = sp[i];
        if (!withnan) {
            if (!std::isnan(val)) {
                sum += val;
            }
        } else {
            sum += val;
        }
    }
    dp[0] = sum;
}
//=============================================================================
template <class T>
void
RealSumT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    if (withnan) {
        reduceLinesInOrder(
            sp, dp, planes, planesize, linesize, (T)0, [](T a, T v) { return a + v; });
    } else {
        reduceLinesInOrder(sp, dp, planes, planesize, linesize, (T)0,
            [](T a, T v) { return std::isnan(v) ? a : a + v; });
    }
}
//=============================================================================
template <class T>
void
ComplexSumT(const T* sp, T* dp, indexType elementCount, bool withnan)
{
    if (dp == nullptr) {
        return;
    }
    T sum_r = (T)0;
    T sum_i = (T)0;
#if WITH_OPENMP
#pragma omp parallel for reduction(+ : sum_r, sum_i)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; i++) {
        T vr = sp[2 * i];
        T vi = sp[(2 * i) + 1];
        if (!withnan) {
            if (!std::isnan(vr) && !std::isnan(vi)) {
                sum_r += vr;
                sum_i += vi;
            }
        } else {
            sum_r += vr;
            sum_i += vi;
        }
    }
    dp[0] = sum_r;
    dp[1] = sum_i;
}
//=============================================================================
template <class T>
void
ComplexSumT(
    const T* sp, T* dp, indexType planes, indexType planesize, indexType linesize, bool withnan)
{
    using C = ComplexPair<T>;
    const C* csp = reinterpret_cast<const C*>(sp);
    C* cdp = reinterpret_cast<C*>(dp);
    if (withnan) {
        reduceLinesInOrder(csp, cdp, planes, planesize, linesize, C {},
            [](C a, C v) { return C { a.re + v.re, a.im + v.im }; });
    } else {
        reduceLinesInOrder(csp, cdp, planes, planesize, linesize, C {}, [](C a, C v) {
            return (std::isnan(v.re) || std::isnan(v.im)) ? a : C { a.re + v.re, a.im + v.im };
        });
    }
}
//=============================================================================
ArrayOf
Sum(ArrayOf A, indexType d, const std::wstring& strtype, bool withnan, indexType lastDim)
{
    ArrayOf res;
    NelsonType classA = A.dataClass();
    if (A.isSparse()) {
        return SparseSum(A, d, strtype, withnan);
    }
    if (classA == NLS_CHAR) {
        // sum('ab') sums the character codes as double
        if (strtype == L"native") {
            Error(L"Nelson:data_analysis:nativeAccumulationCharArraySupported",
                L"Native accumulation on char array is not supported.");
        }
        A.promoteType(NLS_DOUBLE);
        classA = NLS_DOUBLE;
    }
    if (classA > NLS_LOGICAL) {
        std::wstring classname;
        ClassName(A, classname);
        std::wstring msg
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:data_analysis:functionSUndefined",
                                    L"function %s undefined."),
                classname + L"_sum");
        Error(L"Nelson:sum:typeNotSupported", msg);
    }
    bool allElements = d == static_cast<indexType>(-1);
    // sum([]) is 0; with a dimension, [] follows the reduction rule
    if (A.isEmpty(true) && A.is2D() && (d == 0 || allElements)) {
        res = ArrayOf::doubleScalar(0);
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
                res = NativeIntegerReduce<false>(A, dimsRes, planecount, planesize, linesize);
                break;
            }
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, dimsRes.elementCount(), stringVector(), true));
            res = ArrayOf(NLS_DOUBLE, dimsRes, ptr);
            IntegerReduceToDouble<false>(
                A, ptr, allElements, dimsA.elementCount(), planecount, planesize, linesize);
        } break;
        case NLS_SINGLE: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (allElements) {
                RealSumT<single>(static_cast<const single*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                RealSumT<single>(static_cast<const single*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DOUBLE: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (allElements) {
                RealSumT<double>(static_cast<const double*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                RealSumT<double>(static_cast<const double*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_SCOMPLEX: {
            single* ptr = static_cast<single*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (allElements) {
                ComplexSumT<single>(static_cast<const single*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                ComplexSumT<single>(static_cast<const single*>(A.dataPointer()), ptr, planecount,
                    planesize, linesize, withnan);
            }
            res = ArrayOf(classA, dimsRes, ptr);
        } break;
        case NLS_DCOMPLEX: {
            double* ptr = static_cast<double*>(
                ArrayOf::allocateArrayOf(classA, dimsRes.elementCount(), stringVector(), true));
            if (allElements) {
                ComplexSumT<double>(static_cast<const double*>(A.dataPointer()), ptr,
                    dimsA.elementCount(), withnan);
            } else {
                ComplexSumT<double>(static_cast<const double*>(A.dataPointer()), ptr, planecount,
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
                classname + L"_sum");
            Error(L"Nelson:sum:typeNotSupported", msg);
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
