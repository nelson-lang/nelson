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
#define _SCL_SECURE_NO_WARNINGS
#endif
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "GeneralizedEigenDecomposition.hpp"
#include "NewWithException.hpp"
#include "GeneralizedEigenLapackHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
checkSupportedType(const ArrayOf& M)
{
    return (M.isDoubleType(false) || M.isSingleType(false));
}
//=============================================================================
static bool
checkSupportedType(const ArrayOf& M1, const ArrayOf& M2)
{
    return checkSupportedType(M1) && checkSupportedType(M2);
}
//=============================================================================
static bool
checkSize(const ArrayOf& M, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (!M.is2D()) {
        errorMessage = _W("Input(s) array must be 2 - dimensional.");
        return false;
    }
    if (!M.isSquare()) {
        errorMessage = _W("Square matrix expected.");
        return false;
    }
    return true;
}
//=============================================================================
static bool
checkSize(const ArrayOf& M1, const ArrayOf& M2, std::wstring& errorMessage)
{
    errorMessage.clear();
    if (!checkSize(M1, errorMessage)) {
        return false;
    }
    if (!checkSize(M2, errorMessage)) {
        return false;
    }
    if (M1.getRows() != M2.getRows()) {
        errorMessage = _W("input matrices must be the same size.");
        return false;
    }
    return true;
}
//=============================================================================
static void
promoteSameType(const ArrayOf& M1_IN, const ArrayOf& M2_IN, ArrayOf& M1_OUT, ArrayOf& M2_OUT)
{
    if (M1_IN.getDataClass() == M2_IN.getDataClass()) {
        M1_OUT = M1_IN;
        M2_OUT = M2_IN;
        return;
    }
    M1_OUT = M1_IN;
    M2_OUT = M2_IN;
    bool asSingle = false;
    if (M1_IN.isSingleClass() || M2_IN.isSingleClass()) {
        asSingle = true;
    }
    bool asComplex = false;
    if (M1_IN.isComplex() || M2_IN.isComplex()) {
        asComplex = true;
    }
    if (asComplex && asSingle) {
        M1_OUT.promoteType(NLS_SCOMPLEX);
        M2_OUT.promoteType(NLS_SCOMPLEX);
        return;
    }
    if (asComplex) {
        M1_OUT.promoteType(NLS_DCOMPLEX);
        M2_OUT.promoteType(NLS_DCOMPLEX);
        return;
    }
    if (asSingle) {
        M1_OUT.promoteType(NLS_SINGLE);
        M2_OUT.promoteType(NLS_SINGLE);
        return;
    }
}
//=============================================================================
bool
GeneralizedEigenDecompositionCompactSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, B, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A, B)) {
        needToOverload = true;
        return false;
    }
    ArrayOf _A;
    ArrayOf _B;
    promoteSameType(A, B, _A, _B);
    if (_A.isEmpty() && _B.isEmpty()) {
        D = ArrayOf::emptyConstructor(Dimensions(1, 0));
        D.promoteType(_A.getDataClass());
        return true;
    }
    indexType N = A.getDimensionLength(0);
    Dimensions Ddims(N, 1);

    NelsonType Aclass = _A.getDataClass();
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, true);
        if (!singleGeneralizedEigenDecompositionSymmetric((int)N, nullptr, eigenvals,
                (single*)_A.getReadWriteDataPointer(), (single*)_B.getReadWriteDataPointer(),
                false)) {
            delete[] eigenvals;
            return false;
        }
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
        return true;
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, true);
        if (!doubleGeneralizedEigenDecompositionSymmetric((int)N, nullptr, eigenvals,
                (double*)_A.getReadWriteDataPointer(), (double*)_B.getReadWriteDataPointer(),
                false)) {
            delete[] eigenvals;
            return false;
        }
        D = ArrayOf(NLS_DOUBLE, Ddims, eigenvals);
        return true;
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, true);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        auto* ptrB = (single*)_B.getReadWriteDataPointer();
        auto* cplxA = reinterpret_cast<std::complex<single>*>(ptrA);
        auto* cplxB = reinterpret_cast<std::complex<single>*>(ptrB);
        if (!singleComplexGeneralizedEigenDecompositionSymmetric(
                (int)N, nullptr, eigenvals, cplxA, cplxB, true)) {
            delete[] eigenvals;
            return false;
        }
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
        return true;
    } break;
    case NLS_DCOMPLEX: {
        double* eigenvals = (double*)new_with_exception<double>(N, true);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrB = (double*)_B.getReadWriteDataPointer();
        auto* cplxA = reinterpret_cast<std::complex<double>*>(ptrA);
        auto* cplxB = reinterpret_cast<std::complex<double>*>(ptrB);
        if (!doubleComplexGeneralizedEigenDecompositionSymmetric(
                (int)N, nullptr, eigenvals, cplxA, cplxB, true)) {
            delete[] eigenvals;
            return false;
        }
        D = ArrayOf(NLS_DOUBLE, Ddims, eigenvals);
        return true;
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return false;
}
//=============================================================================
bool
GeneralizedEigenDecompositionFullSymmetric(const ArrayOf& A, const ArrayOf& B, ArrayOf& V,
    ArrayOf& D, bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, B, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A, B)) {
        needToOverload = true;
        return false;
    }
    ArrayOf _A;
    ArrayOf _B;
    promoteSameType(A, B, _A, _B);
    if (_A.isEmpty() && _B.isEmpty()) {
        V = _A;
        D = _A;
        return true;
    }
    indexType N = A.getDimensionLength(0);
    Dimensions Vdims(N, N);

    NelsonType Aclass = _A.getDataClass();
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, true);
        single* Vp = (single*)new_with_exception<single>(N * N, true);
        if (!singleGeneralizedEigenDecompositionSymmetric((int)N, Vp, eigenvals,
                (single*)_A.getReadWriteDataPointer(), (single*)_B.getReadWriteDataPointer(),
                true)) {
            delete[] eigenvals;
            delete[] Vp;
            return false;
        }
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_SINGLE, Vdims, Vp);
        return true;
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, true);
        double* Vp = (double*)new_with_exception<double>(N * N, true);
        if (!doubleGeneralizedEigenDecompositionSymmetric((int)N, Vp, eigenvals,
                (double*)_A.getReadWriteDataPointer(), (double*)_B.getReadWriteDataPointer(),
                true)) {
            delete[] eigenvals;
            delete[] Vp;
            return false;
        }
        double* Dp = (double*)new_with_exception<double>(N * N, true);
        D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_DOUBLE, Vdims, Vp);
        return true;
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, true);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        auto* ptrB = (single*)_B.getReadWriteDataPointer();
        auto* cplxA = reinterpret_cast<std::complex<single>*>(ptrA);
        auto* cplxB = reinterpret_cast<std::complex<single>*>(ptrB);
        if (!singleComplexGeneralizedEigenDecompositionSymmetric(
                (int)N, Vp, eigenvals, cplxA, cplxB, true)) {
            delete[] eigenvals;
            delete[] Vp;
            return false;
        }
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        delete[] eigenvals;
        V = ArrayOf(NLS_SCOMPLEX, Vdims, Vp);
        return true;
    } break;
    case NLS_DCOMPLEX: {
        double* eigenvals = (double*)new_with_exception<double>(N, true);
        std::complex<double>* Vp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrB = (double*)_B.getReadWriteDataPointer();
        auto* cplxA = reinterpret_cast<std::complex<double>*>(ptrA);
        auto* cplxB = reinterpret_cast<std::complex<double>*>(ptrB);
        if (!doubleComplexGeneralizedEigenDecompositionSymmetric(
                (int)N, Vp, eigenvals, cplxA, cplxB, true)) {
            delete[] eigenvals;
            delete[] Vp;
            return false;
        }
        double* Dp = (double*)new_with_exception<double>(N * N, true);
        D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        delete[] eigenvals;
        V = ArrayOf(NLS_DCOMPLEX, Vdims, Vp);
        return true;
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return false;
}
//=============================================================================
bool
GeneralizedEigenDecompositionFullGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& V, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, B, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A, B)) {
        needToOverload = true;
        return false;
    }
    ArrayOf _A;
    ArrayOf _B;
    promoteSameType(A, B, _A, _B);
    if (_A.isEmpty() && _B.isEmpty()) {
        V = _A;
        D = _A;
        return true;
    }
    indexType N = A.getDimensionLength(0);
    Dimensions Vdims(N, N);
    NelsonType classA = _A.getDataClass();
    switch (classA) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(2 * N, false);
        single* Vp = (single*)new_with_exception<single>(N * N, false);
        singleGeneralizedEigenDecomposition((int)N, Vp, eigenvals,
            (single*)_A.getReadWriteDataPointer(), (single*)_B.getReadWriteDataPointer(), true);
        bool complexEigenvalues = false;
        for (int i = 0; (i < N) && !complexEigenvalues; i++) {
            complexEigenvalues = (eigenvals[2 * i + 1] != 0);
        }
        if (!complexEigenvalues) {
            single* Dp = (single*)new_with_exception<single>(N * N, true);
            D = ArrayOf(NLS_SINGLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[2 * i];
            }
            V = ArrayOf(NLS_SINGLE, Vdims, Vp);
        } else {
            std::complex<single>* Dp
                = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, true);
            D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i].real(eigenvals[2 * i]);
                if (std::isnan(eigenvals[2 * i + 1])) {
                    Dp[i + N * i].imag(0);
                } else {
                    Dp[i + N * i].imag(eigenvals[2 * i + 1]);
                }
            }
            single* Vpc = (single*)new_with_exception<single>(N * N * 2, true);
            V = ArrayOf(NLS_SCOMPLEX, Vdims, Vpc);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigenvals[2 * i + 1] != 0)) {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = Vp[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = Vp[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -Vp[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                    }
                    i++;
                }
            }
            delete[] Vp;
        }
        delete[] eigenvals;
        return true;
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(2 * N, false);
        double* Vp = (double*)new_with_exception<double>(N * N, false);
        doubleGeneralizedEigenDecomposition((int)N, Vp, eigenvals,
            (double*)_A.getReadWriteDataPointer(), (double*)_B.getReadWriteDataPointer(), true);
        bool complexEigenvalues = false;
        for (int i = 0; (i < N) && !complexEigenvalues; i++) {
            complexEigenvalues = (eigenvals[2 * i + 1] != 0);
        }
        if (!complexEigenvalues) {
            double* Dp = (double*)new_with_exception<double>(N * N, true);
            D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[2 * i];
            }
            V = ArrayOf(NLS_DOUBLE, Vdims, Vp);
        } else {
            std::complex<double>* Dp
                = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, true);
            D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i].real(eigenvals[2 * i]);
                if (std::isnan(eigenvals[2 * i + 1])) {
                    Dp[i + N * i].imag(0);
                } else {
                    Dp[i + N * i].imag(eigenvals[2 * i + 1]);
                }
            }
            double* Vpc = (double*)new_with_exception<double>(N * N * 2, true);
            V = ArrayOf(NLS_DCOMPLEX, Vdims, Vpc);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigenvals[2 * i + 1] != 0)) {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = Vp[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = Vp[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -Vp[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                    }
                    i++;
                }
            }
            delete[] Vp;
        }
        delete[] eigenvals;
        return true;
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        auto* cplxA = reinterpret_cast<std::complex<single>*>(_A.getReadWriteDataPointer());
        auto* cplxB = reinterpret_cast<std::complex<single>*>(_B.getReadWriteDataPointer());
        singleComplexGeneralizedEigenDecomposition((int)N, Vp, eigenvals, cplxA, cplxB, true);
        std::complex<single>* Dp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[(i + N * i)] = eigenvals[i];
            if (std::isnan(Dp[(i + N * i)].imag())) {
                Dp[(i + N * i)].imag(0);
            }
        }
        delete[] eigenvals;
        V = ArrayOf(NLS_SCOMPLEX, Vdims, Vp);
        return true;
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        std::complex<double>* Vp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, false);
        auto* cplxA = reinterpret_cast<std::complex<double>*>(_A.getReadWriteDataPointer());
        auto* cplxB = reinterpret_cast<std::complex<double>*>(_B.getReadWriteDataPointer());
        doubleComplexGeneralizedEigenDecomposition((int)N, Vp, eigenvals, cplxA, cplxB, true);
        std::complex<double>* Dp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, false);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[(i + N * i)] = eigenvals[i];
            if (std::isnan(Dp[(i + N * i)].imag())) {
                Dp[(i + N * i)].imag(0);
            }
        }
        delete[] eigenvals;
        V = ArrayOf(NLS_DCOMPLEX, Vdims, Vp);
        return true;
    } break;
    default: {
        needToOverload = true;
        return false;
    } break;
    }
    return false;
}
//=============================================================================
bool
GeneralizedEigenDecompositionCompactGeneral(const ArrayOf& A, const ArrayOf& B, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, B, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A, B)) {
        needToOverload = true;
        return false;
    }
    ArrayOf _A;
    ArrayOf _B;
    promoteSameType(A, B, _A, _B);
    if (_A.isEmpty() && _B.isEmpty()) {
        D = _A;
        return true;
    }
    indexType N = A.getDimensionLength(0);
    Dimensions Vdims(N, 1);
    NelsonType classA = _A.getDataClass();
    switch (classA) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(2 * N, false);
        singleGeneralizedEigenDecomposition((int)N, nullptr, eigenvals,
            (single*)_A.getReadWriteDataPointer(), (single*)_B.getReadWriteDataPointer(), false);
        bool complexEigenvalues = false;
        for (int i = 0; (i < N) && !complexEigenvalues; i++) {
            complexEigenvalues = (eigenvals[2 * i + 1] != 0);
        }
        if (!complexEigenvalues) {
            single* Dp = (single*)new_with_exception<single>(N, false);
            D = ArrayOf(NLS_SINGLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i] = eigenvals[2 * i];
            }
        } else {
            std::complex<single>* Dp
                = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
            D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i].real(eigenvals[2 * i]);
                if (std::isnan(eigenvals[2 * i + 1])) {
                    Dp[i].imag(0);
                } else {
                    Dp[i].imag(eigenvals[2 * i + 1]);
                }
            }
        }
        delete[] eigenvals;
        return true;
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(2 * N, false);
        doubleGeneralizedEigenDecomposition((int)N, nullptr, eigenvals,
            (double*)_A.getReadWriteDataPointer(), (double*)_B.getReadWriteDataPointer(), false);
        bool complexEigenvalues = false;
        for (int i = 0; (i < N) && !complexEigenvalues; i++) {
            complexEigenvalues = (eigenvals[2 * i + 1] != 0);
        }
        if (!complexEigenvalues) {
            double* Dp = (double*)new_with_exception<double>(N, false);
            D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i] = eigenvals[2 * i];
            }
        } else {
            std::complex<double>* Dp
                = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
            D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i].real(eigenvals[2 * i]);
                if (std::isnan(eigenvals[2 * i + 1])) {
                    Dp[i].imag(0);
                } else {
                    Dp[i].imag(eigenvals[2 * i + 1]);
                }
            }
        }
        delete[] eigenvals;
        return true;
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        auto* cplxA = reinterpret_cast<std::complex<single>*>(_A.getReadWriteDataPointer());
        auto* cplxB = reinterpret_cast<std::complex<single>*>(_B.getReadWriteDataPointer());
        singleComplexGeneralizedEigenDecomposition((int)N, nullptr, eigenvals, cplxA, cplxB, false);
        D = ArrayOf(NLS_SCOMPLEX, Vdims, eigenvals);
        return true;
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        auto* cplxA = reinterpret_cast<std::complex<double>*>(_A.getReadWriteDataPointer());
        auto* cplxB = reinterpret_cast<std::complex<double>*>(_B.getReadWriteDataPointer());
        doubleComplexGeneralizedEigenDecomposition((int)N, nullptr, eigenvals, cplxA, cplxB, false);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, eigenvals);
        return true;
    } break;
    default: {
        needToOverload = true;
        return false;
    } break;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
