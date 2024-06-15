//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "lapack_eigen_config.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "EigenDecomposition.hpp"
#include "NewWithException.hpp"
#include "EigenLapackHelpers.hpp"
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
bool
EigenDecompositionFullSymmetric(
    const ArrayOf& A, ArrayOf& V, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A)) {
        needToOverload = true;
        return false;
    }
    if (A.isEmpty()) {
        V = A;
        D = A;
        return true;
    }
    auto N = (indexType)A.getDimensionLength(0);
    Dimensions Vdims(N, N);

    NelsonType Aclass = A.getDataClass();
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        single* Vp = (single*)new_with_exception<single>(N * N, false);
        singleEigenDecompositionSymmetric(
            (int)N, Vp, eigenvals, (single*)ArrayOf(A).getReadWriteDataPointer(), true);
        V = ArrayOf(NLS_SINGLE, Vdims, Vp);
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        double* Vp = (double*)new_with_exception<double>(N * N, false);
        doubleEigenDecompositionSymmetric(
            (int)N, Vp, eigenvals, (double*)ArrayOf(A).getReadWriteDataPointer(), true);
        V = ArrayOf(NLS_DOUBLE, Vdims, Vp);
        double* Dp = (double*)new_with_exception<double>(N * N, true);
        D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        float* ptrA = (float*)ArrayOf(A).getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecompositionSymmetric((int)N, Vp, eigenvals, ptrAz, true);
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_SCOMPLEX, Vdims, Vp);
        delete[] eigenvals;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        doublecomplex* Vp = (doublecomplex*)new_with_exception<doublecomplex>(N * N, false);

        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecompositionSymmetric((int)N, Vp, eigenvals, ptrAz, true);
        doublecomplex* Dp = (doublecomplex*)new_with_exception<doublecomplex>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_DCOMPLEX, Vdims, Vp);
        delete[] eigenvals;
    } break;
    default: {
    } break;
    }
    return true;
}
//=============================================================================
bool
EigenDecompositionFullGeneral(const ArrayOf& A, bool balance, ArrayOf& V, ArrayOf& D,
    bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A)) {
        needToOverload = true;
        return false;
    }
    if (A.isEmpty()) {
        V = A;
        D = A;
        return true;
    }
    indexType N = A.getDimensionLength(0);
    NelsonType Aclass = A.getDataClass();
    Dimensions Vdims(N, N);
    switch (Aclass) {
    case NLS_SINGLE: {
        ArrayOf _A(A);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        std::complex<single>* Vpz
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        singleEigenDecomposition((int)N, Vpz, eigenvals, ptrA, true, balance);
        std::complex<single>* Dp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        if (D.allReal()) {
            D.promoteType(NLS_SINGLE);
            single* Vp = (single*)new_with_exception<single>(N * N, false);
            V = ArrayOf(NLS_SINGLE, Vdims, Vp);
            auto* VpC = reinterpret_cast<single*>(Vpz);
            memcpy(Vp, VpC, N * N * sizeof(single));
            delete[] Vpz;
        } else {
            single* Vpc = (single*)new_with_exception<single>(N * N * 2, true);
            V = ArrayOf(NLS_SCOMPLEX, Vdims, Vpc);
            auto* Vp = (single*)reinterpret_cast<single*>(Vpz);
            auto* eigvals = (single*)reinterpret_cast<single*>(eigenvals);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigvals[2 * i + 1] != 0)) {
#if WITH_OPENMP
#pragma omp parallel for
#endif
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = Vp[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = Vp[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -Vp[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
#if WITH_OPENMP
#pragma omp parallel for
#endif
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                    }
                    i++;
                }
            }
            delete[] Vpz;
        }
        delete[] eigenvals;
    } break;
    case NLS_DOUBLE: {
        ArrayOf _A(A);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        std::complex<double>* Vpz
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, false);
        doubleEigenDecomposition((int)N, Vpz, eigenvals, ptrA, true, balance);
        std::complex<double>* Dp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        if (D.allReal()) {
            D.promoteType(NLS_DOUBLE);
            double* Vp = (double*)new_with_exception<double>(N * N, false);
            V = ArrayOf(NLS_DOUBLE, Vdims, Vp);
            auto* VpC = reinterpret_cast<double*>(Vpz);
            memcpy(Vp, VpC, N * N * sizeof(double));
            delete[] Vpz;
        } else {
            double* Vpc = (double*)new_with_exception<double>(N * N * 2, true);
            V = ArrayOf(NLS_DCOMPLEX, Vdims, Vpc);
            auto* Vp = (double*)reinterpret_cast<double*>(Vpz);
            auto* eigvals = (double*)reinterpret_cast<double*>(eigenvals);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigvals[2 * i + 1] != 0)) {
#if WITH_OPENMP
#pragma omp parallel for
#endif
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = Vp[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = Vp[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -Vp[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
#if WITH_OPENMP
#pragma omp parallel for
#endif
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = Vp[j + N * i];
                    }
                    i++;
                }
            }
            delete[] Vpz;
        }
        delete[] eigenvals;
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf _A(A);
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecomposition((int)N, Vp, eigenvals, ptrAz, true, balance);
        std::complex<single>* Dp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, true);
        D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_SCOMPLEX, Vdims, Vp);
        delete[] eigenvals;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        std::complex<double>* Vp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, false);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecomposition((int)N, Vp, eigenvals, ptrAz, true, balance);
        std::complex<double>* Dp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        V = ArrayOf(NLS_DCOMPLEX, Vdims, Vp);
        delete[] eigenvals;
    } break;
    default: {
    } break;
    }
    return true;
}
//=============================================================================
bool
EigenDecompositionCompactSymmetric(
    const ArrayOf& A, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A)) {
        needToOverload = true;
        return false;
    }
    if (A.isEmpty()) {
        D = A;
        return true;
    }

    auto N = (indexType)A.getDimensionLength(0);
    Dimensions Ddims(N, 1);

    NelsonType Aclass = A.getDataClass();
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        singleEigenDecompositionSymmetric(
            (int)N, nullptr, eigenvals, (single*)ArrayOf(A).getReadWriteDataPointer(), false);
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        doubleEigenDecompositionSymmetric(
            (int)N, nullptr, eigenvals, (double*)ArrayOf(A).getReadWriteDataPointer(), false);
        D = ArrayOf(NLS_DOUBLE, Ddims, eigenvals);
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        float* ptrA = (float*)ArrayOf(A).getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecompositionSymmetric((int)N, nullptr, eigenvals, ptrAz, false);
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecompositionSymmetric((int)N, nullptr, eigenvals, ptrAz, false);
        D = ArrayOf(NLS_DCOMPLEX, Ddims, eigenvals);
    } break;
    default: {
    } break;
    }
    return true;
}
//=============================================================================
bool
EigenDecompositionCompactGeneral(
    const ArrayOf& A, bool balance, ArrayOf& D, bool& needToOverload, std::wstring& errorMessage)
{
    needToOverload = false;
    if (!checkSize(A, errorMessage)) {
        return false;
    }
    if (!checkSupportedType(A)) {
        needToOverload = true;
        return false;
    }
    if (A.isEmpty()) {
        D = A;
        return true;
    }
    indexType N = A.getDimensionLength(0);
    NelsonType Aclass = A.getDataClass();
    Dimensions Ddims(N, 1);
    switch (Aclass) {
    case NLS_SINGLE: {
        ArrayOf _A(A);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        singleEigenDecomposition((int)N, nullptr, eigenvals, ptrA, false, balance);
        D = ArrayOf(NLS_DCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_DOUBLE: {
        ArrayOf _A(A);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        doubleEigenDecomposition((int)N, nullptr, eigenvals, ptrA, false, balance);
        D = ArrayOf(NLS_DCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf _A(A);
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        auto* ptrA = (single*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecomposition((int)N, nullptr, eigenvals, ptrAz, false, balance);
        D = ArrayOf(NLS_SCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        auto* ptrA = (double*)_A.getReadWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecomposition((int)N, nullptr, eigenvals, ptrAz, false, balance);
        D = ArrayOf(NLS_DCOMPLEX, Ddims, eigenvals);
    } break;
    default: {
    } break;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
