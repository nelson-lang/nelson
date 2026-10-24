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
#include <algorithm>
#include <cmath>
#include <cstring>
#include <limits>
#include <vector>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "LapackPrototypes.hpp"
#include "EigenDecomposition.hpp"
#include "NewWithException.hpp"
#include "EigenLapackHelpers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
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
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:linear_algebra:inputArrayDimensional",
                L"Input(s) array must be 2 - dimensional.");
        return false;
    }
    if (!M.isSquare()) {
        errorMessage
            = formatI18nMessage(Nelson::resolveMessageTemplate(
                                    L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected."),
                L"Square matrix");
        return false;
    }
    return true;
}
//=============================================================================
static bool
containsNaN(const ArrayOf& A)
{
    NelsonType cls = A.dataClass();
    size_t count = (size_t)A.elementCount();
    if (cls == NLS_SCOMPLEX || cls == NLS_DCOMPLEX) {
        count *= 2;
    }
    if (cls == NLS_SINGLE || cls == NLS_SCOMPLEX) {
        const single* ptr = (const single*)A.dataPointer();
        for (size_t k = 0; k < count; ++k) {
            if (std::isnan(ptr[k])) {
                return true;
            }
        }
    } else {
        const double* ptr = (const double*)A.dataPointer();
        for (size_t k = 0; k < count; ++k) {
            if (std::isnan(ptr[k])) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
// Numerical computing compatibility: eig with NaN input returns NaN results
// (all NaN eigenvector matrix, NaN on the diagonal of D).
static ArrayOf
nanArray(NelsonType cls, indexType rows, indexType cols, bool diagonalOnly)
{
    Dimensions dims(rows, cols);
    indexType nbElements = dims.elementCount();
    void* ptr = ArrayOf::allocateArrayOf(cls, nbElements, stringVector(), true);
    ArrayOf res(cls, dims, ptr);
    bool asComplex = (cls == NLS_SCOMPLEX || cls == NLS_DCOMPLEX);
    bool asSingle = (cls == NLS_SINGLE || cls == NLS_SCOMPLEX);
    indexType stride = asComplex ? 2 : 1;
    if (asSingle) {
        single* sp = (single*)ptr;
        single nanValue = std::numeric_limits<single>::quiet_NaN();
        if (diagonalOnly) {
            for (indexType i = 0; i < rows && i < cols; ++i) {
                for (indexType c = 0; c < stride; ++c) {
                    sp[stride * (i + rows * i) + c] = nanValue;
                }
            }
        } else {
            for (indexType k = 0; k < nbElements * stride; ++k) {
                sp[k] = nanValue;
            }
        }
    } else {
        double* dp = (double*)ptr;
        double nanValue = std::numeric_limits<double>::quiet_NaN();
        if (diagonalOnly) {
            for (indexType i = 0; i < rows && i < cols; ++i) {
                for (indexType c = 0; c < stride; ++c) {
                    dp[stride * (i + rows * i) + c] = nanValue;
                }
            }
        } else {
            for (indexType k = 0; k < nbElements * stride; ++k) {
                dp[k] = nanValue;
            }
        }
    }
    return res;
}
//=============================================================================
template <typename T>
static bool
eigenvaluesAreReal(const std::complex<T>* values, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        if (values[k].imag() != T(0)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
tryLowRankSymmetricEigenvaluesDouble(const double* A, int n, double* eigenvalues)
{
    constexpr int MAX_FAST_RANK = 32;
    if (n < 64) {
        return false;
    }

    std::vector<double> basis((size_t)MAX_FAST_RANK * (size_t)n);
    std::vector<double> column(n);
    double referenceNorm = 0.;
    double tolerance = 0.;
    int rank = 0;

    for (int col = 0; col < n; ++col) {
        if (rank == MAX_FAST_RANK) {
            return false;
        }

        const double* sourceColumn = A + (size_t)col * (size_t)n;
        double columnNormSquared = 0.;
        for (int row = 0; row < n; ++row) {
            double value = sourceColumn[row];
            column[row] = value;
            columnNormSquared += value * value;
        }

        double columnNorm = std::sqrt(columnNormSquared);
        if (columnNorm > referenceNorm) {
            referenceNorm = columnNorm;
            tolerance = referenceNorm * std::numeric_limits<double>::epsilon() * (double)n * 16.;
        }
        if (columnNorm <= tolerance) {
            continue;
        }

        for (int k = 0; k < rank; ++k) {
            const double* q = basis.data() + (size_t)k * (size_t)n;
            double projection = 0.;
            for (int row = 0; row < n; ++row) {
                projection += q[row] * column[row];
            }
            for (int row = 0; row < n; ++row) {
                column[row] -= projection * q[row];
            }
        }

        double residualNormSquared = 0.;
        for (int row = 0; row < n; ++row) {
            residualNormSquared += column[row] * column[row];
        }
        double residualNorm = std::sqrt(residualNormSquared);
        if (residualNorm <= tolerance) {
            continue;
        }

        double* q = basis.data() + (size_t)rank * (size_t)n;
        double inverseNorm = 1. / residualNorm;
        for (int row = 0; row < n; ++row) {
            q[row] = column[row] * inverseNorm;
        }
        ++rank;
    }

    std::fill(eigenvalues, eigenvalues + n, 0.);
    if (rank == 0) {
        return true;
    }

    std::vector<double> projected((size_t)rank * (size_t)rank);
    std::vector<double> product(n);
    for (int col = 0; col < rank; ++col) {
        const double* qCol = basis.data() + (size_t)col * (size_t)n;
        std::fill(product.begin(), product.end(), 0.);
        for (int sourceCol = 0; sourceCol < n; ++sourceCol) {
            double qValue = qCol[sourceCol];
            const double* sourceColumn = A + (size_t)sourceCol * (size_t)n;
            for (int row = 0; row < n; ++row) {
                product[row] += sourceColumn[row] * qValue;
            }
        }
        for (int row = 0; row <= col; ++row) {
            const double* qRow = basis.data() + (size_t)row * (size_t)n;
            double value = 0.;
            for (int k = 0; k < n; ++k) {
                value += qRow[k] * product[k];
            }
            projected[(size_t)row + (size_t)col * (size_t)rank] = value;
            projected[(size_t)col + (size_t)row * (size_t)rank] = value;
        }
    }

    std::vector<double> compactEigenvalues(rank);
    char jobz = 'N';
    char uplo = 'U';
    int lwork = -1;
    double workQuery = 0.;
    int info = 0;
    LAPACK_dsyev(&jobz, &uplo, &rank, projected.data(), &rank, compactEigenvalues.data(),
        &workQuery, &lwork, &info);
    if (info != 0) {
        return false;
    }
    lwork = std::max(1, (int)workQuery);
    std::vector<double> work((size_t)lwork);
    LAPACK_dsyev(&jobz, &uplo, &rank, projected.data(), &rank, compactEigenvalues.data(),
        work.data(), &lwork, &info);
    if (info != 0) {
        return false;
    }

    int negativeCount = 0;
    while (negativeCount < rank && compactEigenvalues[negativeCount] < 0.) {
        eigenvalues[negativeCount] = compactEigenvalues[negativeCount];
        ++negativeCount;
    }

    int destination = negativeCount + (n - rank);
    for (int k = negativeCount; k < rank; ++k) {
        eigenvalues[destination++] = compactEigenvalues[k];
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
    auto N = (indexType)A.dimensionLength(0);
    Dimensions Vdims(N, N);

    NelsonType Aclass = A.dataClass();
    if (containsNaN(A)) {
        V = nanArray(Aclass, N, N, false);
        D = nanArray(Aclass, N, N, true);
        return true;
    }
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        single* Vp = (single*)new_with_exception<single>(N * N, false);
        singleEigenDecompositionSymmetric(
            (int)N, Vp, eigenvals, (single*)ArrayOf(A).readWriteDataPointer(), true);
        V = ArrayOf(NLS_SINGLE, Vdims, Vp);
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        delete[] eigenvals;
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        double* Vp = (double*)new_with_exception<double>(N * N, false);
        doubleEigenDecompositionSymmetric(
            (int)N, Vp, eigenvals, (double*)ArrayOf(A).readWriteDataPointer(), true);
        V = ArrayOf(NLS_DOUBLE, Vdims, Vp);
        double* Dp = (double*)new_with_exception<double>(N * N, true);
        D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            Dp[i + N * i] = eigenvals[i];
        }
        delete[] eigenvals;
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        float* ptrA = (float*)ArrayOf(A).readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecompositionSymmetric((int)N, Vp, eigenvals, ptrAz, true);
        single* Dp = (single*)new_with_exception<single>(N * N, true);
        D = ArrayOf(NLS_SINGLE, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
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

        auto* ptrA = (double*)_A.readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecompositionSymmetric((int)N, Vp, eigenvals, ptrAz, true);
        doublecomplex* Dp = (doublecomplex*)new_with_exception<doublecomplex>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
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
    indexType N = A.dimensionLength(0);
    NelsonType Aclass = A.dataClass();
    Dimensions Vdims(N, N);
    if (containsNaN(A)) {
        V = nanArray(Aclass, N, N, false);
        D = nanArray(Aclass, N, N, true);
        return true;
    }
    switch (Aclass) {
    case NLS_SINGLE: {
        ArrayOf _A(A);
        auto* ptrA = (single*)_A.readWriteDataPointer();
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        single* VpRaw = (single*)new_with_exception<single>(N * N, false);
        singleEigenDecomposition((int)N, VpRaw, eigenvals, ptrA, true, balance);
        if (eigenvaluesAreReal<single>(eigenvals, N)) {
            single* Dp = (single*)new_with_exception<single>(N * N, true);
            D = ArrayOf(NLS_SINGLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[i].real();
            }
            V = ArrayOf(NLS_SINGLE, Vdims, VpRaw);
        } else {
            std::complex<single>* Dp
                = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, true);
            D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[i];
            }
            single* Vpc = (single*)new_with_exception<single>(N * N * 2, true);
            V = ArrayOf(NLS_SCOMPLEX, Vdims, Vpc);
            auto* eigvals = (single*)reinterpret_cast<single*>(eigenvals);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigvals[2 * i + 1] != 0)) {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = VpRaw[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = VpRaw[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = VpRaw[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -VpRaw[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = VpRaw[j + N * i];
                    }
                    i++;
                }
            }
            delete[] VpRaw;
        }
        delete[] eigenvals;
    } break;
    case NLS_DOUBLE: {
        ArrayOf _A(A);
        auto* ptrA = (double*)_A.readWriteDataPointer();
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        double* VpRaw = (double*)new_with_exception<double>(N * N, false);
        doubleEigenDecomposition((int)N, VpRaw, eigenvals, ptrA, true, balance);
        if (eigenvaluesAreReal<double>(eigenvals, N)) {
            double* Dp = (double*)new_with_exception<double>(N * N, true);
            D = ArrayOf(NLS_DOUBLE, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[i].real();
            }
            V = ArrayOf(NLS_DOUBLE, Vdims, VpRaw);
        } else {
            std::complex<double>* Dp
                = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, true);
            D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
            OMP_PARALLEL_FOR_LOOP(N)
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                Dp[i + N * i] = eigenvals[i];
            }
            double* Vpc = (double*)new_with_exception<double>(N * N * 2, true);
            V = ArrayOf(NLS_DCOMPLEX, Vdims, Vpc);
            auto* eigvals = (double*)reinterpret_cast<double*>(eigenvals);
            int i = 0;
            while (i < N) {
                if ((i < N - 1) && (eigvals[2 * i + 1] != 0)) {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = VpRaw[j + N * i];
                        Vpc[2 * (j + N * i) + 1] = VpRaw[j + N * (i + 1)];
                        Vpc[2 * (j + N * (i + 1))] = VpRaw[j + N * i];
                        Vpc[2 * (j + N * (i + 1)) + 1] = -VpRaw[j + N * (i + 1)];
                    }
                    i += 2;
                } else {
                    OMP_PARALLEL_FOR_LOOP(N)
                    for (ompIndexType j = 0; j < (ompIndexType)N; j++) {
                        Vpc[2 * (j + N * i)] = VpRaw[j + N * i];
                    }
                    i++;
                }
            }
            delete[] VpRaw;
        }
        delete[] eigenvals;
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf _A(A);
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        std::complex<single>* Vp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, false);
        auto* ptrA = (single*)_A.readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecomposition((int)N, Vp, eigenvals, ptrAz, true, balance);
        std::complex<single>* Dp
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N * N, true);
        D = ArrayOf(NLS_SCOMPLEX, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
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
        auto* ptrA = (double*)_A.readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<doublecomplex*>(ptrA);
        doubleComplexEigenDecomposition((int)N, Vp, eigenvals, ptrAz, true, balance);
        std::complex<double>* Dp
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N * N, true);
        D = ArrayOf(NLS_DCOMPLEX, Vdims, Dp);
        OMP_PARALLEL_FOR_LOOP(N)
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

    auto N = (indexType)A.dimensionLength(0);
    Dimensions Ddims(N, 1);

    NelsonType Aclass = A.dataClass();
    if (containsNaN(A)) {
        D = nanArray(Aclass, N, 1, false);
        return true;
    }
    switch (Aclass) {
    case NLS_SINGLE: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        singleEigenDecompositionSymmetric(
            (int)N, nullptr, eigenvals, (single*)ArrayOf(A).readWriteDataPointer(), false);
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
    } break;
    case NLS_DOUBLE: {
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        ArrayOf _A(A);
        double* ptrA = (double*)_A.readWriteDataPointer();
        if (!tryLowRankSymmetricEigenvaluesDouble(ptrA, (int)N, eigenvals)) {
            doubleEigenDecompositionSymmetric((int)N, nullptr, eigenvals, ptrA, false);
        }
        D = ArrayOf(NLS_DOUBLE, Ddims, eigenvals);
    } break;
    case NLS_SCOMPLEX: {
        single* eigenvals = (single*)new_with_exception<single>(N, false);
        float* ptrA = (float*)ArrayOf(A).readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecompositionSymmetric((int)N, nullptr, eigenvals, ptrAz, false);
        D = ArrayOf(NLS_SINGLE, Ddims, eigenvals);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        double* eigenvals = (double*)new_with_exception<double>(N, false);
        auto* ptrA = (double*)_A.readWriteDataPointer();
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
    indexType N = A.dimensionLength(0);
    NelsonType Aclass = A.dataClass();
    Dimensions Ddims(N, 1);
    if (containsNaN(A)) {
        D = nanArray(Aclass, N, 1, false);
        return true;
    }
    switch (Aclass) {
    case NLS_SINGLE: {
        ArrayOf _A(A);
        auto* ptrA = (single*)_A.readWriteDataPointer();
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        singleEigenDecomposition((int)N, nullptr, eigenvals, ptrA, false, balance);
        D = ArrayOf(NLS_SCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_DOUBLE: {
        ArrayOf _A(A);
        auto* ptrA = (double*)_A.readWriteDataPointer();
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        doubleEigenDecomposition((int)N, nullptr, eigenvals, ptrA, false, balance);
        D = ArrayOf(NLS_DCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf _A(A);
        std::complex<single>* eigenvals
            = (std::complex<single>*)new_with_exception<std::complex<single>>(N, false);
        auto* ptrA = (single*)_A.readWriteDataPointer();
        auto* ptrAz = reinterpret_cast<singlecomplex*>(ptrA);
        singleComplexEigenDecomposition((int)N, nullptr, eigenvals, ptrAz, false, balance);
        D = ArrayOf(NLS_SCOMPLEX, Ddims, eigenvals);
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf _A(A);
        std::complex<double>* eigenvals
            = (std::complex<double>*)new_with_exception<std::complex<double>>(N, false);
        auto* ptrA = (double*)_A.readWriteDataPointer();
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
