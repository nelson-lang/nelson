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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "CholeskyFactorization.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int
singleRealCholeskyFactorization(bool lowerTriangle, single* ptrS, int leadDim)
{
    int info;
    char orient = lowerTriangle ? 'L' : 'U';
    LAPACK_spotrf(&orient, &leadDim, ptrS, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
        int idx1 = 0;
        int idx2 = 0;
#if WITH_OPENMP
#pragma omp parallel for private(idx2) if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (idx1 = 0; idx1 < leadDim; idx1++) {
            for (idx2 = idx1 + 1; idx2 < leadDim; idx2++) {
                ptrS[idx2 + idx1 * leadDim] = 0;
            }
        }
    }
    return 0;
}
//=============================================================================
int
singleComplexCholeskyFactorization(bool lowerTriangle, std::complex<single>* ptrZ, int leadDim)
{
    int info = 0;
    char orient = lowerTriangle ? 'L' : 'U';
    LAPACK_cpotrf(&orient, &leadDim, ptrZ, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
        int idx1 = 0;
        int idx2 = 0;
#if WITH_OPENMP
#pragma omp parallel for private(idx2) if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (idx1 = 0; idx1 < leadDim; idx1++) {
            for (idx2 = idx1 + 1; idx2 < leadDim; idx2++) {
                ptrZ[idx2 + idx1 * leadDim].real(0);
                ptrZ[idx2 + idx1 * leadDim].imag(0);
            }
        }
    }
    return 0;
}
//=============================================================================
static int
doubleRealCholeskyFactorization(bool lowerTriangle, double* ptrD, int leadDim)
{
    int info;
    char orient = lowerTriangle ? 'L' : 'U';
    LAPACK_dpotrf(&orient, &leadDim, ptrD, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
        OMP_PARALLEL_FOR_LOOP(leadDim * leadDim)
        for (int idx = 0; idx < leadDim * leadDim; idx++) {
            int row = idx % leadDim;
            int col = idx / leadDim;
            if ((lowerTriangle && col > row) || (!lowerTriangle && row > col)) {
                ptrD[idx] = 0;
            }
        }
    }
    return 0;
}
//=============================================================================
static int
doubleComplexCholeskyFactorization(bool lowerTriangle, std::complex<double>* ptrZ, int leadDim)
{
    int info = 0;
    char orient = lowerTriangle ? 'L' : 'U';
    LAPACK_zpotrf(&orient, &leadDim, ptrZ, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
        const std::complex<double> zero(0.0, 0.0);
        const int totalElements = (leadDim * (leadDim - 1)) / 2;

        OMP_PARALLEL_FOR_LOOP(totalElements)
        for (int k = 0; k < totalElements; ++k) {
            int i = k / leadDim;
            int j = k % leadDim + i + 1;
            if (j < leadDim) {
                ptrZ[i + j * leadDim] = zero;
            }
        }
    }
    return 0;
}
//=============================================================================
ArrayOf
CholeskyFactorization(const ArrayOf& A, bool lowerTriangle, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes = (A.isDoubleClass() || A.isSingleClass()) && !A.isSparse();
    if (!isSupportedTypes) {
        needToOverload = true;
        return {};
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf res = ArrayOf::emptyConstructor();
        if (A.isSingleType()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    }
    ArrayOf R = A;
    Dimensions dimsA = A.getDimensions();
    int info = 0;
    if (A.isComplex()) {
        if (A.isDoubleClass()) {
            double* ptr = (double*)R.getReadWriteDataPointer();
            std::complex<double>* ptrZ = reinterpret_cast<std::complex<double>*>(ptr);
            info = doubleComplexCholeskyFactorization(lowerTriangle, ptrZ, (int)dimsA.getColumns());
        } else {
            single* ptr = (single*)R.getReadWriteDataPointer();
            std::complex<single>* ptrZ = reinterpret_cast<std::complex<single>*>(ptr);
            info = singleComplexCholeskyFactorization(lowerTriangle, ptrZ, (int)dimsA.getColumns());
        }
        if (R.allReal()) {
            R.promoteType(A.isDoubleClass() ? NLS_DOUBLE : NLS_SINGLE);
        }
    } else {
        if (A.isDoubleClass()) {
            double* ptr = (double*)R.getReadWriteDataPointer();
            info = doubleRealCholeskyFactorization(lowerTriangle, ptr, (int)dimsA.getColumns());
        } else {
            single* ptr = (single*)R.getReadWriteDataPointer();
            info = singleRealCholeskyFactorization(lowerTriangle, ptr, (int)dimsA.getColumns());
        }
    }
    if (info != 0) {
        if (info < 0) {
            Error(_W("One argument had an illegal value."));
        }
        if (info > 0) {
            Error(_W("Positive finite matrix expected."));
        }
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
