//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "nlsConfig.h"
#include "lapack_eigen.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "CholeskyFactorization.hpp"
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(idx2)
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(idx2)
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
        int idx1 = 0;
        int idx2 = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(idx2)
#endif
        for (idx1 = 0; idx1 < leadDim; idx1++) {
            for (idx2 = idx1 + 1; idx2 < leadDim; idx2++) {
                ptrD[idx2 + idx1 * leadDim] = 0;
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
        int idx1 = 0;
        int idx2 = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for private(idx2)
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
ArrayOf
CholeskyFactorization(const ArrayOf& A, bool lowerTriangle, bool& needToOverload)
{
    needToOverload = false;
    bool isSupportedTypes = (A.isDoubleClass() || A.isSingleClass()) && !A.isSparse();
    if (!isSupportedTypes) {
        needToOverload = true;
        return ArrayOf();
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
    int info = 0;
    if (A.isComplex()) {
        if (A.isDoubleClass()) {
            double* ptr = (double*)R.getReadWriteDataPointer();
            std::complex<double>* ptrZ = reinterpret_cast<std::complex<double>*>(ptr);
            info = doubleComplexCholeskyFactorization(lowerTriangle, ptrZ, (int)A.getColumns());
        } else {
            single* ptr = (single*)R.getReadWriteDataPointer();
            std::complex<single>* ptrZ = reinterpret_cast<std::complex<single>*>(ptr);
            info = singleComplexCholeskyFactorization(lowerTriangle, ptrZ, (int)A.getColumns());
        }
    } else {
        if (A.isDoubleClass()) {
            double* ptr = (double*)R.getReadWriteDataPointer();
            info = doubleRealCholeskyFactorization(lowerTriangle, ptr, (int)A.getColumns());
        } else {
            single* ptr = (single*)R.getReadWriteDataPointer();
            info = singleRealCholeskyFactorization(lowerTriangle, ptr, (int)A.getColumns());
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
