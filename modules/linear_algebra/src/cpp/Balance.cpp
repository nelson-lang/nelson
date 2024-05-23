//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/src/misc/lapacke.h>
#include "Balance.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "LinearAlgebraHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
doubleRealBalance(const ArrayOf& A, bool noperm, int nLhs);
static ArrayOfVector
doubleComplexBalance(const ArrayOf& A, bool noperm, int nLhs);
static ArrayOfVector
singleRealBalance(const ArrayOf& A, bool noperm, int nLhs);
static ArrayOfVector
singleComplexBalance(const ArrayOf& A, bool noperm, int nLhs);
//=============================================================================
template <class T>
static ArrayOf
permuteVector(const T* ptrScale, int N, int IHI)
{
    ArrayOf P = ArrayOf::doubleMatrix2dConstructor(N, 1);
    double* ptrPermutingVector = (double*)P.getDataPointer();
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        ptrPermutingVector[i] = (double)(i + 1);
    }

    for (int i = N - 1; i >= IHI; i--) {
        int j = (int)(ptrScale[i] - 1);
        std::swap(ptrPermutingVector[i], ptrPermutingVector[j]);
    }
    return P;
}
//=============================================================================
template <class T>
static ArrayOf
scalingVector(const T* ptrScale, int N, int ILO, int IHI)
{
    ArrayOf S = ArrayOf::doubleMatrix2dConstructor(N, 1);
    double* ptrScalingVector = (double*)S.getDataPointer();

#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (int i = 0; i < N; ++i) {
        if (i < ILO - 1 || i >= IHI) {
            ptrScalingVector[i] = 1.0;
        } else {
            ptrScalingVector[i] = static_cast<double>(ptrScale[i]);
        }
    }
    return S;
}
//=============================================================================
ArrayOfVector
Balance(const ArrayOf& A, bool noperm, int nLhs, bool& needToOverload)
{
    bool isSupportedType = !A.isSparse() && (A.isSingleClass() || A.isDoubleClass()) && A.is2D();
    if (!isSupportedType) {
        needToOverload = true;
        return {};
    }
    Dimensions dimsA = A.getDimensions();
    if (dimsA.getRows() != dimsA.getColumns()) {
        Error(_("Matrix must be square."), "Nelson:square");
    }
    if (A.isEmpty()) {
        ArrayOfVector retval;
        Dimensions dims(0, 0);
        ArrayOf emptyMatrix = ArrayOf::emptyConstructor(dims);
        needToOverload = false;
        if (A.isSingleClass()) {
            emptyMatrix.promoteType(NLS_SINGLE);
        }
        retval << emptyMatrix;
        if (nLhs > 1) {
            retval << emptyMatrix;
        }
        if (nLhs > 2) {
            retval << emptyMatrix;
        }
        return retval;
    }
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        return doubleRealBalance(A, noperm, nLhs);
    } break;
    case NLS_SINGLE: {
        return singleRealBalance(A, noperm, nLhs);
    } break;
    case NLS_DCOMPLEX: {
        return doubleComplexBalance(A, noperm, nLhs);
    } break;
    case NLS_SCOMPLEX: {
        return singleComplexBalance(A, noperm, nLhs);
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
ArrayOfVector
doubleRealBalance(const ArrayOf& A, bool noperm, int nLhs)
{
    if (!isAllFinite<double>(A)) {
        Error(
            _("Input to BALANCE must not contain NaN or Inf."), "Nelson:balance:matrixWithNaNInf");
    }
    char JOB = noperm ? (false ? 'N' : 'S') : (false ? 'P' : 'B');
    int N = (int)A.getColumns();
    int ILO = 0;
    int IHI = 0;
    ArrayOf B(A);
    B.ensureSingleOwner();
    ArrayOf SCALE = ArrayOf::doubleMatrix2dConstructor(N, 1);
    double* ptrScale = (double*)SCALE.getDataPointer();

    int INFO = LAPACKE_dgebal(
        LAPACK_COL_MAJOR, JOB, N, (double*)B.getDataPointer(), N, &ILO, &IHI, ptrScale);
    if (INFO < 0) {
        Error(_("Invalid arguments for LAPACKE_dgebal"));
    }

    ArrayOfVector retval;

    if (nLhs <= 1) {
        retval << B;
        return retval;
    }

    if (nLhs == 2) {
        retval << ArrayOf::diagonalConstructor(scalingVector<double>(ptrScale, N, ILO, IHI), 0);
        retval << B;
    } else {
        retval << scalingVector<double>(ptrScale, N, ILO, IHI);
        retval << permuteVector(ptrScale, N, IHI);
        retval << B;
    }
    return retval;
}
//=============================================================================

ArrayOfVector
doubleComplexBalance(const ArrayOf& A, bool noperm, int nLhs)
{
    if (!isAllFinite<double>(A)) {
        Error(
            _("Input to BALANCE must not contain NaN or Inf."), "Nelson:balance:matrixWithNaNInf");
    }
    char JOB = noperm ? (false ? 'N' : 'S') : (false ? 'P' : 'B');
    int N = (int)A.getColumns();
    int ILO = 0;
    int IHI = 0;
    ArrayOf B(A);
    B.ensureSingleOwner();
    auto* pzB = reinterpret_cast<doublecomplex*>((double*)B.getDataPointer());

    ArrayOf SCALE = ArrayOf::doubleMatrix2dConstructor(N, 1);
    double* ptrScale = (double*)SCALE.getDataPointer();

    int INFO = LAPACKE_zgebal(LAPACK_COL_MAJOR, JOB, N, pzB, N, &ILO, &IHI, ptrScale);
    if (INFO < 0) {
        Error(_("Invalid arguments for LAPACKE_dgebal"));
    }

    ArrayOfVector retval;

    if (nLhs <= 1) {
        retval << B;
        return retval;
    }

    if (nLhs == 2) {
        retval << ArrayOf::diagonalConstructor(scalingVector<double>(ptrScale, N, ILO, IHI), 0);
        retval << B;
    } else {
        retval << scalingVector<double>(ptrScale, N, ILO, IHI);
        retval << permuteVector(ptrScale, N, IHI);
        retval << B;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
singleRealBalance(const ArrayOf& A, bool noperm, int nLhs)
{
    if (!isAllFinite<single>(A)) {
        Error(
            _("Input to BALANCE must not contain NaN or Inf."), "Nelson:balance:matrixWithNaNInf");
    }
    char JOB = noperm ? (false ? 'N' : 'S') : (false ? 'P' : 'B');
    int N = (int)A.getColumns();
    int ILO = 0;
    int IHI = 0;
    ArrayOf B(A);
    B.ensureSingleOwner();
    ArrayOf SCALE = ArrayOf::singleMatrix2dConstructor(N, 1);
    single* ptrScale = (single*)SCALE.getDataPointer();

    int INFO = LAPACKE_sgebal(
        LAPACK_COL_MAJOR, JOB, N, (single*)B.getDataPointer(), N, &ILO, &IHI, ptrScale);
    if (INFO < 0) {
        Error(_("Invalid arguments for LAPACKE_dgebal"));
    }

    ArrayOfVector retval;

    if (nLhs <= 1) {
        retval << B;
        return retval;
    }

    if (nLhs == 2) {
        retval << ArrayOf::diagonalConstructor(scalingVector<single>(ptrScale, N, ILO, IHI), 0);
        retval << B;
    } else {
        retval << scalingVector<single>(ptrScale, N, ILO, IHI);
        retval << permuteVector(ptrScale, N, IHI);
        retval << B;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
singleComplexBalance(const ArrayOf& A, bool noperm, int nLhs)
{
    if (!isAllFinite<single>(A)) {
        Error(
            _("Input to BALANCE must not contain NaN or Inf."), "Nelson:balance:matrixWithNaNInf");
    }
    char JOB = noperm ? (false ? 'N' : 'S') : (false ? 'P' : 'B');
    int N = (int)A.getColumns();
    int ILO = 0;
    int IHI = 0;
    ArrayOf B(A);
    B.ensureSingleOwner();
    auto* pzB = reinterpret_cast<singlecomplex*>((single*)B.getDataPointer());

    ArrayOf SCALE = ArrayOf::singleMatrix2dConstructor(N, 1);
    single* ptrScale = (single*)SCALE.getDataPointer();

    int INFO = LAPACKE_cgebal(LAPACK_COL_MAJOR, JOB, N, pzB, N, &ILO, &IHI, ptrScale);
    if (INFO < 0) {
        Error(_("Invalid arguments for LAPACKE_dgebal"));
    }

    ArrayOfVector retval;

    if (nLhs <= 1) {
        retval << B;
        return retval;
    }

    if (nLhs == 2) {
        retval << ArrayOf::diagonalConstructor(scalingVector<single>(ptrScale, N, ILO, IHI), 0);
        retval << B;
    } else {
        retval << scalingVector<single>(ptrScale, N, ILO, IHI);
        retval << permuteVector(ptrScale, N, IHI);
        retval << B;
    }
    return retval;
}
//=============================================================================
}
//=============================================================================
