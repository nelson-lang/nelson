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
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "MatrixCosinus.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
template <class T>
ArrayOf
cosmComplex(const ArrayOf& A)
{
    T* ptrR = (T*)ArrayOf::allocateArrayOf(
        A.getDataClass(), A.getElementCount(), stringVector(), false);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)ptrR);
    Dimensions dimsA = A.getDimensions();
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)dimsA.getRows(), (Eigen::Index)dimsA.getColumns());
    // [V, D] = eig(A);
    // cosm = V * diag(cos(diag(D))) * inv(V);
    Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>>
        solver(matA.template cast<std::complex<T>>());
    auto evects = solver.eigenvectors();
    auto evals = solver.eigenvalues();
    OMP_PARALLEL_FOR_LOOP(evals.rows())
    for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
        evals(i) = cos(evals(i));
    }
    auto evalsdiag = evals.asDiagonal();
    matR = evects * evalsdiag * evects.inverse();
    return ArrayOf(A.getDataClass(), A.getDimensions(), ptrR);
}
//=============================================================================
ArrayOf
MatrixCos(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return {};
    }
    switch (A.getDataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R = cosmComplex<single>(A);
        if (R.allReal()) {
            R.promoteType(NLS_SINGLE);
        }
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.promoteType(NLS_SCOMPLEX);
        R = cosmComplex<single>(R);
        R.promoteType(NLS_SINGLE);
        return R;
    } break;
    case NLS_DCOMPLEX: {
        ArrayOf R = cosmComplex<double>(A);
        if (R.allReal()) {
            R.promoteType(NLS_DOUBLE);
        }
        return R;
    } break;
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.promoteType(NLS_DCOMPLEX);
        R = cosmComplex<double>(R);
        R.promoteType(NLS_DOUBLE);
        return R;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
