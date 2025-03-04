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
#include "LogMatrix.hpp"
#include "ClassName.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
logmComplex(ArrayOf& A)
{
    ArrayOf R(A);
    R.ensureSingleOwner();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)R.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)A.getRows(), (Eigen::Index)A.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)R.getRows(), (Eigen::Index)R.getColumns());
    if (!matA.allFinite()) {
        Error(_("Input must be finite."));
    } else {
        // [V, D] = eig(A);
        // sqrtm = V * diag(log(diag(D))) * inv(V);
        Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>>
            solver(matA.template cast<std::complex<T>>());
        auto evects = solver.eigenvectors();
        auto evals = solver.eigenvalues();
        OMP_PARALLEL_FOR_LOOP(evals.rows())
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
            evals(i) = std::log(evals(i));
        }
        auto evalsdiag = evals.asDiagonal();
        matR = evects * evalsdiag * evects.inverse();
    }
    return R;
}
//=============================================================================
ArrayOf
LogMatrix(ArrayOf A)
{
    bool isSupportedTypes
        = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE
              || A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error(_("Undefined function 'sqrtm' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    if (!A.isSquare()) {
        Error(_("Square matrix expected."));
    }
    if (A.isEmpty()) {
        ArrayOf res(A);
        res.ensureSingleOwner();
        return res;
    }
    ArrayOf res;
    if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX) {
        if (A.getDataClass() == NLS_DOUBLE) {
            A.promoteType(NLS_DCOMPLEX);
        }
        res = logmComplex<double>(A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        if (A.getDataClass() == NLS_SINGLE) {
            A.promoteType(NLS_SCOMPLEX);
        }
        res = logmComplex<single>(A);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
