//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ArrayOf.hpp"
#include "NelsonComplexStorage.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Matrix trigonometric functions (cosm, sinm, tanm) share the same algorithm:
//   [V, D] = eig(A); f(A) = V * diag(f(diag(D))) * inv(V)
// evaluated over the complex field. Only the scalar applied to each eigenvalue
// differs, so the three builtins reduce to one call to the helpers below.
//=============================================================================
/**
 * Apply a scalar complex function to the eigenvalues of a complex square matrix
 * and recompose: V * diag(scalarFn(eigenvalues)) * inv(V). T is the real
 * element type (single or double); A must already be of the matching complex
 * class.
 */
template <class T, class ScalarFn>
ArrayOf
matrixFunctionViaEig(const ArrayOf& A, ScalarFn scalarFn)
{
    T* ptrR = (T*)ArrayOf::allocateArrayOf(A.dataClass(), A.elementCount(), stringVector(), false);
    const std::complex<T>* Az = TypeTraits::asComplex<T>(A.dataPointer());
    std::complex<T>* Rz = TypeTraits::asComplex<T>(ptrR);
    Eigen::Map<const Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    Eigen::ComplexEigenSolver<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>>
        solver(matA.template cast<std::complex<T>>());
    auto evects = solver.eigenvectors();
    auto evals = solver.eigenvalues();
    OMP_PARALLEL_FOR_LOOP(evals.rows())
    for (ompIndexType i = 0; i < static_cast<ompIndexType>(evals.rows()); ++i) {
        evals(i) = scalarFn(evals(i));
    }
    auto evalsdiag = evals.asDiagonal();
    matR = evects * evalsdiag * evects.inverse();
    return ArrayOf(A.dataClass(), A.dimensions(), ptrR);
}
//=============================================================================
/**
 * Shared dispatch for the matrix trigonometric builtins: validates a square
 * dense matrix, promotes real input to complex, runs the eigenvalue function
 * and demotes the result back to real when it carries no imaginary part.
 * scalarFn maps one complex eigenvalue to its image.
 */
template <class ScalarFn>
ArrayOf
matrixTrigonometricDispatch(const ArrayOf& A, bool& needToOverload, ScalarFn scalarFn)
{
    needToOverload = false;
    if (!A.isSquare()) {
        Error("Nelson:trigonometric_functions:squareMatrixExpected", "Square matrix expected.");
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
    switch (A.dataClass()) {
    default: {
        needToOverload = true;
    } break;
    case NLS_SCOMPLEX: {
        ArrayOf R = matrixFunctionViaEig<single>(A, scalarFn);
        R.demoteIfReal();
        return R;
    }
    case NLS_SINGLE: {
        ArrayOf R(A);
        R.promoteType(NLS_SCOMPLEX);
        R = matrixFunctionViaEig<single>(R, scalarFn);
        R.promoteType(NLS_SINGLE);
        return R;
    }
    case NLS_DCOMPLEX: {
        ArrayOf R = matrixFunctionViaEig<double>(A, scalarFn);
        R.demoteIfReal();
        return R;
    }
    case NLS_DOUBLE: {
        ArrayOf R(A);
        R.promoteType(NLS_DCOMPLEX);
        R = matrixFunctionViaEig<double>(R, scalarFn);
        R.promoteType(NLS_DOUBLE);
        return R;
    }
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
