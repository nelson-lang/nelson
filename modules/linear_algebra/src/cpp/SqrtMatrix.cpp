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
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <unsupported/Eigen/MatrixFunctions>
#include "SqrtMatrix.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SchurDecomposition.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
sqrtmComplex(ArrayOf& A)
{
    const auto* inputData = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    for (indexType k = 0; k < A.elementCount(); ++k) {
        if (!std::isfinite(static_cast<double>(inputData[k].real()))
            || !std::isfinite(static_cast<double>(inputData[k].imag()))) {
            Error("Nelson:linear_algebra:inputNotFinite", "Input must be finite.");
        }
    }
    ArrayOf U;
    ArrayOf Tschur;
    SchurDecomposition(A, true, U, Tschur);
    NelsonType complexClass = sizeof(T) == sizeof(single) ? NLS_SCOMPLEX : NLS_DCOMPLEX;
    U.promoteType(complexClass);
    Tschur.promoteType(complexClass);
    ArrayOf R(U);
    R.ensureSingleOwner();
    using MatrixType = Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>;
    const auto* up = reinterpret_cast<const std::complex<T>*>(U.dataPointer());
    const auto* tp = reinterpret_cast<const std::complex<T>*>(Tschur.dataPointer());
    auto* rp = reinterpret_cast<std::complex<T>*>(const_cast<void*>(R.dataPointer()));
    Eigen::Map<const MatrixType> matU(up, (Eigen::Index)U.rows(), (Eigen::Index)U.columns());
    Eigen::Map<const MatrixType> matT(
        tp, (Eigen::Index)Tschur.rows(), (Eigen::Index)Tschur.columns());
    Eigen::Map<MatrixType> matR(rp, (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
    bool isSingular = false;
    for (Eigen::Index i = 0; i < matT.rows(); ++i) {
        if (matT(i, i) == std::complex<T>((T)0, (T)0)) {
            isSingular = true;
            break;
        }
    }
    MatrixType sqrtT = MatrixType::Zero(matT.rows(), matT.cols());
    Eigen::matrix_sqrt_triangular(matT, sqrtT);
    matR = matU * sqrtT * matU.adjoint();
    if (isSingular) {
        Warning(
            "Nelson:sqrtm:SingularMatrix", "Matrix is singular and may not have a square root.");
    }
    return R;
}
//=============================================================================
ArrayOf
SqrtMatrix(ArrayOf A)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
                                || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX)
        && !A.isSparse();
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "sqrtm", ClassName(A));
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    if (A.isEmpty()) {
        ArrayOf res(A);
        res.ensureSingleOwner();
        return res;
    }
    ArrayOf res;
    if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
        if (A.dataClass() == NLS_DOUBLE) {
            A.promoteType(NLS_DCOMPLEX);
        }
        res = sqrtmComplex<double>(A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        if (A.dataClass() == NLS_SINGLE) {
            A.promoteType(NLS_SCOMPLEX);
        }
        res = sqrtmComplex<single>(A);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
