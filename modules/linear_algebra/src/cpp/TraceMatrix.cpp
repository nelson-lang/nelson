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
#include "TraceMatrix.hpp"
#include "ClassName.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static T
sparseTrace(const ArrayOf& A)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    signedIndexType n = static_cast<signedIndexType>(std::min(A.rows(), A.columns()));
    T value(0);
    for (signedIndexType k = 0; k < n; ++k) {
        value += matrix->coeff(k, k);
    }
    return value;
}
//=============================================================================
ArrayOf
TraceMatrix(const ArrayOf& A)
{
    bool isSupportedTypes = (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_SINGLE
        || A.dataClass() == NLS_DCOMPLEX || A.dataClass() == NLS_SCOMPLEX);
    if (!isSupportedTypes) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", "trace", ClassName(A));
    }
    if (!A.isSquare()) {
        Error(L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected.", L"Square matrix");
    }
    if (A.isEmpty()) {
        return ArrayOf::doubleScalar(0);
    }
    if (A.isSparse()) {
        switch (A.dataClass()) {
        case NLS_DOUBLE: {
            return ArrayOf::doubleScalar(sparseTrace<double>(A));
        }
        case NLS_SINGLE: {
            return ArrayOf::singleScalar(sparseTrace<single>(A));
        }
        case NLS_DCOMPLEX: {
            doublecomplex res = sparseTrace<doublecomplex>(A);
            return ArrayOf::dsingleComplexScalar(res.real(), res.imag());
        }
        case NLS_SCOMPLEX: {
            singlecomplex res = sparseTrace<singlecomplex>(A);
            return ArrayOf::singleComplexScalar(res.real(), res.imag());
        }
        default:
            Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
                "Undefined function '%s' for input arguments of type '%s'.", "trace", ClassName(A));
        }
    }
    if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
        if (A.dataClass() == NLS_DOUBLE) {
            Eigen::Map<Eigen::MatrixXd> matA(
                (double*)A.dataPointer(), (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
            double res = matA.trace();
            return ArrayOf::doubleScalar(res);
        } // NLS_DCOMPLEX

        auto* Az = reinterpret_cast<doublecomplex*>((single*)A.dataPointer());
        Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
        doublecomplex res = matA.trace();
        return ArrayOf::dsingleComplexScalar(res.real(), res.imag());
    }
    if (A.dataClass() == NLS_SINGLE) {
        Eigen::Map<Eigen::MatrixXf> matA(
            (single*)A.dataPointer(), (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
        return ArrayOf::singleScalar(matA.trace());
    } // NLS_SCOMPLEX
    auto* Az = reinterpret_cast<singlecomplex*>((single*)A.dataPointer());
    Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    singlecomplex res = matA.trace();
    return ArrayOf::singleComplexScalar(res.real(), res.imag());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
