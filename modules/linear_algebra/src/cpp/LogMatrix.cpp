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
#include <complex>
#include <cmath>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include "LogMatrix.hpp"
#include "ClassName.hpp"
#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4305)
#endif
#include <unsupported/Eigen/MatrixFunctions>
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
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
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.dataPointer());
    std::complex<T>* Rz = reinterpret_cast<std::complex<T>*>((T*)R.dataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(
        Az, (Eigen::Index)A.rows(), (Eigen::Index)A.columns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matR(
        Rz, (Eigen::Index)R.rows(), (Eigen::Index)R.columns());
    if (!matA.allFinite()) {
        Error("Nelson:linear_algebra:inputNotFinite", "Input must be finite.");
    } else {
        // Schur-based logarithm (Eigen MatrixFunctions): works for defective matrices too.
        // Evaluate into a temporary to avoid aliasing between matA and matR.
        Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic> tmp = matA.log();
        matR = tmp;
    }
    return R;
}
//=============================================================================
static ArrayOf
LogScalarMatrix(const ArrayOf& A)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        double v = A.asDoubleScalar();
        if (!std::isfinite(v)) {
            return ArrayOf::doubleScalar(std::nan(""));
        }
        if (v <= 0.) {
            RaiseWarning("Nelson:logm:nonPosRealEig",
                Nelson::resolveMessageTemplate(
                    "Nelson:linear_algebra:principalMatrixLogarithmDefined",
                    "Principal matrix logarithm is not defined for A with nonpositive real "
                    "eigenvalues. A non-principal matrix logarithm is returned."));
        }
        if (v < 0.) {
            std::complex<double> r = std::log(std::complex<double>(v, 0.));
            return ArrayOf::dsingleComplexScalar(r.real(), r.imag());
        }
        return ArrayOf::doubleScalar(std::log(v));
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> v = A.asDoubleComplexScalar();
        if (!std::isfinite(v.real()) || !std::isfinite(v.imag())) {
            return ArrayOf::doubleScalar(std::nan(""));
        }
        if (v.imag() == 0. && v.real() <= 0.) {
            RaiseWarning("Nelson:logm:nonPosRealEig",
                Nelson::resolveMessageTemplate(
                    "Nelson:linear_algebra:principalMatrixLogarithmDefined",
                    "Principal matrix logarithm is not defined for A with nonpositive real "
                    "eigenvalues. A non-principal matrix logarithm is returned."));
        }
        std::complex<double> r = std::log(v);
        if (r.imag() == 0.) {
            return ArrayOf::doubleScalar(r.real());
        }
        return ArrayOf::dsingleComplexScalar(r.real(), r.imag());
    } break;
    case NLS_SINGLE: {
        single v = A.asSingleScalar();
        if (!std::isfinite(v)) {
            return ArrayOf::singleScalar(std::nanf(""));
        }
        if (v <= 0.f) {
            RaiseWarning("Nelson:logm:nonPosRealEig",
                Nelson::resolveMessageTemplate(
                    "Nelson:linear_algebra:principalMatrixLogarithmDefined",
                    "Principal matrix logarithm is not defined for A with nonpositive real "
                    "eigenvalues. A non-principal matrix logarithm is returned."));
        }
        if (v < 0.f) {
            std::complex<single> r = std::log(std::complex<single>(v, 0.f));
            return ArrayOf::singleComplexScalar(r.real(), r.imag());
        }
        return ArrayOf::singleScalar(std::log(v));
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> v = A.asSingleComplexScalar();
        if (!std::isfinite(v.real()) || !std::isfinite(v.imag())) {
            return ArrayOf::singleScalar(std::nanf(""));
        }
        if (v.imag() == 0.f && v.real() <= 0.f) {
            RaiseWarning("Nelson:logm:nonPosRealEig",
                Nelson::resolveMessageTemplate(
                    "Nelson:linear_algebra:principalMatrixLogarithmDefined",
                    "Principal matrix logarithm is not defined for A with nonpositive real "
                    "eigenvalues. A non-principal matrix logarithm is returned."));
        }
        std::complex<single> r = std::log(v);
        if (r.imag() == 0.f) {
            return ArrayOf::singleScalar(r.real());
        }
        return ArrayOf::singleComplexScalar(r.real(), r.imag());
    } break;
    default: {
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
LogMatrix(ArrayOf A)
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
    if (A.elementCount() == 1) {
        // dedicated scalar path: avoids NaN pollution of the matrix product
        // with infinite intermediate values (e.g. logm(0) must return -Inf)
        return LogScalarMatrix(A);
    }
    ArrayOf res;
    if (A.dataClass() == NLS_DOUBLE || A.dataClass() == NLS_DCOMPLEX) {
        if (A.dataClass() == NLS_DOUBLE) {
            A.promoteType(NLS_DCOMPLEX);
        }
        res = logmComplex<double>(A);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        if (A.dataClass() == NLS_SINGLE) {
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
