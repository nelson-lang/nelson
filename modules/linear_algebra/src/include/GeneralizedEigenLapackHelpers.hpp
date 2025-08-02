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
#include <string>
#include <complex>
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleGeneralizedEigenDecomposition(
    int n, float* v, float* d, float* a, float* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
singleGeneralizedEigenDecompositionSymmetric(
    int n, float* v, float* d, float* a, float* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
doubleGeneralizedEigenDecomposition(
    int n, double* v, double* d, double* a, double* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
doubleGeneralizedEigenDecompositionSymmetric(
    int n, double* v, double* d, double* a, double* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleComplexGeneralizedEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d,
    std::complex<float>* a, std::complex<float>* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
singleComplexGeneralizedEigenDecompositionSymmetric(int n, std::complex<float>* v, float* d,
    std::complex<float>* a, std::complex<float>* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP bool
doubleComplexGeneralizedEigenDecompositionSymmetric(int n, std::complex<double>* v, double* d,
    std::complex<double>* a, std::complex<double>* b, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
doubleComplexGeneralizedEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d,
    std::complex<double>* a, std::complex<double>* b, bool eigenvectors);
//=============================================================================
}
//=============================================================================
