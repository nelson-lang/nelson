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
doubleEigenDecompositionSymmetric(int n, double* v, double* d, double* a, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleEigenDecompositionSymmetric(int n, float* v, float* d, float* a, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleComplexEigenDecompositionSymmetric(
    int n, std::complex<float>* v, float* d, std::complex<float>* a, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
doubleComplexEigenDecompositionSymmetric(
    int n, std::complex<double>* v, double* d, std::complex<double>* a, bool eigenvectors);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d, float* a,
    bool eigenvectors, bool balance);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
doubleEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d, double* a,
    bool eigenvectors, bool balance);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
singleComplexEigenDecomposition(int n, std::complex<float>* v, std::complex<float>* d,
    std::complex<float>* a, bool eigenvectors, bool balance);
//=============================================================================
NLSLINEAR_ALGEBRA_IMPEXP void
doubleComplexEigenDecomposition(int n, std::complex<double>* v, std::complex<double>* d,
    std::complex<double>* a, bool eigenvectors, bool balance);
//=============================================================================
}
//=============================================================================
