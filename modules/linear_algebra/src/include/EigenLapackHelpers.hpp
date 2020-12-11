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
