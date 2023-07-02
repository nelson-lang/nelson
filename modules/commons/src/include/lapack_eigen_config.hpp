//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if defined(_MSC_VER)
#define EIGEN_USE_MKL
#define EIGEN_USE_MKL_VML
#define _NLS_WITH_VML
#endif
//=============================================================================
#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE_STRICT
//=============================================================================
#include <complex>
//=============================================================================
#pragma warning(disable : 4190)
#ifndef lapack_complex_float
#ifndef __INTEL_COMPILER
#define lapack_complex_float std::complex<float>
#endif
#endif
//=============================================================================
#ifndef lapack_complex_double
#ifndef __INTEL_COMPILER
#define lapack_complex_double std::complex<double>
#endif
#endif
//=============================================================================
