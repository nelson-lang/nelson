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
#include <complex>
#include "Types.hpp"
#include "omp_for_loop.hpp"
// lapack_eigen_config.hpp is what defines _NLS_WITH_VML (MSVC x64 with MKL/VML).
// Include it unconditionally and before the test, so the vectorized path is
// selected consistently regardless of the including translation unit's own
// include order.
#include "lapack_eigen_config.hpp"
#if defined(_NLS_WITH_VML)
#include <Eigen/Dense>
#endif
//=============================================================================
namespace Nelson::TypeTraits {
//=============================================================================
// Dense elementwise map with a vectorized (VML/Eigen) hook.
//
// Many unary math kernels (cos, sin, exp, ...) were hand-written four times --
// once per {single, double} x {real, complex} -- each copy carrying the same
// `#if defined(_NLS_WITH_VML) Eigen array path #else OpenMP scalar loop #endif`
// skeleton. These helpers factor that skeleton out: the caller supplies a
// vectorized functor (Eigen Array expression -> Array expression) and a scalar
// functor (one element -> one element). Under _NLS_WITH_VML the Eigen path runs
// exactly as the hand-written code did, so there is no change in numerical
// result or performance; otherwise the OpenMP scalar loop runs.
//
//   TypeTraits::mapRealDense<double>(in, out, n,
//       [](const auto& a) { return a.cos(); },   // vectorized
//       [](double x) { return std::cos(x); });    // scalar fallback
//=============================================================================
/**
 * Map a dense real array of element type T into a destination of the same type
 * and length.
 */
template <class T, class VectorFn, class ScalarFn>
inline void
mapRealDense(const T* in, T* out, ompIndexType n, VectorFn vectorFn, ScalarFn scalarFn)
{
#if defined(_NLS_WITH_VML)
    Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> source(in, n);
    Eigen::Map<Eigen::Array<T, Eigen::Dynamic, 1>> destination(out, n);
    destination = vectorFn(source);
#else
    (void)vectorFn;
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType k = 0; k < n; ++k) {
        out[k] = scalarFn(in[k]);
    }
#endif
}
//=============================================================================
/**
 * Map a dense complex array of element type std::complex<T> into a destination
 * of the same type and length.
 */
template <class T, class VectorFn, class ScalarFn>
inline void
mapComplexDense(const std::complex<T>* in, std::complex<T>* out, ompIndexType n, VectorFn vectorFn,
    ScalarFn scalarFn)
{
#if defined(_NLS_WITH_VML)
    Eigen::Map<const Eigen::Array<std::complex<T>, Eigen::Dynamic, 1>> source(in, n);
    Eigen::Map<Eigen::Array<std::complex<T>, Eigen::Dynamic, 1>> destination(out, n);
    destination = vectorFn(source);
#else
    (void)vectorFn;
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType k = 0; k < n; ++k) {
        out[k] = scalarFn(in[k]);
    }
#endif
}
//=============================================================================
} // namespace Nelson::TypeTraits
//=============================================================================
