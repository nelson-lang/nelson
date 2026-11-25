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
//=============================================================================
namespace Nelson::TypeTraits {
//=============================================================================
// Complex arrays store their elements as an interleaved (real, imaginary)
// sequence, which is layout-compatible with std::complex<T>. Many kernels
// therefore reinterpret the raw payload pointer of a complex ArrayOf as a
// std::complex<T>*. These helpers give that reinterpretation a single named,
// documented spot instead of a hand-written reinterpret_cast repeated at every
// call site. They are inline and compile to the same code as the cast.
//=============================================================================
/**
 * View a raw complex payload as a writable std::complex<T> array.
 *
 * `data` is the real-valued payload pointer of a complex ArrayOf of element
 * type T (a void*, T*, or equivalent): the elements are stored interleaved as
 * real, imaginary, real, imaginary, ...
 */
template <class T>
inline std::complex<T>*
asComplex(void* data) noexcept
{
    return reinterpret_cast<std::complex<T>*>(data);
}
//=============================================================================
/**
 * View a raw complex payload as a read-only std::complex<T> array.
 */
template <class T>
inline const std::complex<T>*
asComplex(const void* data) noexcept
{
    return reinterpret_cast<const std::complex<T>*>(data);
}
//=============================================================================
} // namespace Nelson::TypeTraits
//=============================================================================
