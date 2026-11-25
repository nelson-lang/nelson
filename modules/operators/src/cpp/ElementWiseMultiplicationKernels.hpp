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
// Parity-critical saturating integer multiply, isolated from the dispatch logic of
// ElementWiseMultiplication.cpp. Pure: depends only on <limits> and the scalar
// integer multiply primitive. The value equals scalar_scalar_integer_times for every
// input; the widened path exists only to auto-vectorize 8/16/32-bit products.
//=============================================================================
#include <limits>
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> struct WiderIntFor
{
    using type = T; // fallback (64-bit and non-integer): the wider path is unused
};
//=============================================================================
template <> struct WiderIntFor<int8>
{
    using type = int32;
};
//=============================================================================
template <> struct WiderIntFor<int16>
{
    using type = int32;
};
//=============================================================================
template <> struct WiderIntFor<int32>
{
    using type = int64;
};
//=============================================================================
template <> struct WiderIntFor<uint8>
{
    using type = uint32;
};
//=============================================================================
template <> struct WiderIntFor<uint16>
{
    using type = uint32;
};
//=============================================================================
template <> struct WiderIntFor<uint32>
{
    using type = uint64;
};
//=============================================================================
// Saturating integer multiply. For 8/16/32-bit types the exact product fits in a
// wider integer, so it is computed exactly and clamped, which auto-vectorizes. 64-bit
// types have no portable wider integer and use the overflow-checked helper. The value
// equals scalar_scalar_integer_times for every input.
template <class T>
inline T
saturating_integer_times(T a, T b)
{
    if constexpr (sizeof(T) <= 4) {
        using W = typename WiderIntFor<T>::type;
        W p = (W)a * (W)b;
        W lo = (W)std::numeric_limits<T>::min();
        W hi = (W)std::numeric_limits<T>::max();
        p = (p < lo) ? lo : p;
        p = (p > hi) ? hi : p;
        return (T)p;
    } else {
        return scalar_scalar_integer_times<T>(a, b);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
