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
#include <cmath>
#include <cstdint>
#include "ziggurat_normal_tables.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Standard normal deviate by the 256-layer ziggurat of the 'twister' engine.
// nextWords(w0, w1) supplies two 32-bit words: a 53-bit signed value drives the
// fast path. The wedge and the exponential tail draw uniform() values in (0, 1).
// Nothing is cached between calls, so the engine state alone fixes the sequence.
template <typename NextWords, typename Uniform>
inline double
zigguratNormal(NextWords nextWords, Uniform uniform)
{
    for (;;) {
        uint32_t w0 = 0;
        uint32_t w1 = 0;
        nextWords(w0, w1);
        uint32_t iz = w1 >> 24;
        uint64_t m53
            = (static_cast<uint64_t>(w0 >> 3) << 24) | static_cast<uint64_t>(w1 & 0xFFFFFF);
        double x = (static_cast<double>(m53) - 4503599627370496.0) / 2097152.0;
        if (std::fabs(x) < static_cast<double>(ZIG_NORM_KN[iz])) {
            return x * ZIG_NORM_WN[iz];
        }
        if (iz == 255) {
            /* Sample from the exponential tail (Marsaglia rejection). */
            double xtail, ytail;
            do {
                xtail = -std::log(uniform()) * ZIG_NORM_RINV;
                ytail = -std::log(uniform());
            } while (ytail + ytail < xtail * xtail);
            return (x > 0.0) ? (ZIG_NORM_R + xtail) : -(ZIG_NORM_R + xtail);
        }
        /* Wedge acceptance test against the normal density. */
        double xval = x * ZIG_NORM_WN[iz];
        int prev = (iz == 0) ? 255 : static_cast<int>(iz - 1);
        if (ZIG_NORM_FN[iz] + uniform() * (ZIG_NORM_FN[prev] - ZIG_NORM_FN[iz])
            < std::exp(-0.5 * xval * xval)) {
            return xval;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
