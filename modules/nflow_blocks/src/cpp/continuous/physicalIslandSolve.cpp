//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared numerical helpers of the physical island: the dense Newton linear
// solve and the latched-mode logic of the ideal diode (knee gap + settle).
// See physicalIsland.cpp (handler) and physicalIslandShared.hpp (types).
//=============================================================================
#include "physicalIslandShared.hpp"
#include <cmath>
#include <utility>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace physisland {
        //=============================================================================
        void
        islandGaussSolve(std::vector<double>& J, std::vector<double>& bv, int N)
        {
            for (int k = 0; k < N; ++k) {
                int p = k;
                double mx = std::fabs(J[static_cast<size_t>(k) * N + k]);
                for (int r = k + 1; r < N; ++r) {
                    const double v = std::fabs(J[static_cast<size_t>(r) * N + k]);
                    if (v > mx) {
                        mx = v;
                        p = r;
                    }
                }
                if (p != k) {
                    for (int col = 0; col < N; ++col) {
                        std::swap(J[static_cast<size_t>(k) * N + col],
                            J[static_cast<size_t>(p) * N + col]);
                    }
                    std::swap(bv[static_cast<size_t>(k)], bv[static_cast<size_t>(p)]);
                }
                double d = J[static_cast<size_t>(k) * N + k];
                if (d < 1e-30 && d > -1e-30) {
                    d = 1e-30;
                }
                for (int r = k + 1; r < N; ++r) {
                    const double f = J[static_cast<size_t>(r) * N + k] / d;
                    for (int col = k; col < N; ++col) {
                        J[static_cast<size_t>(r) * N + col]
                            -= f * J[static_cast<size_t>(k) * N + col];
                    }
                    bv[static_cast<size_t>(r)] -= f * bv[static_cast<size_t>(k)];
                }
            }
            for (int i = N - 1; i >= 0; --i) {
                double s = bv[static_cast<size_t>(i)];
                for (int col = i + 1; col < N; ++col) {
                    s -= J[static_cast<size_t>(i) * N + col] * bv[static_cast<size_t>(col)];
                }
                double d = J[static_cast<size_t>(i) * N + i];
                if (d < 1e-30 && d > -1e-30) {
                    d = 1e-30;
                }
                bv[static_cast<size_t>(i)] = s / d;
            }
        }
        //=============================================================================
        double
        islandDiodeKneeGap(const IslandLayout* L, const IslandComp& c, const double* x,
            const std::vector<double>& fallback)
        {
            auto at = [&](int si) -> double {
                if (si < 0) {
                    return 0.0;
                }
                if (x) {
                    return x[si];
                }
                return (si < (int)fallback.size()) ? fallback[static_cast<size_t>(si)] : 0.0;
            };
            auto ePot = [&](int node) -> double {
                if (node <= 0 || node >= (int)L->eIndex.size()) {
                    return 0.0;
                }
                return at(L->eIndex[static_cast<size_t>(node)]);
            };
            return ePot(c.a) - ePot(c.b) - c.value3;
        }
        //=============================================================================
        bool
        islandSettleDiodeModes(
            IslandLayout* L, const double* x, const std::vector<double>& fallback)
        {
            bool changed = false;
            for (size_t ci = 0; ci < L->comps.size() && ci < L->diodeMode.size(); ++ci) {
                const IslandComp& c = L->comps[ci];
                if (c.kind != Kind::IdealDiode) {
                    continue;
                }
                const double gap = islandDiodeKneeGap(L, c, x, fallback);
                int& mode = L->diodeMode[ci];
                if (mode == 0 && gap > kDiodeKneeTolV) {
                    mode = 1;
                    changed = true;
                } else if (mode == 1 && gap < -kDiodeKneeTolV) {
                    mode = 0;
                    changed = true;
                }
            }
            return changed;
        }
        //=============================================================================
    } // namespace physisland
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
