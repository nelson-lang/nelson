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
#include "SimEngineTypes.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Resolved MIMO state-space matrices (row-major) with their dimensions.
    // A is nx x nx, B is nx x nu, C is ny x nx, D is ny x nu. A scalar param
    // (SISO) yields nx = nu = ny = 1. Shared by the continuous state space and
    // its discrete counterpart so the two read a model the same way.
    //=============================================================================
    struct SsMatrices
    {
        int nx = 1, nu = 1, ny = 1;
        std::vector<double> A, B, C, D;
    };
    //=============================================================================
    // Number of states: A is square, so nx = sqrt(len(A)).
    inline int
    ssStateCountOf(const Block& b, const ValMap& vars)
    {
        nflow::BlockDescriptor bd(b, vars);
        const std::vector<double> a = bd.paramList(nflow::kA);
        if (a.empty()) {
            return 1;
        }
        return (std::max)(1, (int)std::llround(std::sqrt((double)a.size())));
    }
    //=============================================================================
    // Output width: ny = rows(C) = len(C) / nx.
    inline int
    ssOutputWidthOf(const Block& b, const ValMap& vars)
    {
        nflow::BlockDescriptor bd(b, vars);
        const int nx = ssStateCountOf(b, vars);
        const std::vector<double> c = bd.paramList(nflow::kC);
        if (c.empty() || nx <= 0) {
            return 1;
        }
        return (std::max)(1, (int)c.size() / nx);
    }
    //=============================================================================
    // Initial state. A scalar broadcasts over the nx states, a list gives them
    // one by one, an absent parameter keeps all zeros.
    inline std::vector<double>
    ssInitialStateOf(const nflow::BlockDescriptor& bd, int nx)
    {
        std::vector<double> out((size_t)(std::max)(0, nx), 0.0);
        const std::vector<double> x0 = bd.paramList(nflow::kInitial);
        if (x0.empty()) {
            return out;
        }
        for (int i = 0; i < nx; ++i) {
            out[(size_t)i] = (i < (int)x0.size()) ? x0[(size_t)i] : x0.back();
        }
        return out;
    }
    //=============================================================================
    // Read A/B/C/D and pad the missing ones with zeros, so index arithmetic
    // can never overrun whatever the model provided.
    inline SsMatrices
    ssBuild(const Block& b, const ValMap& vars, int nu)
    {
        nflow::BlockDescriptor bd(b, vars);
        SsMatrices m;
        m.A = bd.paramList(nflow::kA);
        m.B = bd.paramList(nflow::kB);
        m.C = bd.paramList(nflow::kC);
        m.D = bd.paramList(nflow::kD);
        if (m.A.empty()) {
            m.A = { 0.0 };
        }
        m.nx = (std::max)(1, (int)std::llround(std::sqrt((double)m.A.size())));
        m.nu = (std::max)(1, nu);
        m.ny = m.C.empty() ? 1 : (std::max)(1, (int)m.C.size() / m.nx);
        m.A.resize((size_t)m.nx * m.nx, 0.0);
        m.B.resize((size_t)m.nx * m.nu, 0.0);
        m.C.resize((size_t)m.ny * m.nx, 0.0);
        m.D.resize((size_t)m.ny * m.nu, 0.0);
        return m;
    }
    //=============================================================================
    // True when every matrix is a single number (SISO form), which the discrete
    // block applies element-wise across the signal width.
    inline bool
    ssIsScalarForm(const Block& b, const ValMap& vars)
    {
        nflow::BlockDescriptor bd(b, vars);
        for (const char* key : { nflow::kA, nflow::kB, nflow::kC, nflow::kD }) {
            if (bd.paramList(key).size() > 1) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    // y = C x + D u, with u read through a SigView.
    inline void
    ssEmit(const SsMatrices& m, const double* x, const SigView& u, double* y)
    {
        for (int i = 0; i < m.ny; ++i) {
            double acc = 0.0;
            for (int j = 0; j < m.nx; ++j) {
                acc += m.C[(size_t)i * m.nx + j] * x[j];
            }
            for (int j = 0; j < m.nu; ++j) {
                acc += m.D[(size_t)i * m.nu + j] * sigAt(u, j);
            }
            y[i] = acc;
        }
    }
    //=============================================================================
    // The state one step on: A x + B u.
    inline void
    ssAdvance(const SsMatrices& m, const double* x, const SigView& u, double* xnext)
    {
        for (int i = 0; i < m.nx; ++i) {
            double acc = 0.0;
            for (int j = 0; j < m.nx; ++j) {
                acc += m.A[(size_t)i * m.nx + j] * x[j];
            }
            for (int j = 0; j < m.nu; ++j) {
                acc += m.B[(size_t)i * m.nu + j] * sigAt(u, j);
            }
            xnext[i] = acc;
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
