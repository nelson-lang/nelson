//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// prelookup (Prelookup): computes the interval index k and the fraction f for a
// scalar input over a strictly increasing breakpoint vector, so several tables
// can share one interval search via interpolationPrelookup. The index k leaves
// on port 0 and the fraction f on port 1, where bp[k] <= u < bp[k+1] and
// f = (u - bp[k]) / (bp[k+1] - bp[k]). Out-of-range inputs are clipped: below
// the first breakpoint -> [0, 0]; at/above the last -> [N-2, 1]. Feedthrough
// (ALGEBRAIC), real double, no state. Native runtime only.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        int
        preFindInterval(const std::vector<double>& bp, double u)
        {
            const int n = (int)bp.size();
            for (int i = n - 2; i >= 0; --i) {
                if (u >= bp[i]) {
                    return i;
                }
            }
            return 0;
        }
        //=============================================================================
        bool
        preIsStrictlyIncreasing(const std::vector<double>& v)
        {
            for (size_t i = 1; i < v.size(); ++i) {
                if (!(v[i] > v[i - 1])) {
                    return false;
                }
            }
            return true;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    bool
    handlePrelookup(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> bp = bd.paramList("BreakpointsForDimension1");
        const double u = getInput(ctx, b.nid, 0, 0.0);
        const int n = (int)bp.size();
        double k = 0.0, f = 0.0;
        if (n >= 2) {
            if (u <= bp[0]) {
                k = 0.0;
                f = 0.0;
            } else if (u >= bp[n - 1]) {
                k = (double)(n - 2);
                f = 1.0;
            } else {
                const int i = preFindInterval(bp, u);
                k = (double)i;
                const double d = bp[i + 1] - bp[i];
                f = (d != 0.0) ? (u - bp[i]) / d : 0.0;
            }
        }
        setOutput(ctx, b.nid, k);
        if (numOutputs(ctx, b.nid) > 1) {
            double* f1 = outputSlice(ctx, b.nid, 1);
            if (f1 != nullptr) {
                f1[0] = f;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    resolvePrelookupDims(const Block& b, const ValMap& vars, const std::vector<PortSig>&,
        std::vector<PortSig>& outSigs, std::string& err)
    {
        nflow::BlockDescriptor bd(b, vars);
        const std::vector<double> bp = bd.paramList("BreakpointsForDimension1");
        if (bp.size() < 2) {
            err = "prelookup requires at least 2 breakpoints";
            return false;
        }
        if (!preIsStrictlyIncreasing(bp)) {
            err = "prelookup breakpoints must be strictly increasing";
            return false;
        }
        for (size_t i = 0; i < outSigs.size() && i < 2; ++i) {
            outSigs[i].setVector(1);
            outSigs[i].type = SigType::Double;
        }
        return true;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
