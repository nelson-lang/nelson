//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// interpolationPrelookup (Interpolation Using Prelookup): interpolates a static
// Table using a precomputed index/fraction pair produced by a prelookup block.
// The index k comes on port 0 and the fraction f on port 1 (a 2-element vector
// [k, f] on port 0 is also accepted); the output is
//   tbl[k] + f * (tbl[k+1] - tbl[k]),
// i.e. linear interpolation over the Table at the shared interval. k is clamped
// to a valid table index. Sharing one prelookup across several of these blocks
// avoids repeating the interval search per table. Feedthrough (ALGEBRAIC), real
// double, scalar output. Native runtime only.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include <string>
#include <vector>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleInterpolationPrelookup(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0)) {
            return false;
        }
        nflow::BlockDescriptor bd(b, ctx.variables);
        const std::vector<double> tbl = bd.paramList("Table");
        const int m = (int)tbl.size();
        SigView u = getInputSig(ctx, b.nid, 0);
        double kd = sigAt(u, 0);
        double f = 0.0;
        if (numInputs(ctx, b.nid) > 1 && hasInput(ctx, b.nid, 1)) {
            f = getInput(ctx, b.nid, 1, 0.0);
        } else if (u.width > 1) {
            f = sigAt(u, 1);
        }
        double out = 0.0;
        if (m == 1) {
            out = tbl[0];
        } else if (m >= 2) {
            int k = (int)std::floor(kd + 0.5);
            if (k < 0) {
                k = 0;
            }
            if (k > m - 2) {
                k = m - 2;
            }
            out = tbl[k] + f * (tbl[k + 1] - tbl[k]);
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    bool
    resolveInterpolationPrelookupDims(const Block& b, const ValMap& vars,
        const std::vector<PortSig>&, std::vector<PortSig>& outSigs, std::string& err)
    {
        nflow::BlockDescriptor bd(b, vars);
        const std::vector<double> tbl = bd.paramList("Table");
        if (tbl.empty()) {
            err = "interpolationPrelookup requires a non-empty Table";
            return false;
        }
        outSigs[0].setVector(1);
        outSigs[0].type = SigType::Double;
        return true;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
