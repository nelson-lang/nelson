//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// lookupDynamic (Lookup Table Dynamic): a 1-D linearly interpolated lookup whose
// breakpoint and table data come from input ports instead of parameters, so the
// table can change at run time. Port 0 = value x; port 1 = breakpoint vector
// xdat (strictly increasing); port 2 = table vector ydat (same length). Output =
// linear interpolation of (xdat, ydat) at x, clipped outside the range.
// Feedthrough (ALGEBRAIC), real double, scalar output. Native runtime only: no
// code generation for the run-time vector table.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "lookup_blocks.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Read the table (port 2) with breakpoint indices (0..n-1, n = breakpoint
    // width). The index is clamped so a saved model that wires a table port
    // narrower than the breakpoint vector cannot read past the table region.
    // A width-1 table still broadcasts through sigAt().
    static inline double
    lookupTableAt(const SigView& yd, int i)
    {
        if (yd.width > 1 && i >= yd.width) {
            i = yd.width - 1;
        }
        return sigAt(yd, i);
    }
    //=============================================================================
    bool
    handleLookupDynamic(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!hasInput(ctx, b.nid, 0) || !hasInput(ctx, b.nid, 1) || !hasInput(ctx, b.nid, 2)) {
            return false;
        }
        const double x = getInput(ctx, b.nid, 0, 0.0);
        SigView xd = getInputSig(ctx, b.nid, 1);
        SigView yd = getInputSig(ctx, b.nid, 2);
        const int n = xd.width;
        double out = 0.0;
        if (n <= 0) {
            out = 0.0;
        } else if (n == 1) {
            out = lookupTableAt(yd, 0);
        } else if (x <= sigAt(xd, 0)) {
            out = lookupTableAt(yd, 0);
        } else if (x >= sigAt(xd, n - 1)) {
            out = lookupTableAt(yd, n - 1);
        } else {
            int i = n - 2;
            while (i > 0 && x < sigAt(xd, i)) {
                --i;
            }
            const double x0 = sigAt(xd, i);
            const double x1 = sigAt(xd, i + 1);
            const double d = x1 - x0;
            const double f = (d != 0.0) ? (x - x0) / d : 0.0;
            const double y0 = lookupTableAt(yd, i);
            const double y1 = lookupTableAt(yd, i + 1);
            out = y0 + f * (y1 - y0);
        }
        setOutput(ctx, b.nid, out);
        return false;
    }
    //=============================================================================
    bool
    resolveLookupDynamicDims(const Block& b, const ValMap& vars, const std::vector<PortSig>&,
        std::vector<PortSig>& outSigs, std::string&)
    {
        (void)b;
        (void)vars;
        outSigs[0].setVector(1);
        outSigs[0].type = SigType::Double;
        return true;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
