//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// busAssignment: replaces selected members of a bus and passes the rest
// through. Port 0 is the base bus (its type sets the output bus type); ports
// 1..N carry replacement signals, one per path in AssignedSignals. The output
// is a bus of the same type: a full copy of the base, with each assigned
// member's packed region overwritten by the matching replacement input. The
// mirror image of busSelector; member offsets come from SimCtx::busRegistry and
// all storage lanes (real / imaginary / int64) are preserved.
//=============================================================================
#include "routing_blocks.hpp"
#include "SimEngineBusTypes.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
#include <string>
#include <vector>
//=============================================================================
namespace {
// Assigned member paths from the AssignedSignals parameter (port k+1 replaces
// path k). Falls back to SelectedSignals so a diagram may reuse either key.
std::vector<std::string>
assignedPaths(const Nelson::NFlow::Block& b)
{
    std::vector<std::string> paths;
    const char* keys[] = { "AssignedSignals", "SelectedSignals" };
    for (const char* key : keys) {
        if (b.params.contains(key) && b.params[key].is_array()) {
            for (const auto& s : b.params[key]) {
                if (s.is_string()) {
                    paths.push_back(s.get<std::string>());
                }
            }
            if (!paths.empty()) {
                break;
            }
        }
    }
    return paths;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleBusAssignment(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    int busId = -1;
    if (ctx.inputSigs && ctx.sigs && b.nid < (int)ctx.inputSigs->size()) {
        const auto& sIds = (*ctx.inputSigs)[b.nid];
        if (!sIds.empty() && sIds[0] >= 0 && sIds[0] < (int)ctx.sigs->size()) {
            busId = (*ctx.sigs)[sIds[0]].busTypeId;
        }
    }
    if (busId < 0 || !ctx.busRegistry) {
        return false;
    }
    SigView base = getInputSig(ctx, b.nid, 0);
    double* y = outputSlice(ctx, b.nid, 0);
    double* yim = outputSliceImag(ctx, b.nid, 0);
    long long* yi = outputSliceI64(ctx, b.nid, 0);
    const int outW = outputWidth(ctx, b.nid, 0);
    bool changed = false;

    for (int k = 0; k < outW; ++k) {
        const double val = sigAt(base, k);
        if (y[k] != val) {
            y[k] = val;
            changed = true;
        }
        if (yim) {
            const double iv = (base.imag && base.width > 0)
                ? ((base.width == 1) ? base.imag[0] : base.imag[k])
                : 0.0;
            if (yim[k] != iv) {
                yim[k] = iv;
                changed = true;
            }
        }
        if (yi) {
            const long long ev = base.idata ? ((base.width == 1) ? base.idata[0] : base.idata[k])
                                            : static_cast<long long>(val);
            if (yi[k] != ev) {
                yi[k] = ev;
                changed = true;
            }
        }
    }

    const std::vector<std::string> paths = assignedPaths(b);
    for (size_t p = 0; p < paths.size(); ++p) {
        const int off = ctx.busRegistry->memberOffset(busId, paths[p]);
        const PortSig* ms = ctx.busRegistry->resolvePath(busId, paths[p]);
        if (off < 0 || !ms) {
            continue; // unknown path: rejected at compile time, defensive here
        }
        const int wo
            = (ms->busTypeId >= 0) ? ctx.busRegistry->packedWidth(ms->busTypeId) : ms->width();
        SigView rv = getInputSig(ctx, b.nid, static_cast<int>(p) + 1);
        for (int k = 0; k < wo && off + k < outW; ++k) {
            const double val = sigAt(rv, k);
            if (y[off + k] != val) {
                y[off + k] = val;
                changed = true;
            }
            if (yim) {
                const double iv
                    = (rv.imag && rv.width > 0) ? ((rv.width == 1) ? rv.imag[0] : rv.imag[k]) : 0.0;
                if (yim[off + k] != iv) {
                    yim[off + k] = iv;
                    changed = true;
                }
            }
            if (yi) {
                const long long ev = rv.idata ? ((rv.width == 1) ? rv.idata[0] : rv.idata[k])
                                              : static_cast<long long>(val);
                if (yi[off + k] != ev) {
                    yi[off + k] = ev;
                    changed = true;
                }
            }
        }
    }
    return changed;
}
//=============================================================================
bool
Nelson::NFlow::resolveBusAssignmentDims(const Block& /*b*/, const ValMap& /*vars*/,
    const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, BusRegistry& /*busReg*/,
    std::string& /*err*/)
{
    // The output keeps the shape and type of the base bus (port 0). Copy its
    // descriptor but keep the output's own packed offset; leave the output
    // as-is until the base bus resolves.
    const int busId = inSigs.empty() ? -1 : inSigs[0].busTypeId;
    if (busId < 0 || outSigs.empty()) {
        return true;
    }
    const int keepOffset = outSigs[0].offset;
    outSigs[0] = inSigs[0];
    outSigs[0].offset = keepOffset;
    return true;
}
//=============================================================================
