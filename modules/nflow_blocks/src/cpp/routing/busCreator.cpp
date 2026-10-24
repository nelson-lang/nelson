//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// busCreator: group input signals (or nested buses) into one bus. The output
// port owns a packed contiguous region; member p starts at the sum of the
// widths of members 0..p-1 (the registry layout mirrors the port order), so
// the runtime copy needs no registry lookup. All storage lanes are
// preserved.
//=============================================================================
#include "routing_blocks.hpp"
#include "SimEngineBusTypes.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
//=============================================================================
bool
Nelson::NFlow::handleBusCreator(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    const int nIn = numInputs(ctx, b.nid);
    const int wOut = outputWidth(ctx, b.nid, 0);
    double* y = outputSlice(ctx, b.nid, 0);
    double* yim = outputSliceImag(ctx, b.nid, 0);
    long long* yi = outputSliceI64(ctx, b.nid, 0);
    bool changed = false;
    int at = 0;
    for (int p = 0; p < nIn && at < wOut; ++p) {
        SigView v = getInputSig(ctx, b.nid, p);
        const int w = std::max(1, v.width);
        for (int k = 0; k < w && at < wOut; ++k, ++at) {
            const double val = sigAt(v, k);
            if (y[at] != val) {
                y[at] = val;
                changed = true;
            }
            if (yim) {
                const double iv
                    = (v.imag && v.width > 0) ? ((v.width == 1) ? v.imag[0] : v.imag[k]) : 0.0;
                if (yim[at] != iv) {
                    yim[at] = iv;
                    changed = true;
                }
            }
            if (yi) {
                const long long ev
                    = v.idata ? ((v.width == 1) ? v.idata[0] : v.idata[k]) : (long long)val;
                if (yi[at] != ev) {
                    yi[at] = ev;
                    changed = true;
                }
            }
        }
    }
    return changed;
}
//=============================================================================
// Dimension/type rule: build (or reuse) the bus type from the input
// descriptors. Deduplicated by structural equality so the fixed-point loop
// converges to a stable id.
bool
Nelson::NFlow::resolveBusCreatorDims(const Block& b, const ValMap& vars,
    const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, BusRegistry& busReg,
    std::string& err)
{
    nflow::BlockDescriptor bd(b, vars);
    const int nIn = std::max(1, b.inputs);
    // Member names: MemberNames[p] or "signal<p+1>".
    std::vector<std::string> names;
    if (b.params.contains(nflow::kMemberNames) && b.params[nflow::kMemberNames].is_array()) {
        for (const auto& n : b.params[nflow::kMemberNames]) {
            if (n.is_string()) {
                names.push_back(n.get<std::string>());
            }
        }
    }
    BusType t;
    t.nonvirtual = false;
    if (b.params.contains(nflow::kNonVirtual) && b.params[nflow::kNonVirtual].is_boolean()) {
        t.nonvirtual = b.params[nflow::kNonVirtual].get<bool>();
    }
    for (int p = 0; p < nIn; ++p) {
        BusMember m;
        m.name = (p < (int)names.size() && !names[(size_t)p].empty())
            ? names[(size_t)p]
            : ("signal" + std::to_string(p + 1));
        if (p < (int)inSigs.size()) {
            m.sig = inSigs[(size_t)p];
            m.sig.offset = -1;
        }
        t.members.push_back(std::move(m));
    }
    const std::string declared = bd.paramStr(nflow::kBusType, "");
    if (!declared.empty()) {
        const int want = busReg.idOf(declared);
        if (want < 0) {
            err = "unknown bus type '" + declared + "'";
            return false;
        }
        // Adopt the declared layout (it is the contract); the runtime packs
        // members in port order, so member count must match.
        if ((int)busReg.types[(size_t)want].members.size() != nIn) {
            err = "bus type '" + declared + "' declares "
                + std::to_string(busReg.types[(size_t)want].members.size())
                + " members but the block has " + std::to_string(nIn) + " inputs";
            return false;
        }
        outSigs[0].busTypeId = want;
        outSigs[0].setVector(std::max(1, busReg.packedWidth(want)));
        outSigs[0].type = SigType::Double;
        outSigs[0].isComplex = false;
        return true;
    }
    // Anonymous (inferred) bus: reuse a structurally equal registered type.
    int id = -1;
    const int probe = busReg.add(t); // candidate at the end
    for (int k = 0; k < probe; ++k) {
        if (busReg.structurallyEqual(k, probe)) {
            id = k;
            break;
        }
    }
    if (id >= 0) {
        busReg.types.pop_back();
    } else {
        id = probe;
    }
    outSigs[0].busTypeId = id;
    outSigs[0].setVector(std::max(1, busReg.packedWidth(id)));
    outSigs[0].type = SigType::Double;
    outSigs[0].isComplex = false;
    return true;
}
//=============================================================================
