//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// busSelector: extract members (or nested sub-buses) from a bus by path
// ("a" or "a.b.c"), one output port per selected path. Member offsets come
// from the registry (SimCtx::busRegistry); all storage lanes are preserved.
//=============================================================================
#include "routing_blocks.hpp"
#include "SimEngineBusTypes.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <algorithm>
//=============================================================================
namespace {
std::vector<std::string>
selectedPaths(const Nelson::NFlow::Block& b)
{
    std::vector<std::string> paths;
    if (b.params.contains(nflow::kSelectedSignals)
        && b.params[nflow::kSelectedSignals].is_array()) {
        for (const auto& s : b.params[nflow::kSelectedSignals]) {
            if (s.is_string()) {
                paths.push_back(s.get<std::string>());
            }
        }
    }
    return paths;
}
//=============================================================================
bool
outputAsBus(const Nelson::NFlow::Block& b)
{
    return b.params.contains(nflow::kOutputAsBus) && b.params[nflow::kOutputAsBus].is_boolean()
        && b.params[nflow::kOutputAsBus].get<bool>();
}
//=============================================================================
std::string
memberNameForPath(const std::vector<std::string>& paths, size_t idx)
{
    std::string name = paths[idx];
    const size_t dot = name.find_last_of('.');
    if (dot != std::string::npos && dot + 1 < name.size()) {
        name = name.substr(dot + 1);
    }
    for (size_t k = 0; k < idx; ++k) {
        std::string other = paths[k];
        const size_t odot = other.find_last_of('.');
        if (odot != std::string::npos && odot + 1 < other.size()) {
            other = other.substr(odot + 1);
        }
        if (other == name) {
            name = paths[idx];
            std::replace(name.begin(), name.end(), '.', '_');
            break;
        }
    }
    return name;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleBusSelector(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::ALGEBRAIC) {
        return false;
    }
    SigView v = getInputSig(ctx, b.nid, 0);
    const std::vector<std::string> paths = selectedPaths(b);
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
    bool changed = false;
    if (outputAsBus(b)) {
        double* y = outputSlice(ctx, b.nid, 0);
        double* yim = outputSliceImag(ctx, b.nid, 0);
        long long* yi = outputSliceI64(ctx, b.nid, 0);
        int dst = 0;
        const int outW = outputWidth(ctx, b.nid, 0);
        for (const auto& path : paths) {
            const int off = ctx.busRegistry->memberOffset(busId, path);
            const PortSig* ms = ctx.busRegistry->resolvePath(busId, path);
            if (off < 0 || !ms) {
                continue;
            }
            const int wo
                = (ms->busTypeId >= 0) ? ctx.busRegistry->packedWidth(ms->busTypeId) : ms->width();
            for (int k = 0; k < wo && dst < outW; ++k, ++dst) {
                const int src = off + k;
                const double val = sigAt(v, src);
                if (y[dst] != val) {
                    y[dst] = val;
                    changed = true;
                }
                if (yim) {
                    const double iv = (v.imag && v.width > 0)
                        ? ((v.width == 1) ? v.imag[0] : v.imag[src])
                        : 0.0;
                    if (yim[dst] != iv) {
                        yim[dst] = iv;
                        changed = true;
                    }
                }
                if (yi) {
                    const long long ev
                        = v.idata ? ((v.width == 1) ? v.idata[0] : v.idata[src]) : (long long)val;
                    if (yi[dst] != ev) {
                        yi[dst] = ev;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    }
    const int nOut = std::max(1, b.outputs);
    for (int p = 0; p < nOut && p < (int)paths.size(); ++p) {
        const int off = ctx.busRegistry->memberOffset(busId, paths[(size_t)p]);
        if (off < 0) {
            continue; // rejected at compile time; defensive here
        }
        const int wo = outputWidth(ctx, b.nid, p);
        double* y = outputSlice(ctx, b.nid, p);
        double* yim = outputSliceImag(ctx, b.nid, p);
        long long* yi = outputSliceI64(ctx, b.nid, p);
        for (int k = 0; k < wo; ++k) {
            const int src = off + k;
            const double val = sigAt(v, src);
            if (y[k] != val) {
                y[k] = val;
                changed = true;
            }
            if (yim) {
                const double iv
                    = (v.imag && v.width > 0) ? ((v.width == 1) ? v.imag[0] : v.imag[src]) : 0.0;
                if (yim[k] != iv) {
                    yim[k] = iv;
                    changed = true;
                }
            }
            if (yi) {
                const long long ev
                    = v.idata ? ((v.width == 1) ? v.idata[0] : v.idata[src]) : (long long)val;
                if (yi[k] != ev) {
                    yi[k] = ev;
                    changed = true;
                }
            }
        }
    }
    return changed;
}
//=============================================================================
// Dimension/type rule: each selected path adopts the member's descriptor
// (nested sub-bus paths yield a bus-typed output port).
bool
Nelson::NFlow::resolveBusSelectorDims(const Block& b, const ValMap& /*vars*/,
    const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, BusRegistry& busReg,
    std::string& err)
{
    const int busId = inSigs.empty() ? -1 : inSigs[0].busTypeId;
    if (busId < 0) {
        // Input bus not resolved yet (or a non-bus input): leave the ports
        // as-is; the not-a-bus case is rejected after the fixed point.
        return true;
    }
    const std::vector<std::string> paths = selectedPaths(b);
    if (paths.empty()) {
        err = "busSelector requires a non-empty SelectedSignals list";
        return false;
    }
    if (outputAsBus(b)) {
        BusType t;
        t.nonvirtual = false;
        for (size_t p = 0; p < paths.size(); ++p) {
            const PortSig* ms = busReg.resolvePath(busId, paths[p]);
            if (!ms) {
                const BusType* bt = busReg.get(busId);
                std::string members;
                if (bt) {
                    for (const auto& m : bt->members) {
                        if (!members.empty()) {
                            members += ", ";
                        }
                        members += m.name;
                    }
                }
                err = "unknown bus member '" + paths[p] + "' (available: " + members + ")";
                return false;
            }
            BusMember m;
            m.name = memberNameForPath(paths, p);
            m.sig = *ms;
            m.sig.offset = -1;
            t.members.push_back(std::move(m));
        }
        int id = -1;
        const int probe = busReg.add(t);
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
    for (size_t p = 0; p < paths.size() && p < outSigs.size(); ++p) {
        const PortSig* ms = busReg.resolvePath(busId, paths[p]);
        if (!ms) {
            const BusType* t = busReg.get(busId);
            std::string members;
            if (t) {
                for (const auto& m : t->members) {
                    if (!members.empty()) {
                        members += ", ";
                    }
                    members += m.name;
                }
            }
            err = "unknown bus member '" + paths[p] + "' (available: " + members + ")";
            return false;
        }
        const int keepOffset = outSigs[p].offset;
        outSigs[p] = *ms;
        outSigs[p].offset = keepOffset;
        if (outSigs[p].busTypeId >= 0) {
            // Nested sub-bus output: packed footprint.
            outSigs[p].setVector(std::max(1, busReg.packedWidth(outSigs[p].busTypeId)));
            outSigs[p].type = SigType::Double;
            outSigs[p].isComplex = false;
        }
    }
    return true;
}
//=============================================================================
