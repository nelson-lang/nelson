//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <algorithm>
#include "source_blocks.hpp"
//=============================================================================
bool
Nelson::NFlow::handleLabelSource(SimCtx& ctx, const Block& b, Phase phase)
{
    if (phase != Phase::OUTPUT) {
        return false;
    }
    if (b.params.contains(nflow::kIsExternalPort) && b.params[nflow::kIsExternalPort].get<bool>()) {
        return false;
    }
    std::string name;
    if (b.params.contains(nflow::kGotoTag) && b.params[nflow::kGotoTag].is_string()) {
        name = b.params[nflow::kGotoTag].get<std::string>();
    }
    const int w = outputWidth(ctx, b.nid, 0);
    double* y = outputSlice(ctx, b.nid, 0);
    // Sibling lanes of this goto/from wire (active when the port is
    // exact-64 / complex, or a bus whose members may use any lane): the
    // slot offsets are shared across lanes.
    const PortSig* ps = portSigOf(ctx, b.nid, 0);
    const bool busPort = ps && ps->busTypeId >= 0;
    long long* yi = (ps && (portUsesI64(*ps) || busPort)) ? outputSliceI64(ctx, b.nid, 0) : nullptr;
    double* yim = (ps && (portUsesImag(*ps) || busPort)) ? outputSliceImag(ctx, b.nid, 0) : nullptr;
    for (int i = 0; i < w; ++i) {
        y[i] = 0.0;
        if (yi) {
            yi[i] = 0;
        }
        if (yim) {
            yim[i] = 0.0;
        }
    }
    if (!name.empty()) {
        auto sit = ctx.labelSinks.find(name);
        if (sit != ctx.labelSinks.end()) {
            auto nit = ctx.nameToId.find(sit->second);
            if (nit != ctx.nameToId.end()) {
                BlockId sinkNid = nit->second;
                if (!ctx.inputIds[sinkNid].empty() && ctx.inputIds[sinkNid][0] >= 0) {
                    // Copy the full slice: the labelSource port was sized to
                    // the sink's input during signal layout.
                    const int srcSlot = ctx.inputIds[sinkNid][0];
                    for (int i = 0; i < w; ++i) {
                        y[i] = ctx.outputs[srcSlot + i];
                        if (yi && ctx.outputsI64) {
                            yi[i] = (*ctx.outputsI64)[srcSlot + i];
                        }
                        if (yim && ctx.outputsImag) {
                            yim[i] = (*ctx.outputsImag)[srcSlot + i];
                        }
                    }
                }
            }
        }
    }
    return false;
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCLabelSource()
{
    // Not implemented here: the C generator keeps its native path for this
    // block (emission is entangled with generator-wide analysis).
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustLabelSource()
{
    // Not implemented here: the Rust generator keeps its native path for this
    // block (emission is entangled with generator-wide analysis).
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
