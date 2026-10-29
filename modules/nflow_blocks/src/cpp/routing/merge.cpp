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
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenLang.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <cstdlib>
#include <string>
#include <vector>
#include "routing_blocks.hpp"
//=============================================================================
// merge: recombines the outputs of mutually-exclusive conditional (action)
// subsystems. Its inputs are driven directly by conditional subsystems, only one
// of which executes on a given step. INIT latches InitialOutput and resolves the
// source block feeding each input. ALGEBRAIC copies the input whose source ran
// this step (SubsystemState::lastActiveStamp == ctx.stepIndex) and otherwise
// re-emits the last value. Two active sources the same step: the later one (in
// port order) wins.
//=============================================================================
namespace {
double
readInitialOutput(const Nelson::NFlow::json& params)
{
    if (!params.contains("InitialOutput")) {
        return 0.0;
    }
    const auto& v = params["InitialOutput"];
    if (v.is_number()) {
        return v.get<double>();
    }
    if (v.is_string()) {
        const std::string s = v.get<std::string>();
        char* end = nullptr;
        double d = std::strtod(s.c_str(), &end);
        return (end != s.c_str()) ? d : 0.0;
    }
    return 0.0;
}
//=============================================================================
} // namespace
//=============================================================================
bool
Nelson::NFlow::handleMerge(SimCtx& ctx, const Block& b, Phase phase)
{
    auto& st = getState(ctx, b.nid);
    const int nIn = std::max(1, b.inputs);

    if (phase == Phase::INIT) {
        const double init0 = readInitialOutput(asJson(b.params));
        st.outLatch.assign(1, init0);
        setOutput(ctx, b.nid, init0);

        // slot -> owner: the source block nid feeding each input port.
        int totalSlots = 0;
        for (int i = 0; i < static_cast<int>(ctx.blocks.size()); ++i) {
            const int end = ctx.outputBase[ctx.blocks[i].nid] + ctx.blocks[i].outputs;
            if (end > totalSlots) {
                totalSlots = end;
            }
        }
        std::vector<int> slotOwner(static_cast<size_t>(totalSlots), -1);
        for (int i = 0; i < static_cast<int>(ctx.blocks.size()); ++i) {
            const BlockId onid = ctx.blocks[i].nid;
            for (int po = 0; po < ctx.blocks[i].outputs; ++po) {
                const int s = ctx.outputBase[onid] + po;
                if (s >= 0 && s < totalSlots) {
                    slotOwner[static_cast<size_t>(s)] = onid;
                }
            }
        }
        st.mergeSourceNids.assign(static_cast<size_t>(nIn), -1);
        if (b.nid >= 0 && b.nid < static_cast<int>(ctx.inputIds.size())) {
            const auto& ins = ctx.inputIds[b.nid];
            for (int p = 0; p < nIn && p < static_cast<int>(ins.size()); ++p) {
                const int srcSlot = ins[p];
                if (srcSlot >= 0 && srcSlot < totalSlots) {
                    st.mergeSourceNids[static_cast<size_t>(p)]
                        = slotOwner[static_cast<size_t>(srcSlot)];
                }
            }
        }
        return false;
    }

    if (phase == Phase::ALGEBRAIC) {
        int active = -1;
        for (int p = 0; p < nIn; ++p) {
            const int srcNid = (p < static_cast<int>(st.mergeSourceNids.size()))
                ? st.mergeSourceNids[static_cast<size_t>(p)]
                : -1;
            if (srcNid >= 0 && srcNid < static_cast<int>(ctx.blockState.size())) {
                const auto& srcSub = ctx.blockState[static_cast<size_t>(srcNid)].subsystem;
                if (srcSub && srcSub->lastActiveStamp == ctx.stepIndex) {
                    active = p;
                }
            }
        }
        double outv;
        if (active >= 0) {
            outv = getInput(ctx, b.nid, active, 0.0);
            if (st.outLatch.empty()) {
                st.outLatch.assign(1, outv);
            } else {
                st.outLatch[0] = outv;
            }
        } else {
            outv = st.outLatch.empty() ? 0.0 : st.outLatch[0];
        }
        setOutput(ctx, b.nid, outv);
        return false;
    }
    return false;
}
//=============================================================================
// Code generation. The handler copies the input whose SOURCE ran this step and
// otherwise re-emits the last value; in generated code "ran this step" is the
// source's gate being open. Each input of a merge is fed by the boundary hold
// the flattener puts on a conditional subsystem's output, named
// "<sub>__nfh<port>" against a gate named "<sub>__nfg" - so the gate is read
// off the source's own name. An input that is not fed by one of those holds is
// unconditional and always wins its turn. Later port wins, as in the handler.
//=============================================================================
namespace {
// The gate id governing a merge input, or "" when the source is unconditional.
std::string
mergeSourceGate(const std::string& sourceId)
{
    const std::string mark = "__nfh";
    const size_t k = sourceId.rfind(mark);
    if (k == std::string::npos) {
        return std::string();
    }
    return sourceId.substr(0, k) + "__nfg";
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
makeMergeCodegen(Nelson::NFlow::CodegenLang L)
{
    using namespace Nelson::NFlow;
    BlockCodegenTemplate t;
    t.emitState = [L](const BlockCodegenStateArgs& a) {
        nflow::BlockDescriptor bd(*a.block, *a.variables);
        a.addState("mg_" + a.id, a.fmt(bd.paramDouble("InitialOutput", 0.0)), "");
    };
    t.emitStep = [L](const BlockCodegenArgs& a) {
        const std::string held = L.sref("mg_" + a.id);
        const int nIn = std::max(1, a.block->value(nflow::kInputs, 1));
        a.line("out_" + a.id + " = " + held + ";");
        for (int k = 0; k < nIn && k < static_cast<int>(a.in.size()); ++k) {
            const std::string src
                = (k < static_cast<int>(a.inputIds.size())) ? a.inputIds[k] : std::string();
            const std::string gate = mergeSourceGate(a.sanitize(src));
            if (gate.empty()) {
                a.line("out_" + a.id + " = " + a.in[k] + ";");
            } else if (L.rust) {
                a.line("if out_" + gate + " != 0.0 { out_" + a.id + " = " + a.in[k] + "; }");
            } else {
                a.line("if (out_" + gate + " != 0.0) { out_" + a.id + " = " + a.in[k] + "; }");
            }
        }
        a.line(held + " = out_" + a.id + ";");
    };
    return t;
}
//=============================================================================
} // namespace
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCMerge()
{
    return makeMergeCodegen({ false });
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustMerge()
{
    return makeMergeCodegen({ true });
}
//=============================================================================
