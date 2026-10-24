//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// multiportSwitch: route one of N data inputs to the output, selected by a
// control input. Input port 0 is the control; ports 1..N are the data ports.
// The control is truncated to an integer and clamped to [1, N] (one-based); the
// selected data input is copied to the output, element-wise. ALGEBRAIC
// feedthrough, no state. Has C / Rust code generation.
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "routing_blocks.hpp"
#include <cmath>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    bool
    handleMultiportSwitch(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        const int n = numInputs(ctx, b.nid);
        const int nData = n - 1;
        if (nData < 1 || !hasInput(ctx, b.nid, 0)) {
            return false;
        }
        SigView ctrl = getInputSig(ctx, b.nid, 0);
        // A fractional control selects the port its integer PART names: 1.5
        // is still port 1, and port 2 is reached at 2.0.
        int idx = (int)std::trunc(sigAt(ctrl, 0));
        if (idx < 1) {
            idx = 1;
        }
        if (idx > nData) {
            idx = nData;
        }
        SigView sel = getInputSig(ctx, b.nid, idx);
        if (const PortSig* ex = exact64OutPort(ctx, b.nid)) {
            return emitElementwiseI64(
                ctx, b.nid, [&](int i) { return sigAtI64(sel, i, ex->type); });
        }
        if (complexOutPort(ctx, b.nid)) {
            return emitElementwiseComplex(ctx, b.nid, [&](int i) { return sigAtC(sel, i); });
        }
        return emitElementwise(ctx, b.nid, [&](int i) { return sigAt(sel, i); });
    }
    //=============================================================================
    // Output width follows the widest data input (the control is port 0).
    static bool
    resolveMultiportSwitchDims(const Block& b, const ValMap& vars,
        const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs, std::string& err)
    {
        (void)b;
        (void)vars;
        if (inSigs.size() < 2) {
            err = "multiportSwitch requires a control input and at least one data input";
            return false;
        }
        int w = 1;
        const SigType dataType = inSigs[1].type;
        const bool isComplex = inSigs[1].isComplex;
        for (size_t k = 1; k < inSigs.size(); ++k) {
            w = std::max(w, inSigs[k].width());
            if (inSigs[k].type != dataType || inSigs[k].isComplex != isComplex) {
                err = std::string("multiportSwitch data inputs mix signal types (")
                    + sigTypeName(dataType) + " vs " + sigTypeName(inSigs[k].type)
                    + "); insert a convert block";
                return false;
            }
        }
        outSigs[0].setVector(w);
        outSigs[0].type = dataType;
        outSigs[0].isComplex = isComplex;
        return true;
    }
    //=============================================================================
    void
    registerMultiportSwitch(BlockRegistry& reg)
    {
        BlockMetadata feedthrough;
        feedthrough.directFeedthrough = true;
        feedthrough.acceptsVectorInputs = true;
        reg.registerHandler("multiportSwitch", handleMultiportSwitch, { Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        reg.setDimsMetadata("multiportSwitch", true, resolveMultiportSwitchDims);
        reg.registerCodegen("multiportSwitch", "c", getCodeGenCMultiportSwitch());
        reg.registerCodegen("multiportSwitch", "rust", getCodeGenRustMultiportSwitch());
    }
    //=============================================================================
    namespace {
        int
        dataInputCount(const BlockCodegenArgs& a)
        {
            int n = 0;
            if (a.block && a.block->contains(nflow::kInputs)
                && (*a.block)[nflow::kInputs].is_number()) {
                n = (int)(*a.block)[nflow::kInputs].get<double>();
            }
            if (n <= 0) {
                n = (int)a.in.size();
            }
            return std::max(1, n - 1); // exclude the control port
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenCMultiportSwitch()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            const int nData = dataInputCount(a);
            const std::string iv = "__ms_" + a.id;
            const std::string nd = std::to_string(nData);
            a.line("int " + iv + " = (int)(" + a.in[0] + ");");
            a.line("if (" + iv + " < 1) " + iv + " = 1; if (" + iv + " > " + nd + ") " + iv + " = "
                + nd + ";");
            std::string expr = a.in[(size_t)nData]; // final else = last data port
            for (int k = nData - 1; k >= 1; --k) {
                expr = "(" + iv + " == " + std::to_string(k) + ") ? " + a.in[(size_t)k] + " : ("
                    + expr + ")";
            }
            a.line("out_" + a.id + " = " + expr + ";");
        };
        return t;
    }
    //=============================================================================
    BlockCodegenTemplate
    getCodeGenRustMultiportSwitch()
    {
        BlockCodegenTemplate t;
        t.emitStep = [](const BlockCodegenArgs& a) {
            const int nData = dataInputCount(a);
            const std::string iv = "__ms_" + a.id;
            const std::string nd = std::to_string(nData);
            a.line("let mut " + iv + ": i32 = (" + a.in[0] + ").trunc() as i32;");
            a.line("if " + iv + " < 1 { " + iv + " = 1; } if " + iv + " > " + nd + " { " + iv
                + " = " + nd + "; }");
            std::string expr = a.in[(size_t)nData];
            for (int k = nData - 1; k >= 1; --k) {
                expr = "if " + iv + " == " + std::to_string(k) + " { " + a.in[(size_t)k]
                    + " } else { " + expr + " }";
            }
            a.line("out_" + a.id + " = " + expr + ";");
        };
        return t;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
