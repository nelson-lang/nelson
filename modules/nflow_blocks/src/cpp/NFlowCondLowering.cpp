//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Conditional-subsystem lowering artifacts (code generation only).
// The flattener rewrites an enable-gated subsystem into its inner blocks
// tagged with a gate id (params.__nfgGates), a synthetic `condGate` block fed
// by the control wire, and one synthetic `condHold` per external output.
// Both types are registered without phases: they exist only in flattened
// code-generation diagrams, never in a simulated one.
//
//   condGate: out = (control > 0) ? 1 : 0; the level gate of the runtime's
//     enable semantics (active while any control element > 0). The control
//     wire is scalar: the vector expansion rejects vector control wires.
//   condHold: while the gate is open, latch the input; the output always
//     reads the latch, like the runtime's held external outputs (initialized
//     to 0.0).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "NFlowCodegenLang.hpp"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace {
        // The smallest positive double, used as the "parked on zero" marker of a
        // trigger gate's control latch (see zeroSideLatch in SubsystemHandlers).
        const std::string kTinyLiteral = "2.2250738585072014e-308";
        //=============================================================================
        bool
        handleCondNoop(SimCtx&, const Block&, Phase)
        {
            return false; // codegen-only: never scheduled by the engine
        }
        //=============================================================================
        static BlockCodegenTemplate
        makeCondGateCodegen(CodegenLang L)
        {
            BlockCodegenTemplate t;
            t.emitState = [](const BlockCodegenStateArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                if (bd.paramStr("kind", "") == "enable") {
                    // Level gate sampled once per major step, so every RK
                    // sub-stage integrates under the same one (see the rhs-mode
                    // emission in the C / Rust generators).
                    a.addState("nfgl_" + a.id, "", "");
                }
                const std::string em = bd.paramStr("mode", "enable");
                if (em == "trigger" || em == "enableTrigger") {
                    // Edge memory: control latched at end of step + first-latch
                    // flag (no edge fires before the first latch, t0 parity).
                    a.addState("nfgp_" + a.id, "", "");
                    a.addState("nfgh_" + a.id, "", "");
                }
            };
            t.emitStep = [L](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string mode = bd.paramStr("mode", "enable");
                if (mode == "trigger" || mode == "enableTrigger") {
                    const int tt = (int)bd.paramDouble("tt", 0.0);
                    const std::string p = L.sref("nfgp_" + a.id);
                    // enableTrigger reads the edge on port 1; port 0 is the
                    // enable level that has to be high for it to count.
                    const std::string c = (mode == "enableTrigger") ? a.in[1] : a.in[0];
                    // Same edge rule as the runtime's triggerFired(): a falling
                    // edge is a positive control reaching zero or below, so a 0/1
                    // gate signal fires on its 1 -> 0 fall. A control parked ON zero
                    // is latched as +/- the smallest positive double (the sign says
                    // which side it came from); its arrival already fired, so
                    // leaving it needs a strict move to the other side.
                    const std::string zero = "(" + p + " == " + kTinyLiteral + " || " + p + " == -"
                        + kTinyLiteral + ")";
                    const std::string rise = "("
                        + L.ternary(zero, "(" + p + " < 0.0 && " + c + " > 0.0)",
                            "((" + p + " < 0.0 && " + c + " >= 0.0) || (" + p + " == 0.0 && " + c
                                + " > 0.0))")
                        + ")";
                    const std::string fall = "("
                        + L.ternary(zero, "(" + p + " > 0.0 && " + c + " < 0.0)",
                            "((" + p + " > 0.0 && " + c + " <= 0.0) || (" + p + " == 0.0 && " + c
                                + " < 0.0))")
                        + ")";
                    std::string edge;
                    if (tt == 1) {
                        edge = fall;
                    } else if (tt == 2) {
                        edge = rise + " || " + fall;
                    } else {
                        edge = rise;
                    }
                    std::string fired = L.sref("nfgh_" + a.id) + " != 0.0 && (" + edge + ")";
                    if (mode == "enableTrigger") {
                        fired = "(" + a.in[0] + ") > 0.0 && " + fired;
                    }
                    a.line("out_" + a.id + " = " + L.ternary(fired, "1.0", "0.0") + ";");
                    return;
                }
                a.line("out_" + a.id + " = " + L.ternary(a.in[0] + " > 0.0", "1.0", "0.0") + ";");
            };
            return t;
        }
        //=============================================================================
        // End-of-step latch of a trigger gate's control value (the runtime
        // latches prevCtrl during UPDATE so the edge is measured against the
        // previous major step). Ordered after its gate by a synthetic edge.
        static BlockCodegenTemplate
        makeCondLatchCodegen(CodegenLang L)
        {
            BlockCodegenTemplate t;
            t.emitStep = [L](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string gate = a.sanitize(bd.paramStr("gate", ""));
                if (gate.empty()) {
                    return;
                }
                // Same latch as the runtime's zeroSideLatch(): a control
                // resting exactly on zero is remembered as +/- the smallest
                // positive double, whose sign says which side it arrived from,
                // so a crossing that lands on a sample fires once and not twice.
                const std::string p = L.sref("nfgp_" + gate);
                const std::string c = a.in[0];
                const std::string zero
                    = "(" + p + " == " + kTinyLiteral + " || " + p + " == -" + kTinyLiteral + ")";
                const std::string arrived = L.ternary(
                    p + " > 0.0", "-" + kTinyLiteral, L.ternary(p + " < 0.0", kTinyLiteral, "0.0"));
                a.line(p + " = "
                    + L.ternary(c + " == 0.0 && " + L.sref("nfgh_" + gate) + " != 0.0",
                        L.ternary(zero, p, arrived), c)
                    + ";");
                a.line(L.sref("nfgh_" + gate) + " = 1.0;");
            };
            return t;
        }
        //=============================================================================
        static BlockCodegenTemplate
        makeCondHoldCodegen(CodegenLang L)
        {
            BlockCodegenTemplate t;
            t.emitState = [](const BlockCodegenStateArgs& a) { a.addState("ch_" + a.id, "", ""); };
            t.emitStep = [L](const BlockCodegenArgs& a) {
                nflow::BlockDescriptor bd(*a.block, *a.variables);
                const std::string gate = a.sanitize(bd.paramStr("gate", ""));
                const std::string h = L.sref("ch_" + a.id);
                if (gate.empty()) {
                    // Defensive: an untagged hold degenerates to a wire.
                    a.line("out_" + a.id + " = " + a.in[0] + ";");
                    return;
                }
                if (L.rust) {
                    a.line("if out_" + gate + " != 0.0 { " + h + " = " + a.in[0] + "; }");
                } else {
                    a.line("if (out_" + gate + " != 0.0) { " + h + " = " + a.in[0] + "; }");
                }
                a.line("out_" + a.id + " = " + h + ";");
            };
            return t;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    void
    registerCondLoweringHandlers(BlockRegistry& reg)
    {
        BlockMetadata gateMeta;
        gateMeta.directFeedthrough = true;
        gateMeta.acceptsVectorInputs = true;
        gateMeta.resolveDims = [](const Block&, const ValMap&, const std::vector<PortSig>&,
                                   std::vector<PortSig>& outSigs, std::string&) {
            outSigs[0].setVector(1);
            outSigs[0].type = SigType::Double;
            return true;
        };
        reg.registerHandler("condGate", handleCondNoop, {}, "builtin.utility", gateMeta);
        BlockMetadata holdMeta;
        holdMeta.directFeedthrough = true;
        holdMeta.acceptsVectorInputs = true;
        holdMeta.resolveDims = [](const Block&, const ValMap&, const std::vector<PortSig>& inSigs,
                                   std::vector<PortSig>& outSigs, std::string&) {
            if (!inSigs.empty()) {
                outSigs[0] = inSigs[0];
            } else {
                outSigs[0].setVector(1);
                outSigs[0].type = SigType::Double;
            }
            return true;
        };
        reg.registerHandler("condHold", handleCondNoop, {}, "builtin.utility", holdMeta);
        BlockMetadata latchMeta;
        latchMeta.directFeedthrough = true;
        latchMeta.resolveDims = [](const Block&, const ValMap&, const std::vector<PortSig>&,
                                    std::vector<PortSig>& outSigs, std::string&) {
            if (!outSigs.empty()) {
                outSigs[0].setVector(1);
                outSigs[0].type = SigType::Double;
            }
            return true;
        };
        reg.registerHandler("condLatch", handleCondNoop, {}, "builtin.utility", latchMeta);
        reg.registerCodegen("condLatch",
            BlockRegistry::BlockCodegenPair {
                makeCondLatchCodegen({ false }), makeCondLatchCodegen({ true }) });
        reg.registerCodegen("condGate",
            BlockRegistry::BlockCodegenPair {
                makeCondGateCodegen({ false }), makeCondGateCodegen({ true }) });
        reg.registerCodegen("condHold",
            BlockRegistry::BlockCodegenPair {
                makeCondHoldCodegen({ false }), makeCondHoldCodegen({ true }) });
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
