//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BlockRegistry.hpp"
#include "StateSpaceMatrices.hpp"
#include "FieldNames.hpp"
#include "discrete/discrete_blocks.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerDiscreteHandlers(BlockRegistry& reg)
    {
        // zoh answers the CURRENT input at a hit instant (direct feedthrough,
        // like the emitted step) and commits the held value in UPDATE; reading
        // the committed value back as the output delayed it by a sample period.
        {
            BlockMetadata zohMeta;
            zohMeta.directFeedthrough = true;
            reg.registerHandler("zoh", handleZoh, { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE },
                "builtin.discrete", zohMeta);
        }
        // foh answers the CURRENT input at a hit instant (direct feedthrough,
        // like the emitted step) and commits its samples in UPDATE; reading the
        // committed value back delayed it by a whole sample period.
        {
            BlockMetadata fohMeta;
            fohMeta.acceptsVectorInputs = true;
            fohMeta.directFeedthrough = true;
            reg.registerHandler("foh", handleFoh, { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE },
                "builtin.discrete", fohMeta);
        }
        {
            BlockMetadata m;
            m.acceptsVectorInputs = true;
            // The discrete transfer function reads its coefficients as polynomials
            // in z, so a biproper one feeds through and only a lower-degree
            // numerator delays: its output belongs to the topologically-ordered
            // ALGEBRAIC phase, like the discrete state space.
            BlockMetadata dtfMeta = m;
            dtfMeta.directFeedthrough = true;
            reg.registerHandler("dtf", handleDtf, { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE },
                "builtin.discrete", dtfMeta);
            reg.registerHandler("ddelay", handleDdelay,
                { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.discrete", m);
            // y(k) = C x(k) + D u(k) reads the current input, so it belongs to the
            // topologically-ordered ALGEBRAIC phase.
            BlockMetadata dss = m;
            dss.directFeedthrough = true;
            // A MIMO form has as many outputs as C has rows; the scalar form is
            // element-wise and keeps the input width.
            dss.resolveDims
                = [](const Block& blk, const ValMap& vars, const std::vector<PortSig>& inSigs,
                      std::vector<PortSig>& outSigs, std::string&) {
                      if (outSigs.empty()) {
                          return true;
                      }
                      if (ssIsScalarForm(blk, vars)) {
                          outSigs[0].setVector(inSigs.empty() ? 1 : std::max(1, inSigs[0].width()));
                      } else {
                          outSigs[0].setVector(ssOutputWidthOf(blk, vars));
                      }
                      outSigs[0].type = SigType::Double;
                      return true;
                  };
            reg.registerHandler("dstateSpace", handleDstateSpace,
                { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete", dss);
        }
        reg.registerHandler("unitDelay", handleUnitDelay,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.discrete");
        {
            BlockMetadata rtMeta;
            rtMeta.acceptsVectorInputs = true;
            // The tick answers the input it samples, so the output belongs to the
            // topologically ordered phase.
            rtMeta.directFeedthrough = true;
            reg.registerHandler("rateTransition", handleRateTransition,
                { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete", rtMeta);
        }
        // difference and the detect* family are direct-feedthrough: their output reads
        // the CURRENT input, so they run in the topologically-ordered ALGEBRAIC phase.
        // In the declaration-ordered OUTPUT phase a block declared ahead of its source
        // would sample an input not yet computed. They are also multirate-capable:
        // they recompute their output on every base step and gate only the state
        // advance on the sample hit. setAlgebraicOutputEveryStep exempts them from the
        // sub-rate sample-and-hold, as in codegen.
        reg.registerHandler("difference", handleDifference,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        reg.registerHandler("detectChange", handleDetectChange,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        reg.registerHandler("risingEdge", handleRisingEdge,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        reg.registerHandler("fallingEdge", handleFallingEdge,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        reg.registerHandler("detectIncrease", handleDetectIncrease,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        reg.registerHandler("detectDecrease", handleDetectDecrease,
            { Phase::INIT, Phase::ALGEBRAIC, Phase::UPDATE }, "builtin.discrete");
        for (const char* t : { "difference", "detectChange", "risingEdge", "fallingEdge",
                 "detectIncrease", "detectDecrease" }) {
            reg.setAlgebraicOutputEveryStep(t);
        }

        // Code-generation templates (other blocks keep native emission paths).
        reg.registerCodegen("zoh", "c", getCodeGenCZoh());
        reg.registerCodegen("zoh", "rust", getCodeGenRustZoh());
        reg.registerCodegen("rateTransition",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCRateTransition(), getCodeGenRustRateTransition() });
        reg.registerCodegen("foh", "c", getCodeGenCFoh());
        reg.registerCodegen("foh", "rust", getCodeGenRustFoh());
        reg.registerCodegen("dtf", "c", getCodeGenCDtf());
        reg.registerCodegen("dtf", "rust", getCodeGenRustDtf());
        reg.registerCodegen("ddelay", "c", getCodeGenCDdelay());
        reg.registerCodegen("ddelay", "rust", getCodeGenRustDdelay());
        reg.registerCodegen("dstateSpace", "c", getCodeGenCDstateSpace());
        reg.registerCodegen("dstateSpace", "rust", getCodeGenRustDstateSpace());
        // These emitters gate their state advance through codegenSampleRate,
        // so a slower per-block SampleTime is honored in generated code.
        auto multirate = [](BlockCodegenTemplate t) {
            t.multirateCapable = true;
            return t;
        };
        reg.registerCodegen("unitDelay", "c", multirate(getCodeGenCUnitDelay()));
        reg.registerCodegen("unitDelay", "rust", multirate(getCodeGenRustUnitDelay()));
        reg.registerCodegen("difference", "c", multirate(getCodeGenCDifference()));
        reg.registerCodegen("difference", "rust", multirate(getCodeGenRustDifference()));
        reg.registerCodegen("detectChange", "c", multirate(getCodeGenCDetectChange()));
        reg.registerCodegen("detectChange", "rust", multirate(getCodeGenRustDetectChange()));
        reg.registerCodegen("risingEdge", "c", multirate(getCodeGenCRisingEdge()));
        reg.registerCodegen("risingEdge", "rust", multirate(getCodeGenRustRisingEdge()));
        reg.registerCodegen("fallingEdge", "c", multirate(getCodeGenCFallingEdge()));
        reg.registerCodegen("fallingEdge", "rust", multirate(getCodeGenRustFallingEdge()));
        reg.registerCodegen("detectIncrease", "c", multirate(getCodeGenCDetectIncrease()));
        reg.registerCodegen("detectIncrease", "rust", multirate(getCodeGenRustDetectIncrease()));
        reg.registerCodegen("detectDecrease", "c", multirate(getCodeGenCDetectDecrease()));
        reg.registerCodegen("detectDecrease", "rust", multirate(getCodeGenRustDetectDecrease()));
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
