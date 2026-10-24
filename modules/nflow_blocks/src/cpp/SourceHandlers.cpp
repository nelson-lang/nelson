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
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "source/source_blocks.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerSourceHandlers(BlockRegistry& reg)
    {
        reg.registerHandler("constant", handleConstant, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler(
            "enumeratedConstant", handleEnumeratedConstant, { Phase::OUTPUT }, "builtin.source");
        {
            BlockMetadata stepMeta;
            stepMeta.numZeroCrossings = 1; // step discontinuity surface (t - stepTime)
            stepMeta.directFeedthrough = true;
            stepMeta.discontinuityInstants = [](const Block& b, const ValMap& vars, double t0,
                                                 double t1, std::vector<double>& out) {
                nflow::BlockDescriptor bd(b, vars);
                const double td = bd.paramDouble(nflow::kStepTime, 0.0);
                if (td > t0 + 1e-12 && td <= t1 + 1e-12) {
                    out.push_back(td);
                }
            };
            reg.registerHandler("step", handleStep, { Phase::OUTPUT, Phase::ZERO_CROSSING },
                "builtin.source", stepMeta);
        }
        reg.registerHandler("ramp", handleRamp, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("impulse", handleImpulse, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("sine", handleSine, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("chirp", handleChirp, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("noise", handleNoise, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("clock", handleClock, { Phase::OUTPUT }, "builtin.source");
        {
            // A pulse jumps up at each period start (t = start + k*period) and
            // back down after its duty window (+ width%*period). List those hard
            // edges so the DAE solver stops just before each and restarts there.
            BlockMetadata pulseMeta;
            pulseMeta.directFeedthrough = true;
            pulseMeta.discontinuityInstants = [](const Block& b, const ValMap& vars, double t0,
                                                  double t1, std::vector<double>& out) {
                nflow::BlockDescriptor bd(b, vars);
                const double period = bd.paramDouble("Period", 1.0);
                const double width = bd.paramDouble("Width", 50.0);
                const double start = bd.paramDouble("StartTime", 0.0);
                if (period <= 0.0) {
                    return;
                }
                const double dutyDur = (width * period) / 100.0;
                long k = static_cast<long>(std::floor((t0 - start) / period));
                if (k < 0) {
                    k = 0;
                }
                for (;; ++k) {
                    const double rise = start + k * period;
                    if (rise > t1 + 1e-12) {
                        break;
                    }
                    if (rise > t0 + 1e-12) {
                        out.push_back(rise);
                    }
                    if (dutyDur > 0.0 && dutyDur < period) {
                        const double fall = rise + dutyDur;
                        if (fall > t0 + 1e-12 && fall <= t1 + 1e-12) {
                            out.push_back(fall);
                        }
                    }
                }
            };
            reg.registerHandler(
                "pulse", handlePulse, { Phase::OUTPUT }, "builtin.source", pulseMeta);
        }
        {
            // A sawtooth ramps up then jumps back to its offset at every period
            // reset (t = start + k*period, k >= 1). List those jumps.
            BlockMetadata sawMeta;
            sawMeta.directFeedthrough = true;
            sawMeta.discontinuityInstants = [](const Block& b, const ValMap& vars, double t0,
                                                double t1, std::vector<double>& out) {
                nflow::BlockDescriptor bd(b, vars);
                const double period = bd.paramDouble("Period", 1.0);
                const double start = bd.paramDouble("StartTime", 0.0);
                if (period <= 0.0) {
                    return;
                }
                long k = static_cast<long>(std::floor((t0 - start) / period));
                if (k < 1) {
                    k = 1;
                }
                for (;; ++k) {
                    const double td = start + k * period;
                    if (td > t1 + 1e-12) {
                        break;
                    }
                    if (td > t0 + 1e-12) {
                        out.push_back(td);
                    }
                }
            };
            reg.registerHandler(
                "sawTooth", handleSawTooth, { Phase::OUTPUT }, "builtin.source", sawMeta);
        }
        reg.registerHandler("expSine", handleExpSine, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("trapezoid", handleTrapezoid, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler(
            "exponentials", handleExponentials, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler("counterFreeRunning", handleCounterFreeRunning,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.source");
        reg.registerHandler("counterLimited", handleCounterLimited,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.source");
        reg.registerHandler("repeatingSequenceStair", handleRepeatingSequenceStair,
            { Phase::INIT, Phase::OUTPUT, Phase::UPDATE }, "builtin.source");
        reg.registerHandler("repeatingSequenceInterpolated", handleRepeatingSequenceInterpolated,
            { Phase::OUTPUT }, "builtin.source");
        {
            BlockMetadata sigGenMeta;
            sigGenMeta.directFeedthrough = true;
            // A square wave flips sign at every half-period (t = k/(2f)); a
            // sawtooth jumps from +A to -A at every period (t = k/f). A sine wave
            // is smooth: no scheduled discontinuity.
            sigGenMeta.discontinuityInstants = [](const Block& b, const ValMap& vars, double t0,
                                                   double t1, std::vector<double>& out) {
                nflow::BlockDescriptor bd(b, vars);
                const std::string wave = bd.paramStr("Waveform", "sine");
                const double freq = bd.paramDouble("Frequency", 1.0);
                if (freq <= 0.0) {
                    return;
                }
                double period = 0.0;
                if (wave == "square") {
                    period = 0.5 / freq; // half-period between sign flips
                } else if (wave == "sawtooth") {
                    period = 1.0 / freq; // full period between resets
                } else {
                    return;
                }
                const long kStart = static_cast<long>(std::floor(t0 / period)) + 1;
                for (long k = kStart; k > 0; ++k) {
                    const double td = k * period;
                    if (td > t1 + 1e-12) {
                        break;
                    }
                    if (td > t0 + 1e-12) {
                        out.push_back(td);
                    }
                }
            };
            reg.registerHandler("signalGenerator", handleSignalGenerator, { Phase::OUTPUT },
                "builtin.source", sigGenMeta);
        }
        reg.registerHandler(
            "fileSource", handleFileSource, { Phase::INIT, Phase::OUTPUT }, "builtin.source");
        reg.registerHandler(
            "fromWorkspace", handleFromWorkspace, { Phase::INIT, Phase::OUTPUT }, "builtin.source");
        reg.registerHandler(
            nflow::kLabelSource, handleLabelSource, { Phase::OUTPUT }, "builtin.source");
        reg.registerHandler(
            "toggleSwitch", handleToggleSwitch, { Phase::OUTPUT }, "builtin.utility");

        reg.registerCodegen("chirp", "c", getCodeGenCChirp());
        reg.registerCodegen("chirp", "rust", getCodeGenRustChirp());
        reg.registerCodegen("constant", "c", getCodeGenCConstant());
        reg.registerCodegen("constant", "rust", getCodeGenRustConstant());
        reg.registerCodegen("enumeratedConstant", "c", getCodeGenCEnumeratedConstant());
        reg.registerCodegen("enumeratedConstant", "rust", getCodeGenRustEnumeratedConstant());
        reg.registerCodegen("clock", "c", getCodeGenCClock());
        reg.registerCodegen("clock", "rust", getCodeGenRustClock());
        reg.registerCodegen("step", "c", getCodeGenCStep());
        reg.registerCodegen("step", "rust", getCodeGenRustStep());
        reg.registerCodegen("ramp", "c", getCodeGenCRamp());
        reg.registerCodegen("ramp", "rust", getCodeGenRustRamp());
        reg.registerCodegen("impulse", "c", getCodeGenCImpulse());
        reg.registerCodegen("impulse", "rust", getCodeGenRustImpulse());
        reg.registerCodegen("sine", "c", getCodeGenCSine());
        reg.registerCodegen("sine", "rust", getCodeGenRustSine());
        reg.registerCodegen("pulse", "c", getCodeGenCPulse());
        reg.registerCodegen("pulse", "rust", getCodeGenRustPulse());
        reg.registerCodegen("sawTooth", "c", getCodeGenCSawTooth());
        reg.registerCodegen("sawTooth", "rust", getCodeGenRustSawTooth());
        reg.registerCodegen("expSine", "c", getCodeGenCExpSine());
        reg.registerCodegen("expSine", "rust", getCodeGenRustExpSine());
        reg.registerCodegen("trapezoid", "c", getCodeGenCTrapezoid());
        reg.registerCodegen("trapezoid", "rust", getCodeGenRustTrapezoid());
        reg.registerCodegen("exponentials", "c", getCodeGenCExponentials());
        reg.registerCodegen("exponentials", "rust", getCodeGenRustExponentials());
        reg.registerCodegen("toggleSwitch", "c", getCodeGenCToggleSwitch());
        reg.registerCodegen("toggleSwitch", "rust", getCodeGenRustToggleSwitch());
        reg.registerCodegen("fileSource", "c", getCodeGenCFileSource());
        reg.registerCodegen("fileSource", "rust", getCodeGenRustFileSource());
        reg.registerCodegen("fromWorkspace", "c", getCodeGenCFromWorkspace());
        reg.registerCodegen("fromWorkspace", "rust", getCodeGenRustFromWorkspace());

        reg.registerCodegen("noise", "c", getCodeGenCNoise());
        reg.registerCodegen("noise", "rust", getCodeGenRustNoise());
        // Counters gate their state advance through codegenSampleRate, so a
        // slower per-block SampleTime is honored in generated code.
        auto multirate = [](BlockCodegenTemplate t) {
            t.multirateCapable = true;
            return t;
        };
        reg.registerCodegen("counterFreeRunning", "c", multirate(getCodeGenCCounterFreeRunning()));
        reg.registerCodegen(
            "counterFreeRunning", "rust", multirate(getCodeGenRustCounterFreeRunning()));
        reg.registerCodegen("counterLimited", "c", multirate(getCodeGenCCounterLimited()));
        reg.registerCodegen("counterLimited", "rust", multirate(getCodeGenRustCounterLimited()));
        reg.registerCodegen("repeatingSequenceStair", "c", getCodeGenCRepeatingSequenceStair());
        reg.registerCodegen(
            "repeatingSequenceStair", "rust", getCodeGenRustRepeatingSequenceStair());
        reg.registerCodegen(
            "repeatingSequenceInterpolated", "c", getCodeGenCRepeatingSequenceInterpolated());
        reg.registerCodegen(
            "repeatingSequenceInterpolated", "rust", getCodeGenRustRepeatingSequenceInterpolated());
        reg.registerCodegen("signalGenerator", "c", getCodeGenCSignalGenerator());
        reg.registerCodegen("signalGenerator", "rust", getCodeGenRustSignalGenerator());
        reg.registerCodegen("labelSource", "c", getCodeGenCLabelSource());
        reg.registerCodegen("labelSource", "rust", getCodeGenRustLabelSource());
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
