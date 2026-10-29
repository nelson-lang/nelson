//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Registrar for the signal-routing blocks (routing/*.cpp): mux, demux,
// selector, reshape, concatenate and convert.
//
// It also holds the dimension/type-propagation capabilities of the
// width-generalized built-in blocks (registerVectorCapabilities).
//=============================================================================
#include "SimEngineTypes.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include "routing/routing_blocks.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    void
    registerRoutingHandlers(BlockRegistry& reg)
    {
        BlockMetadata feedthrough;
        feedthrough.directFeedthrough = true;

        reg.registerHandler("mux", handleMux, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler(
            "demux", handleDemux, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler(
            "selector", handleSelector, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler("reshape", handleReshape, { Phase::INIT, Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        reg.registerHandler(
            "concatenate", handleConcatenate, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler(
            "convert", handleConvert, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerCodegen("convert", "c", getCodeGenCConvert());
        reg.registerCodegen("convert", "rust", getCodeGenRustConvert());
        reg.registerHandler("initialCondition", handleInitialCondition, { Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        reg.registerCodegen("initialCondition", "c", getCodeGenCInitialCondition());
        reg.registerCodegen("initialCondition", "rust", getCodeGenRustInitialCondition());
        reg.registerHandler(
            "width", handleWidth, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler("signalConversion", handleSignalConversion, { Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        reg.registerCodegen("signalConversion", "c", getCodeGenCSignalConversion());
        reg.registerCodegen("signalConversion", "rust", getCodeGenRustSignalConversion());
        reg.registerHandler(
            "assignment", handleAssignment, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler(
            "dataStoreMemory", handleDataStoreMemory, { Phase::INIT }, "builtin.utility");
        BlockMetadata dsMeta;
        dsMeta.directFeedthrough = true;
        reg.registerHandler("dataStoreWrite", handleDataStoreWrite,
            { Phase::INIT, Phase::ALGEBRAIC }, "builtin.utility", dsMeta);
        reg.registerHandler(
            "dataStoreRead", handleDataStoreRead, { Phase::ALGEBRAIC }, "builtin.utility");
        reg.registerCodegen("dataStoreMemory",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCDataStoreMemory(), getCodeGenRustDataStoreMemory() });
        reg.registerCodegen("dataStoreWrite",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCDataStoreWrite(), getCodeGenRustDataStoreWrite() });
        reg.registerCodegen("dataStoreRead",
            BlockRegistry::BlockCodegenPair {
                getCodeGenCDataStoreRead(), getCodeGenRustDataStoreRead() });
        // Iterator-subsystem helpers (native only). iteratorNumber emits the
        // current iteration index in OUTPUT; iteratorCondition carries the While
        // loop predicate.
        reg.registerHandler(
            "iteratorNumber", handleIteratorNumber, { Phase::OUTPUT }, "builtin.utility");
        reg.registerHandler("iteratorCondition", handleIteratorCondition, { Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        // Only meaningful inside For/While iterator subsystems, which have no
        // code generation yet: an explicit unimplemented pair keeps the support
        // report truthful while generation rejects them.
        reg.registerCodegen("iteratorNumber",
            BlockRegistry::BlockCodegenPair {
                BlockCodegenTemplate::unimplemented(), BlockCodegenTemplate::unimplemented() });
        reg.registerCodegen("iteratorCondition",
            BlockRegistry::BlockCodegenPair {
                BlockCodegenTemplate::unimplemented(), BlockCodegenTemplate::unimplemented() });
        reg.registerHandler(
            "merge", handleMerge, { Phase::INIT, Phase::ALGEBRAIC }, "builtin.utility");
        reg.registerCodegen(
            "merge", BlockRegistry::BlockCodegenPair { getCodeGenCMerge(), getCodeGenRustMerge() });
        reg.registerHandler(
            "busCreator", handleBusCreator, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        reg.registerHandler(
            "busSelector", handleBusSelector, { Phase::ALGEBRAIC }, "builtin.utility", feedthrough);
        // The creator builds and validates the bus type (bus inputs allowed for
        // nesting); the selector resolves member paths.
        reg.registerHandler("busAssignment", handleBusAssignment, { Phase::ALGEBRAIC },
            "builtin.utility", feedthrough);
        reg.setDimsMetadata("busCreator", true);
        reg.setDimsMetadata("busSelector", true);
        reg.setDimsMetadata("busAssignment", true);
        reg.setBusMetadata("busCreator", true, resolveBusCreatorDims);
        reg.setBusMetadata("busSelector", true, resolveBusSelectorDims);
        reg.setBusMetadata("busAssignment", true, resolveBusAssignmentDims);

        registerMultiportSwitch(reg);
        // A label sink passes the packed region through untouched, so buses
        // may terminate on it directly; code generation re-emits nonvirtual
        // buses as struct-typed outputs there (virtual ones flatten).
        reg.setBusMetadata("labelSink", true, nullptr);
        // A bus mixes member types by definition.
        reg.setTypeMetadata("busCreator", true, -1, false);
        reg.setTypeMetadata("busSelector", true, -1, false);
        reg.setTypeMetadata("busAssignment", true, -1, false);
        reg.setExact64Support("busCreator");
        reg.setExact64Support("busSelector");
        reg.setExact64Support("busAssignment");
        reg.setComplexSupport("busCreator", true, 0);
        reg.setComplexSupport("busSelector", true, -1);
        reg.setComplexSupport("busAssignment", true, -1);

        // mux: one 1-D vector of width = sum of the input widths; every input
        // must share one signal type (an unconnected port counts as double).
        reg.setDimsMetadata("mux", true,
            [](const Block& b, const ValMap&, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                int total = 0;
                const int nIn = std::max(1, b.inputs);
                SigType unified = inSigs.empty() ? SigType::Double : inSigs[0].type;
                for (int p = 0; p < nIn; ++p) {
                    total += (p < (int)inSigs.size()) ? std::max(1, inSigs[p].width()) : 1;
                    if (p < (int)inSigs.size() && inSigs[p].type != unified) {
                        err = std::string("mux inputs mix signal types (") + sigTypeName(unified)
                            + " vs " + sigTypeName(inSigs[p].type) + "); insert a convert block";
                        return false;
                    }
                }
                outSigs[0].setVector(total);
                outSigs[0].type = unified;
                return true;
            });
        reg.setDimsMetadata("demux", true,
            [](const Block& b, const ValMap&, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                const int nOut = std::max(1, b.outputs);
                const SigType ty = inSigs.empty() ? SigType::Double : inSigs[0].type;
                for (auto& os : outSigs) {
                    os.type = ty;
                }
                const int w = inSigs.empty() ? 1 : std::max(1, inSigs[0].width());
                if (w <= 1) {
                    return true; // input not resolved yet (or scalar): keep as-is
                }
                if (w % nOut != 0) {
                    err = "input width " + std::to_string(w) + " is not divisible by "
                        + std::to_string(nOut) + " outputs";
                    return false;
                }
                for (int p = 0; p < nOut && p < (int)outSigs.size(); ++p) {
                    outSigs[p].setVector(w / nOut);
                }
                return true;
            });
        reg.setDimsMetadata("selector", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string&) {
                nflow::BlockDescriptor bd(b, vars);
                std::vector<double> idx = bd.paramList(nflow::kIndices);
                const int n = std::max<int>(1, (int)idx.size());
                outSigs[0].setVector(n);
                outSigs[0].type = inSigs.empty() ? SigType::Double : inSigs[0].type;
                return true;
            });
        reg.setDimsMetadata("convert", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                if (!inSigs.empty()) {
                    outSigs[0].copyShape(inSigs[0]);
                }
                nflow::BlockDescriptor bd(b, vars);
                SigType ty = SigType::Double;
                if (!sigTypeFromString(bd.paramStr(nflow::kOutDataType, "double"), ty)) {
                    err = "unknown OutDataType '" + bd.paramStr(nflow::kOutDataType, "") + "'";
                    return false;
                }
                outSigs[0].type = ty;
                return true;
            });
        // reshape: dims come from OutputDimensions ([d1 ... dN], [r c], or a
        // single n for 1-D). The element count is checked at INIT, since the
        // input width may not be resolved yet during propagation.
        reg.setDimsMetadata("reshape", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                nflow::BlockDescriptor bd(b, vars);
                outSigs[0].type = inSigs.empty() ? SigType::Double : inSigs[0].type;
                std::vector<double> dims = bd.paramList(nflow::kOutputDims);
                if (dims.empty()) {
                    const int w = inSigs.empty() ? 1 : std::max(1, inSigs[0].width());
                    outSigs[0].setVector(w);
                    return true;
                }
                if ((int)dims.size() > kMaxSignalDims) {
                    err = "OutputDimensions has " + std::to_string(dims.size())
                        + " entries; at most " + std::to_string(kMaxSignalDims)
                        + " dimensions are supported";
                    return false;
                }
                std::vector<int> od(dims.size());
                long long prod = 1;
                for (size_t k = 0; k < dims.size(); ++k) {
                    od[k] = std::max(1, (int)std::llround(dims[k]));
                    prod *= od[k];
                }
                const int w = inSigs.empty() ? 0 : inSigs[0].width();
                if (w > 1 && prod != w) {
                    std::string shape;
                    for (size_t k = 0; k < od.size(); ++k) {
                        if (k > 0) {
                            shape += "x";
                        }
                        shape += std::to_string(od[k]);
                    }
                    err = "reshape to [" + shape
                        + "] does not preserve the element count (input width " + std::to_string(w)
                        + ")";
                    return false;
                }
                if (dims.size() == 1) {
                    outSigs[0].setVector(od[0]);
                } else {
                    outSigs[0].setDims(od);
                }
                return true;
            });
        // concatenate along ConcatenateDimension (1 = rows, 2 = columns, >= 3 a
        // higher dimension). The other dimensions must match the first input
        // across non-scalar inputs. The output rank is the largest input rank,
        // and at least the concat dimension for dim >= 2 (stacking plain vectors
        // vertically stays 1-D).
        reg.setDimsMetadata("concatenate", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                nflow::BlockDescriptor bd(b, vars);
                int dim = (int)std::llround(bd.paramDouble(nflow::kConcatDim, 1.0));
                if (dim < 1) {
                    dim = 1;
                }
                if (dim > kMaxSignalDims) {
                    err = "ConcatenateDimension " + std::to_string(dim) + " exceeds the "
                        + std::to_string(kMaxSignalDims) + "-dimension limit";
                    return false;
                }
                const int nIn = std::max(1, b.inputs);
                const SigType unified = inSigs.empty() ? SigType::Double : inSigs[0].type;
                outSigs[0].type = unified;
                auto extent = [](const PortSig& s, int k) {
                    return (k <= s.numDims()) ? s.dims[k - 1] : 1;
                };
                int outRank = (dim >= 2) ? dim : 1;
                PortSig tmpl; // other-dims template: first input (scalars included)
                bool haveTmpl = false;
                int sumAlong = 0;
                for (int p = 0; p < nIn; ++p) {
                    PortSig in = (p < (int)inSigs.size()) ? inSigs[p] : PortSig {};
                    if (p < (int)inSigs.size() && in.type != unified) {
                        err = std::string("concatenate inputs mix signal types (")
                            + sigTypeName(unified) + " vs " + sigTypeName(in.type)
                            + "); insert a convert block";
                        return false;
                    }
                    if (in.width() > 1) {
                        outRank = std::max(outRank, in.numDims());
                    }
                    if (!haveTmpl) {
                        tmpl = in;
                        haveTmpl = true;
                    } else if (in.width() > 1) {
                        const int rank = std::max(outRank, tmpl.numDims());
                        for (int k = 1; k <= rank; ++k) {
                            if (k == dim) {
                                continue;
                            }
                            if (extent(in, k) != extent(tmpl, k)) {
                                if (k == 1) {
                                    err = "row counts differ across concatenated inputs";
                                } else if (k == 2) {
                                    err = "column counts differ across concatenated inputs";
                                } else {
                                    err = "dimension " + std::to_string(k)
                                        + " differs across concatenated inputs";
                                }
                                return false;
                            }
                        }
                    }
                    sumAlong += extent(in, dim);
                }
                std::vector<int> od((size_t)std::max(outRank, 1), 1);
                for (int k = 1; k <= outRank; ++k) {
                    od[(size_t)k - 1]
                        = (k == dim) ? std::max(1, sumAlong) : std::max(1, extent(tmpl, k));
                }
                if (outRank == 1) {
                    outSigs[0].setVector(od[0]);
                } else {
                    outSigs[0].setDims(od);
                }
                return true;
            });
    }
    //=============================================================================
    // Marks the width-generalized built-ins as vector-capable, attaches the dims
    // rules of the source blocks and declares the type-propagation capabilities.
    // Called once after all built-in registrations: the flags patch the stored
    // registry entries in place.
    void
    registerVectorCapabilities(BlockRegistry& reg)
    {
        static const char* elementwise[] = { "gain", "sum", "mult", "abs", "divide", "negate",
            "bias", "wrapToZero", "polynomial", "min", "max", "atan2", "mathFunction",
            "roundingFunction", "sign", "sqrt", "trigFunction", "and", "or", "xor", "not",
            "logicalOperator", "compareToConstant", "compareToZero", "relationalOperator",
            "saturation", "deadZone", "coulombViscousFriction", "quantizer", "switch", "integrator",
            "unitDelay", "difference", "zoh", "scope", "display", "terminator", "labelSink",
            "labelSource", "toWorkspace", "signalConversion" };
        for (const char* t : elementwise) {
            reg.setDimsMetadata(t, true);
        }
        // width takes a vector but emits a scalar (the element count), so the
        // element-wise default does not apply.
        reg.setDimsMetadata("width", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string&) {
                outSigs[0].setVector(1);
                outSigs[0].type = SigType::Double;
                return true;
            });
        // assignment: the output has the width of the base signal (port 0); the
        // replacement values on port 1 may be narrower.
        reg.setDimsMetadata("assignment", true,
            [](const Block&, const ValMap&, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string&) {
                const int w = (!inSigs.empty() && inSigs[0].width() > 0) ? inSigs[0].width() : 1;
                outSigs[0].setVector(w);
                outSigs[0].type = SigType::Double;
                return true;
            });
        reg.setDimsMetadata("constant", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string& err) {
                nflow::BlockDescriptor bd(b, vars);
                std::vector<double> lst = bd.paramList(nflow::kValue);
                const int n = std::max<int>(1, (int)lst.size());
                outSigs[0].setVector(n);
                SigType ty = SigType::Double;
                const std::string tname = bd.paramStr(nflow::kOutDataType, "double");
                if (!sigTypeFromString(tname, ty)) {
                    err = "unknown OutDataType '" + tname + "'";
                    return false;
                }
                outSigs[0].type = ty;
                return true;
            });

        // fromWorkspace: 'width' is injected at launch time from the workspace
        // variable (1 while unresolved).
        reg.setDimsMetadata("fromWorkspace", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>&,
                std::vector<PortSig>& outSigs, std::string& err) {
                (void)err;
                nflow::BlockDescriptor bd(b, vars);
                const int w = std::max(1, (int)bd.paramDouble(nflow::kWidth, 1.0));
                outSigs[0].setVector(w);
                outSigs[0].type = SigType::Double;
                // The launcher injects "isComplex" alongside the samples.
                outSigs[0].isComplex = asJson(b.params).is_object()
                    && asJson(b.params).contains("isComplex")
                    && asJson(b.params)["isComplex"].is_boolean()
                    && asJson(b.params)["isComplex"].get<bool>();
                return true;
            });

        // nelsonFunction: OutputDimensions -1 inherits the input width (assumes
        // element-wise); an explicit value fixes the port width.
        reg.setDimsMetadata("nelsonFunction", true,
            [](const Block& b, const ValMap& vars, const std::vector<PortSig>& inSigs,
                std::vector<PortSig>& outSigs, std::string& err) {
                (void)err;
                nflow::BlockDescriptor bd(b, vars);
                const int declared = (int)bd.paramDouble(nflow::kOutputDims, -1.0);
                int w = declared > 0 ? declared
                                     : (!inSigs.empty() ? std::max(1, inSigs[0].width()) : 1);
                outSigs[0].setVector(w);
                outSigs[0].type = SigType::Double;
                return true;
            });

        // Type capabilities of the width-generalized built-ins:
        // - logic and comparison blocks accept any input mix and emit Boolean;
        // - switch/scope/display/terminator/labelSink accept any mix;
        // - continuous-dynamics blocks require double signals.
        for (const char* t : { "and", "or", "xor", "not", "logicalOperator", "compareToConstant",
                 "compareToZero", "relationalOperator" }) {
            reg.setTypeMetadata(t, true, (int)SigType::Boolean, false);
        }
        for (const char* t : { "switch", "scope", "display", "terminator", "labelSink", "xyScope",
                 "xyzScope", "fileSink", "toWorkspace" }) {
            reg.setTypeMetadata(t, true, -1, false);
        }
        for (const char* t : { "integrator", "tf", "stateSpace", "lpf", "hpf", "pid", "derivative",
                 "delay", "dtf", "ddelay", "dstateSpace", "foh", "rate", "backlash" }) {
            reg.setTypeMetadata(t, false, -1, true);
        }
        // Blocks with an exact int64/uint64 path (64-bit lane, integer-space
        // arithmetic or lane-preserving copies). Anything not listed rejects
        // exact-64 inputs at compile time. scope/display/terminator consume
        // the double projection (display-grade; the result JSON stays exact).
        for (const char* t :
            { "constant", "convert", "gain", "sum", "mult", "divide", "negate", "abs", "bias",
                "min", "max", "compareToConstant", "compareToZero", "relationalOperator", "switch",
                "mux", "demux", "selector", "reshape", "concatenate", "scope", "display",
                "terminator", "labelSink", "labelSource", "subsystem", "toWorkspace" }) {
            reg.setExact64Support(t);
        }
        // Blocks that understand the imaginary lane (complex-aware math and
        // lane-preserving routing). abs consumes complex but emits the real
        // magnitude (forcesComplexOut = 0). scope/display/terminator show
        // the real part; the result JSON carries both parts exactly.
        for (const char* t : { "gain", "sum", "mult", "divide", "negate", "switch", "mux", "demux",
                 "selector", "reshape", "concatenate", "scope", "display", "terminator",
                 "labelSink", "labelSource" }) {
            reg.setComplexSupport(t, true, -1);
        }
        reg.setComplexSupport("abs", true, 0);
        // subsystem: outputs adopt the inner sink's descriptor (rule-owned
        // complexity, value 2 = no OR-propagation overwrite).
        reg.setComplexSupport("subsystem", true, 2);
        // fromWorkspace: the port's complexity comes from the injected
        // workspace variable (rule-owned). toWorkspace / nelsonFunction log
        // and evaluate complex signals through the bridge lanes.
        reg.setComplexSupport("fromWorkspace", false, 2);
        reg.setComplexSupport("toWorkspace", true, -1);
        reg.setComplexSupport("nelsonFunction", true, -1);
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
