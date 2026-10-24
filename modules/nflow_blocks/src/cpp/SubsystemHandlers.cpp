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
#include "SubsystemInternal.hpp"
#include "StateLayout.hpp"
#include "EdgeDetect.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    BlockCodegenTemplate
    getCodeGenCSubsystem();
    BlockCodegenTemplate
    getCodeGenRustSubsystem();
    //=============================================================================
    // Overlay params.maskParameters (name -> numeric value) onto the parent variables,
    // so a masked subsystem resolves its inner block params to its own values.
    // Non-numeric values are ignored.
    static std::unordered_map<std::string, double>
    mergeMaskParameters(
        const std::unordered_map<std::string, double>& parentVars, const nlohmann::json& params)
    {
        std::unordered_map<std::string, double> merged = parentVars;
        if (params.contains("maskParameters") && params["maskParameters"].is_object()) {
            for (const auto& [key, value] : params["maskParameters"].items()) {
                if (value.is_number()) {
                    merged[key] = value.get<double>();
                }
            }
        }
        return merged;
    }
    //=============================================================================
    // Parse params["forEach"]. When numIterations > 1, divide each partitioned input
    // port's width (and descriptor) by the count, so the inner body is built at the
    // per-iteration width. Returns the count (1 = ordinary subsystem), or -1 with err
    // set when a partitioned width is not a multiple of numIterations.
    // isPartition[outerPort] is filled either way.
    static int
    forEachInputDivision(const json& params, int nInputPorts, std::vector<int>& inW,
        std::vector<PortSig>* inSigs, std::vector<char>& isPartition, std::string& err)
    {
        isPartition.assign((size_t)std::max(0, nInputPorts), 0);
        if (!params.contains("forEach") || !params["forEach"].is_object()) {
            return 1;
        }
        const json& fe = params["forEach"];
        int n = 1;
        if (fe.contains("numIterations") && fe["numIterations"].is_number()) {
            n = (int)std::llround(fe["numIterations"].get<double>());
        }
        if (n <= 1) {
            return 1;
        }
        if (fe.contains("partition") && fe["partition"].is_array()) {
            for (const auto& pj : fe["partition"]) {
                if (pj.is_number()) {
                    const int p = (int)std::llround(pj.get<double>());
                    if (p >= 0 && p < nInputPorts) {
                        isPartition[(size_t)p] = 1;
                    }
                }
            }
        }
        for (int p = 0; p < nInputPorts && p < (int)inW.size(); ++p) {
            if (!isPartition[(size_t)p]) {
                continue;
            }
            if (inW[p] <= 0 || inW[p] % n != 0) {
                err = "for-each: partitioned input port " + std::to_string(p) + " width ("
                    + std::to_string(inW[p]) + ") is not a positive multiple of numIterations ("
                    + std::to_string(n) + ")";
                return -1;
            }
            inW[p] /= n;
            if (inSigs && p < (int)inSigs->size()) {
                (*inSigs)[(size_t)p].setVector(std::max(1, inW[p]));
            }
        }
        return n;
    }
    //=============================================================================
    // For-each supports a real double body. A stateless body shares one inner state.
    // A state-bearing body gets one replica per iteration (feReplicas): discrete state
    // in blockStateVec, continuous state in StateLayout (N * nx_inner global slots).
    // Complex outputs are rejected. Sets hasState when the body carries any
    // per-iteration state, so INIT builds the replicas.
    static bool
    forEachBodyIsSupported(const BlockState::SubsystemState& ss, bool& hasState, std::string& err)
    {
        auto& reg = BlockRegistry::instance();
        hasState = false;
        for (const Block& ib : ss.blocks) {
            if (ib.type == "subsystem") {
                err = "for-each: a nested subsystem in the body is not supported yet";
                return false;
            }
            const BlockMetadata md = reg.metadataOf(ib.type);
            // State is advanced in a continuous phase (DERIVATIVE) or a discrete one (UPDATE /
            // AFTER_STEP). The state-count metadata is often left at the default, so check
            // phase membership too.
            if (md.numContinuousStates > 0 || md.continuousStateCount != nullptr
                || md.numDiscreteStates > 0 || reg.hasPhase(ib.type, Phase::DERIVATIVE)
                || reg.hasPhase(ib.type, Phase::UPDATE)
                || reg.hasPhase(ib.type, Phase::AFTER_STEP)) {
                hasState = true;
            }
        }
        for (const PortSig& s : ss.extOutSig) {
            if (s.isComplex) {
                err = "for-each: complex outputs are not supported yet";
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    static void
    runSubsystemAlgebraic(BlockState::SubsystemState& ss, const std::vector<double>& outerInputs,
        const std::unordered_map<std::string, double>& vars, double dt, double t,
        const std::vector<double>* outerInputsImag = nullptr,
        const std::vector<long long>* outerInputsI64 = nullptr, const SimCtx* outer = nullptr,
        Phase contPhase = Phase::OUTPUT)
    {
        auto& reg = BlockRegistry::instance();

        SimCtx innerCtx { ss.blocks, ss.inputIds, ss.outputBase, ss.labelSinks, ss.nameToId,
            ss.blockStateVec, ss.outputsVec, vars, dt, t };
        innerCtx.sigs = &ss.sigs;
        innerCtx.portBase = &ss.portBase;
        innerCtx.inputSigs = &ss.inputSigs;
        if (!ss.outputsImagVec.empty()) {
            innerCtx.outputsImag = &ss.outputsImagVec;
        }
        if (!ss.outputsI64Vec.empty()) {
            innerCtx.outputsI64 = &ss.outputsI64Vec;
        }
        // Under a global solver the inner blocks share the global x/xdot/g vectors
        // (StateLayout recursed into ss.blockStateVec). Null on the legacy fixed-step
        // path.
        if (outer != nullptr) {
            innerCtx.x = outer->x;
            innerCtx.xdot = outer->xdot;
            innerCtx.g = outer->g;
            innerCtx.nx = outer->nx;
            // A nested physicalIsland needs the integration mode and the side-effect-free
            // marker: its implicit advance fires once per committed major step, never inside
            // a solver rhs / roots evaluation.
            innerCtx.sideEffectFree = outer->sideEffectFree;
            innerCtx.islandImplicitStep = outer->islandImplicitStep;
        }
        // Iterator subsystems expose the current iteration to the body (read by
        // iteratorNumber). 0 for a normal subsystem.
        innerCtx.iterationIndex = ss.iterIndex;
        innerCtx.iterationCount = ss.iterCount;

        std::fill(ss.outputsVec.begin(), ss.outputsVec.end(), 0.0);

        // Warm-start OUTPUT-phase blocks (full port-0 slice when vector-latched).
        for (int i : ss.outputIdx) {
            int nid = ss.blocks[i].nid;
            const BlockState& bst = ss.blockStateVec[nid];
            const int wsz = ss.sigs[ss.portBase[nid]].width();
            if (!bst.outLatch.empty()) {
                const int nn = std::min<int>(wsz, (int)bst.outLatch.size());
                for (int k = 0; k < nn; ++k) {
                    ss.outputsVec[ss.outputBase[nid] + k] = bst.outLatch[k];
                }
            } else {
                ss.outputsVec[ss.outputBase[nid]] = bst.output;
            }
        }

        // outerInputs is the flat concatenation of the external-input slices. The complex
        // and int64 lanes are marshalled alongside when present.
        auto applyExternalInputs = [&]() {
            for (const auto& [blockId, outerIdx] : ss.inputLookup) {
                auto nit = ss.nameToId.find(blockId);
                if (nit == ss.nameToId.end() || outerIdx < 0
                    || outerIdx >= (int)ss.extInWidth.size()) {
                    continue;
                }
                const int off = ss.extInOffset[outerIdx];
                const int wsz = ss.extInWidth[outerIdx];
                const int base = ss.outputBase[nit->second];
                double* dst = ss.outputsVec.data() + base;
                double* dstIm
                    = ss.outputsImagVec.empty() ? nullptr : ss.outputsImagVec.data() + base;
                long long* dstI
                    = ss.outputsI64Vec.empty() ? nullptr : ss.outputsI64Vec.data() + base;
                for (int k = 0; k < wsz; ++k) {
                    dst[k] = (off + k < (int)outerInputs.size()) ? outerInputs[off + k] : 0.0;
                    if (dstIm) {
                        dstIm[k] = (outerInputsImag && off + k < (int)outerInputsImag->size())
                            ? (*outerInputsImag)[off + k]
                            : 0.0;
                    }
                    if (dstI) {
                        dstI[k] = (outerInputsI64 && off + k < (int)outerInputsI64->size())
                            ? (*outerInputsI64)[off + k]
                            : (long long)dst[k];
                    }
                }
            }
        };

        applyExternalInputs();

        for (int i : ss.outputIdx) {
            auto fn = reg.getHandler(ss.blocks[i].type);
            if (fn) {
                fn(innerCtx, ss.blocks[i], Phase::OUTPUT);
            }
        }

        auto doAlgebraic = [&]() {
            for (int i : ss.algOrdered) {
                auto fn = reg.getHandler(ss.blocks[i].type);
                if (fn) {
                    fn(innerCtx, ss.blocks[i], Phase::ALGEBRAIC);
                }
            }
        };

        if (!ss.hasLabelBlocks && !ss.algHasCycle) {
            doAlgebraic();
        } else {
            bool progress = true;
            int iter = 0;
            while (progress && iter < 50) {
                ++iter;
                progress = false;
                if (ss.hasLabelBlocks) {
                    for (const Block& b : ss.blocks) {
                        if (b.type != nflow::kLabelSource) {
                            continue;
                        }
                        if (ss.inputLookup.count(b.id)) {
                            continue;
                        }
                        if (!b.params.contains(nflow::kGotoTag)
                            || !b.params[nflow::kGotoTag].is_string()) {
                            continue;
                        }
                        std::string name = b.params[nflow::kGotoTag].get<std::string>();
                        auto sit = ss.labelSinks.find(name);
                        if (sit == ss.labelSinks.end()) {
                            continue;
                        }
                        auto sinkNidIt = ss.nameToId.find(sit->second);
                        if (sinkNidIt == ss.nameToId.end()) {
                            continue;
                        }
                        BlockId sinkNid = sinkNidIt->second;
                        if (ss.inputIds[sinkNid].empty() || ss.inputIds[sinkNid][0] < 0) {
                            continue;
                        }
                        const int srcSrcSlot = ss.inputIds[sinkNid][0];
                        auto srcNidIt = ss.nameToId.find(b.id);
                        if (srcNidIt != ss.nameToId.end()) {
                            const int srcSlot = ss.outputBase[srcNidIt->second];
                            const int wsz = ss.sigs[ss.portBase[srcNidIt->second]].width();
                            for (int k = 0; k < wsz; ++k) {
                                double prev = ss.outputsVec[srcSlot + k];
                                ss.outputsVec[srcSlot + k] = ss.outputsVec[srcSrcSlot + k];
                                if (prev != ss.outputsVec[srcSlot + k]) {
                                    progress = true;
                                }
                            }
                        }
                    }
                }
                applyExternalInputs();
                for (int i : ss.algOrdered) {
                    BlockId nid = ss.blocks[i].nid;
                    // Compare every output slot, not just port 0: a later port of a multi-output
                    // block can change and must keep the iteration alive.
                    const int outSlot = ss.outputBase[nid];
                    const int nOut = std::max(1, ss.blocks[i].outputs);
                    std::vector<double> prevOut(
                        ss.outputsVec.begin() + outSlot, ss.outputsVec.begin() + outSlot + nOut);
                    auto fn = reg.getHandler(ss.blocks[i].type);
                    if (fn) {
                        fn(innerCtx, ss.blocks[i], Phase::ALGEBRAIC);
                    }
                    for (int p = 0; p < nOut; ++p) {
                        if (ss.outputsVec[outSlot + p] != prevOut[p]) {
                            progress = true;
                            break;
                        }
                    }
                }
                if (ss.hasLabelBlocks) {
                    for (const Block& b : ss.blocks) {
                        if (b.type != nflow::kLabelSource) {
                            continue;
                        }
                        if (ss.inputLookup.count(b.id)) {
                            continue;
                        }
                        if (!b.params.contains(nflow::kGotoTag)
                            || !b.params[nflow::kGotoTag].is_string()) {
                            continue;
                        }
                        std::string name = b.params[nflow::kGotoTag].get<std::string>();
                        auto sit = ss.labelSinks.find(name);
                        if (sit == ss.labelSinks.end()) {
                            continue;
                        }
                        auto sinkNidIt = ss.nameToId.find(sit->second);
                        if (sinkNidIt == ss.nameToId.end()) {
                            continue;
                        }
                        BlockId sinkNid = sinkNidIt->second;
                        if (ss.inputIds[sinkNid].empty() || ss.inputIds[sinkNid][0] < 0) {
                            continue;
                        }
                        const int srcSrcSlot = ss.inputIds[sinkNid][0];
                        auto srcNidIt = ss.nameToId.find(b.id);
                        if (srcNidIt != ss.nameToId.end()) {
                            const int srcSlot = ss.outputBase[srcNidIt->second];
                            const int wsz = ss.sigs[ss.portBase[srcNidIt->second]].width();
                            for (int k = 0; k < wsz; ++k) {
                                double prev = ss.outputsVec[srcSlot + k];
                                ss.outputsVec[srcSlot + k] = ss.outputsVec[srcSrcSlot + k];
                                if (prev != ss.outputsVec[srcSlot + k]) {
                                    progress = true;
                                }
                            }
                        }
                    }
                }
                applyExternalInputs();
            }
        }

        // Continuous dispatch: after OUTPUT/ALGEBRAIC at the scattered global state, run
        // the requested continuous phase so each inner block writes its DERIVATIVE (xdot)
        // or ZERO_CROSSING (g) surface into its global slice. A nested subsystem re-enters
        // this handler, so any depth works.
        if (contPhase == Phase::DERIVATIVE || contPhase == Phase::ZERO_CROSSING) {
            for (int i = 0; i < (int)ss.blocks.size(); ++i) {
                if (reg.hasPhase(ss.blocks[i].type, contPhase)) {
                    auto fn = reg.getHandler(ss.blocks[i].type);
                    if (fn) {
                        fn(innerCtx, ss.blocks[i], contPhase);
                    }
                }
            }
        }

        for (const auto& [sinkId, outIdx] : ss.outputLookup) {
            if (outIdx < 0 || outIdx >= (int)ss.extOutWidth.size()) {
                continue;
            }
            const int off = ss.extOutOffset[outIdx];
            const int wsz = ss.extOutWidth[outIdx];
            auto nit = ss.nameToId.find(sinkId);
            int srcSlot = -1;
            if (nit != ss.nameToId.end()) {
                const BlockId sinkNid = nit->second;
                if (!ss.inputIds[sinkNid].empty() && ss.inputIds[sinkNid][0] >= 0) {
                    srcSlot = ss.inputIds[sinkNid][0];
                } else {
                    srcSlot = ss.outputBase[sinkNid];
                }
            }
            const double* src = (srcSlot >= 0) ? ss.outputsVec.data() + srcSlot : nullptr;
            const double* srcIm = (srcSlot >= 0 && !ss.outputsImagVec.empty())
                ? ss.outputsImagVec.data() + srcSlot
                : nullptr;
            const long long* srcI = (srcSlot >= 0 && !ss.outputsI64Vec.empty())
                ? ss.outputsI64Vec.data() + srcSlot
                : nullptr;
            for (int k = 0; k < wsz && off + k < (int)ss.lastValues.size(); ++k) {
                ss.lastValues[off + k] = src ? src[k] : 0.0;
                if (!ss.lastValuesImag.empty()) {
                    ss.lastValuesImag[off + k] = srcIm ? srcIm[k] : 0.0;
                }
                if (!ss.lastValuesI64.empty()) {
                    ss.lastValuesI64[off + k] = srcI ? srcI[k] : (long long)ss.lastValues[off + k];
                }
            }
        }
    }
    //=============================================================================
    static void
    advanceSubsystemState(BlockState::SubsystemState& ss,
        const std::unordered_map<std::string, double>& vars, double dt, double t,
        const SimCtx* outer = nullptr)
    {
        auto& reg = BlockRegistry::instance();

        SimCtx innerCtx { ss.blocks, ss.inputIds, ss.outputBase, ss.labelSinks, ss.nameToId,
            ss.blockStateVec, ss.outputsVec, vars, dt, t };
        innerCtx.sigs = &ss.sigs;
        innerCtx.portBase = &ss.portBase;
        innerCtx.inputSigs = &ss.inputSigs;
        if (!ss.outputsImagVec.empty()) {
            innerCtx.outputsImag = &ss.outputsImagVec;
        }
        if (!ss.outputsI64Vec.empty()) {
            innerCtx.outputsI64 = &ss.outputsI64Vec;
        }
        // Under a global solver the solver integrates the inner continuous blocks (their
        // DERIVATIVE writes global xdot), so their fixed-step UPDATE must not run here or
        // the state would advance twice. Inner discrete blocks still update at the sample
        // time.
        const bool underSolver = (outer != nullptr && outer->x != nullptr);
        if (underSolver) {
            innerCtx.x = outer->x;
            innerCtx.xdot = outer->xdot;
            innerCtx.g = outer->g;
            innerCtx.nx = outer->nx;
        }
        if (outer != nullptr) {
            // A doubly-nested physicalIsland reads the integration mode from its own context.
            innerCtx.sideEffectFree = outer->sideEffectFree;
            innerCtx.islandImplicitStep = outer->islandImplicitStep;
        }
        innerCtx.iterationIndex = ss.iterIndex;
        innerCtx.iterationCount = ss.iterCount;

        for (int i : ss.afterStepIdx) {
            auto fn = reg.getHandler(ss.blocks[i].type);
            if (fn) {
                fn(innerCtx, ss.blocks[i], Phase::AFTER_STEP);
            }
        }
        for (int i : ss.updateIdx) {
            // A nested subsystem, a pid and a resettable integrator declare a DERIVATIVE, but
            // their UPDATE advances no continuous state. It latches discrete bookkeeping (the
            // gate level of a conditional subsystem, the control value an edge is measured
            // against), so it must still run under a solver. The top-level loop makes the same
            // exception.
            const bool innerBookkeeping = ss.blocks[i].type == "subsystem"
                || ss.blocks[i].type == "pid"
                || (ss.blocks[i].type == "integrator"
                    && ss.blocks[i].params.contains("ExternalReset")
                    && ss.blocks[i].params["ExternalReset"].is_string()
                    && ss.blocks[i].params["ExternalReset"].get<std::string>() != "none");
            if (underSolver && !innerBookkeeping
                && reg.hasPhase(ss.blocks[i].type, Phase::DERIVATIVE)) {
                continue; // continuous inner block: integrated by the solver
            }
            auto fn = reg.getHandler(ss.blocks[i].type);
            if (fn) {
                fn(innerCtx, ss.blocks[i], Phase::UPDATE);
            }
        }
    }
    //=============================================================================
    // Re-run the inner INIT to reset every inner block to its initial condition
    // (resettable subsystem). Layout, offsets and nested subsystems are preserved.
    static void
    resetSubsystemStates(BlockState::SubsystemState& ss,
        const std::unordered_map<std::string, double>& vars, double dt)
    {
        auto& reg = BlockRegistry::instance();
        SimCtx innerCtx { ss.blocks, ss.inputIds, ss.outputBase, ss.labelSinks, ss.nameToId,
            ss.blockStateVec, ss.outputsVec, vars, dt, 0.0 };
        innerCtx.sigs = &ss.sigs;
        innerCtx.portBase = &ss.portBase;
        innerCtx.inputSigs = &ss.inputSigs;
        if (!ss.outputsImagVec.empty()) {
            innerCtx.outputsImag = &ss.outputsImagVec;
        }
        if (!ss.outputsI64Vec.empty()) {
            innerCtx.outputsI64 = &ss.outputsI64Vec;
        }
        for (int i : ss.initIdx) {
            auto fn = reg.getHandler(ss.blocks[i].type);
            if (fn) {
                fn(innerCtx, ss.blocks[i], Phase::INIT);
            }
        }
    }
    //=============================================================================
    // Flatten the outer input-port slices into one vector ordered by external-input
    // index (extInOffset/extInWidth), plus the complex / int64 lanes when active.
    // Shared by the ALGEBRAIC / DERIVATIVE / ZERO_CROSSING phases.
    static void
    gatherSubsystemInputs(SimCtx& ctx, const Block& b, BlockState::SubsystemState& ss,
        std::vector<double>& inVals, std::vector<double>& inValsImag,
        std::vector<long long>& inValsI64)
    {
        int inTotal = 0;
        for (int w : ss.extInWidth) {
            inTotal += w;
        }
        const bool wantImag = !ss.outputsImagVec.empty();
        const bool wantI64 = !ss.outputsI64Vec.empty();
        inVals.assign(std::max(1, inTotal), 0.0);
        inValsImag.assign(wantImag ? std::max(1, inTotal) : 0, 0.0);
        inValsI64.assign(wantI64 ? std::max(1, inTotal) : 0, 0);
        for (int p = 0; p < (int)ss.extInWidth.size(); ++p) {
            const int off = ss.extInOffset[p];
            SigView u = getInputSig(ctx, b.nid, p);
            for (int k = 0; k < ss.extInWidth[p]; ++k) {
                inVals[off + k] = sigAt(u, k);
                if (wantImag) {
                    inValsImag[off + k]
                        = (u.imag && u.width > 0) ? ((u.width == 1) ? u.imag[0] : u.imag[k]) : 0.0;
                }
                if (wantI64) {
                    inValsI64[off + k] = u.idata ? ((u.width == 1) ? u.idata[0] : u.idata[k])
                                                 : (long long)inVals[off + k];
                }
            }
        }
    }
    //=============================================================================
    // The edge rules live in EdgeDetect.hpp, shared by every block that watches a
    // control signal.
    using Nelson::NFlow::edgeFired;
    using Nelson::NFlow::edgeZeroSideLatch;
    //=============================================================================
    // For-each: the inner-state holder for iteration i. Iteration 0 uses the block's
    // own SubsystemState, the others use their replica. A stateless body has no
    // replicas, so every iteration shares ss.
    static BlockState::SubsystemState&
    feHolder(BlockState::SubsystemState& ss, int i)
    {
        if (i <= 0 || ss.feReplicas.empty()) {
            return ss;
        }
        const int idx = i - 1;
        if (idx < (int)ss.feReplicas.size() && ss.feReplicas[(size_t)idx]) {
            return *ss.feReplicas[(size_t)idx];
        }
        return ss;
    }
    //=============================================================================
    // For-each: build iteration i's flat inner-input vector. A partitioned input
    // contributes its i-th slice, a broadcast input its whole slice, laid out by
    // extInOffset.
    static void
    gatherForEachIterationInputs(SimCtx& ctx, const Block& b, const BlockState::SubsystemState& ss,
        int i, std::vector<double>& inVals)
    {
        int inTotal = 0;
        for (int w : ss.extInWidth) {
            inTotal += w;
        }
        inVals.assign((size_t)std::max(1, inTotal), 0.0);
        const int nIn = (int)ss.extInWidth.size();
        for (int p = 0; p < nIn; ++p) {
            const int off = ss.extInOffset[p];
            const int w = ss.extInWidth[p];
            SigView u = getInputSig(ctx, b.nid, p);
            const bool part = (p < (int)ss.feIsPartition.size() && ss.feIsPartition[(size_t)p]);
            const int base = part ? i * w : 0;
            for (int kk = 0; kk < w; ++kk) {
                inVals[(size_t)(off + kk)] = sigAt(u, base + kk);
            }
        }
    }
    //=============================================================================
    // For-each: run the body ss.feNumIter times and concatenate the inner outputs into
    // ss.feConcat, laid out per outer output port (feNumIter * innerWidth each). Real
    // double only. Each iteration runs against its own state holder, so a discrete
    // per-channel body keeps independent history.
    static void
    runForEach(SimCtx& ctx, const Block& b, BlockState::SubsystemState& ss, const SimCtx* outer)
    {
        const int nIter = ss.feNumIter;
        const int nOut = (int)ss.extOutWidth.size();
        int catTotal = 0;
        std::vector<int> feOff((size_t)std::max(0, nOut), 0);
        for (int k = 0; k < nOut; ++k) {
            feOff[(size_t)k] = catTotal;
            catTotal += nIter * std::max(0, ss.extOutWidth[k]);
        }
        ss.feConcat.assign((size_t)std::max(1, catTotal), 0.0);

        std::vector<double> inVals;
        for (int i = 0; i < nIter; ++i) {
            gatherForEachIterationInputs(ctx, b, ss, i, inVals);
            // Layout metadata (extIn/Out offsets, widths) is identical across replicas, so it
            // is read from ss. The pass and its outputs come from the holder.
            BlockState::SubsystemState& holder = feHolder(ss, i);
            runSubsystemAlgebraic(
                holder, inVals, ctx.variables, ctx.dt, ctx.t, nullptr, nullptr, outer);
            for (int k = 0; k < nOut; ++k) {
                const int io = ss.extOutOffset[k];
                const int w = ss.extOutWidth[k];
                for (int kk = 0; kk < w; ++kk) {
                    if (io + kk < (int)holder.lastValues.size()) {
                        ss.feConcat[(size_t)(feOff[(size_t)k] + i * w + kk)]
                            = holder.lastValues[(size_t)(io + kk)];
                    }
                }
            }
        }
    }
    //=============================================================================
    static void
    scatterForEach(SimCtx& ctx, const Block& b, BlockState::SubsystemState& ss)
    {
        const int nIter = ss.feNumIter;
        const int nOut = (int)ss.extOutWidth.size();
        int catOff = 0;
        for (int k = 0; k < nOut && k < std::max(1, b.outputs); ++k) {
            const int w = nIter * std::max(0, ss.extOutWidth[k]);
            double* dst = outputSlice(ctx, b.nid, k);
            const int wk = std::min(outputWidth(ctx, b.nid, k), w);
            for (int kk = 0; kk < wk && catOff + kk < (int)ss.feConcat.size(); ++kk) {
                dst[kk] = ss.feConcat[(size_t)(catOff + kk)];
            }
            catOff += w;
        }
    }
    //=============================================================================
    static bool
    handleSubsystem(SimCtx& ctx, const Block& b, Phase phase)
    {
        auto& st = getState(ctx, b.nid);

        if (phase == Phase::INIT) {
            const json* specPtr = nullptr;
            if (b.params.contains(nflow::kSubsystem) && b.params[nflow::kSubsystem].is_object()) {
                specPtr = &b.params[nflow::kSubsystem];
            }
            if (!specPtr) {
                return false;
            }
            // Outer widths and full descriptors, so typed and complex signals cross in.
            std::vector<int> outerInWidths(std::max(0, b.inputs), 1);
            std::vector<PortSig> outerInSigs(std::max(0, b.inputs));
            for (int i = 0; i < (int)outerInWidths.size(); ++i) {
                outerInWidths[i] = b.inWidth(i);
                if (ctx.inputSigs && ctx.sigs && b.nid < (int)ctx.inputSigs->size()) {
                    const auto& sIds = (*ctx.inputSigs)[b.nid];
                    if (i < (int)sIds.size() && sIds[i] >= 0 && sIds[i] < (int)ctx.sigs->size()) {
                        outerInSigs[(size_t)i] = (*ctx.sigs)[sIds[i]];
                    }
                }
            }
            // For-each: build the inner body at the per-iteration width. feNumIter == 1 leaves
            // outerInWidths untouched.
            std::string feErr;
            std::vector<char> feIsPartition;
            const int feNumIter = forEachInputDivision(b.params, (int)outerInWidths.size(),
                outerInWidths, &outerInSigs, feIsPartition, feErr);
            // Mask parameters scope over the parent variables, so each instance resolves its
            // inner block params to its own values.
            const std::unordered_map<std::string, double> innerVars
                = mergeMaskParameters(ctx.variables, b.params);
            std::string innerLayoutErr;
            st.subsystem = buildSubsystemInnerState(
                *specPtr, innerVars, ctx.dt, outerInWidths, &outerInSigs, &innerLayoutErr);
            if (!innerLayoutErr.empty() && ctx.diag) {
                // Same refusal as the top level: an invalid inner layout must not be simulated.
                SimDiagnostic d;
                d.code = "subsystem_inner_layout";
                d.blockId = b.id;
                d.message = "subsystem '" + b.id + "' inner diagram: " + innerLayoutErr;
                d.time = ctx.t;
                ctx.diag->fail(d);
                return false;
            }
            if (feNumIter > 1) {
                st.subsystem->feNumIter = feNumIter;
                st.subsystem->feIsPartition = feIsPartition;
                // A discrete-state body needs one inner-state replica per iteration (iteration 0
                // uses st.subsystem itself). A stateless body shares the one state. Unsupported
                // bodies were already rejected by resolveDims.
                bool feHasState = false;
                std::string feSupportErr;
                if (forEachBodyIsSupported(*st.subsystem, feHasState, feSupportErr) && feHasState) {
                    for (int r = 1; r < feNumIter; ++r) {
                        st.subsystem->feReplicas.push_back(buildSubsystemInnerState(
                            *specPtr, innerVars, ctx.dt, outerInWidths, &outerInSigs));
                    }
                    // Continuous inner state needs the DERIVATIVE / ZERO_CROSSING pass per
                    // iteration. A discrete-only body advances in UPDATE.
                    auto& reg = BlockRegistry::instance();
                    for (const Block& ib : st.subsystem->blocks) {
                        if (reg.hasPhase(ib.type, Phase::DERIVATIVE)) {
                            st.subsystem->feHasContinuous = true;
                            break;
                        }
                    }
                }
            }
            // Conditional execution: the control signal is wired to an outer input port
            // appended after the data ports (index = number of external data inputs). No
            // controlPort means always active.
            //
            // The spec's control port is normally mirrored in the block params, but a spec
            // exported through the block packager keeps it at its own root or under its own
            // params. Read all three, otherwise the gate is dropped silently and the subsystem
            // runs unconditionally.
            json cpFound;
            if (b.params.contains("controlPort") && b.params["controlPort"].is_object()) {
                cpFound = b.params["controlPort"];
            }
            if (!(cpFound.is_object() && !cpFound.empty()) && b.params.contains("subsystem")
                && b.params["subsystem"].is_object()) {
                const json& spec = b.params["subsystem"];
                if (spec.contains("controlPort") && spec["controlPort"].is_object()) {
                    cpFound = spec["controlPort"];
                } else if (spec.contains("params") && spec["params"].is_object()
                    && spec["params"].contains("controlPort")
                    && spec["params"]["controlPort"].is_object()) {
                    cpFound = spec["params"]["controlPort"];
                }
            }
            if (cpFound.is_object() && !cpFound.empty()) {
                const json& cp = cpFound;
                const std::string kind = cp.value("kind", std::string());
                const int nData = (int)st.subsystem->extInWidth.size();
                const std::string tt = cp.value("triggerType", std::string("rising"));
                const int ttCode = (tt == "falling") ? 1 : ((tt == "either") ? 2 : 0);
                if (kind == "enable") {
                    st.subsystem->ctrlKind = 1;
                    st.subsystem->ctrlPortIdx = nData;
                } else if (kind == "trigger") {
                    st.subsystem->ctrlKind = 2;
                    st.subsystem->ctrlPortIdx = nData;
                    st.subsystem->triggerType = ttCode;
                } else if (kind == "enableTrigger") {
                    st.subsystem->ctrlKind = 3;
                    st.subsystem->ctrlPortIdx = nData; // enable port
                    st.subsystem->ctrlPortIdx2 = nData + 1; // trigger port
                    st.subsystem->triggerType = ttCode;
                } else if (kind == "reset") {
                    st.subsystem->ctrlKind = 4;
                    st.subsystem->ctrlPortIdx = nData; // reset port
                    const std::string rt = cp.value("resetTriggerType", std::string("rising"));
                    st.subsystem->triggerType = (rt == "falling") ? 1 : ((rt == "either") ? 2 : 0);
                } else if (kind == "action") {
                    // Action subsystem: level-gated by an if/switchCase action selector wired to
                    // the control port. Runs while the selector is > 0. Siblings are mutually
                    // exclusive and recombined by a Merge block.
                    st.subsystem->ctrlKind = 5;
                    st.subsystem->ctrlPortIdx = nData;
                } else if (kind == "functionCall") {
                    // Function-call subsystem: not scheduled in topological order. It runs only
                    // when a functionCallGenerator invokes it through SimCtx::invokeSubsystem (0..n
                    // times per step). The control port carries the event wire.
                    st.subsystem->ctrlKind = 6;
                    st.subsystem->ctrlPortIdx = nData;
                } else if (kind == "forIterator") {
                    // For Iterator subsystem: the body runs a fixed number of
                    // times SEQUENTIALLY within one step (see
                    // docs/iterator_subsystems.md). The count is IterationLimit
                    // (internal) or read from an appended input port (external).
                    st.subsystem->ctrlKind = 7;
                    const std::string src = cp.value("IterationSource", std::string("internal"));
                    st.subsystem->iterSource = (src == "external") ? 1 : 0;
                    st.subsystem->iterCount
                        = std::max(0, (int)(cp.value("IterationLimit", 1.0) + 0.5));
                    // 'held' carries the body's state across steps instead of
                    // putting it back at each entry.
                    st.subsystem->iterResetStates
                        = cp.value("ResetStates", std::string("reset")) != "held";
                    if (st.subsystem->iterSource == 1) {
                        st.subsystem->iterCountPortIdx = nData;
                    }
                } else if (kind == "whileIterator") {
                    // While Iterator subsystem: the body runs repeatedly
                    // (do-while) while an internal condition holds, bounded by the
                    // mandatory MaxIterations safety cap. The condition is the
                    // value of the inner iteratorCondition block, read after each
                    // pass; absent -> the loop runs the full cap.
                    st.subsystem->ctrlKind = 8;
                    st.subsystem->maxIterations
                        = std::max(1, (int)(cp.value("MaxIterations", 100.0) + 0.5));
                    for (const Block& ib : st.subsystem->blocks) {
                        if (ib.type == "iteratorCondition") {
                            st.subsystem->iterCondNid = ib.nid;
                            break;
                        }
                    }
                }
            }
            for (int p = 0; p < std::max(1, b.outputs); ++p) {
                const int wk = outputWidth(ctx, b.nid, p);
                double* dst = outputSlice(ctx, b.nid, p);
                for (int k = 0; k < wk; ++k) {
                    dst[k] = 0.0;
                }
            }
            return false;
        }

        auto scatterOutputs = [&]() {
            const auto& ss = *st.subsystem;
            for (int p = 0; p < std::max(1, b.outputs) && p < (int)ss.extOutWidth.size(); ++p) {
                const int off = ss.extOutOffset[p];
                const int wk = std::min(outputWidth(ctx, b.nid, p), ss.extOutWidth[p]);
                double* dst = outputSlice(ctx, b.nid, p);
                double* dstIm
                    = ss.lastValuesImag.empty() ? nullptr : outputSliceImag(ctx, b.nid, p);
                long long* dstI
                    = ss.lastValuesI64.empty() ? nullptr : outputSliceI64(ctx, b.nid, p);
                for (int k = 0; k < wk && off + k < (int)ss.lastValues.size(); ++k) {
                    dst[k] = ss.lastValues[off + k];
                    if (dstIm) {
                        dstIm[k] = ss.lastValuesImag[off + k];
                    }
                    if (dstI) {
                        dstI[k] = ss.lastValuesI64[off + k];
                    }
                }
            }
        };

        // Conditional gate. No control port: always active. enable: active while any
        // control element is > 0. trigger: active only on the step where the control
        // crosses zero in the configured direction, against the value latched at the
        // previous major step.
        auto subsystemActive = [&]() -> bool {
            const auto& ss = *st.subsystem;
            if (ss.ctrlKind == 6) {
                // functionCall: runs only while a generator is driving it through
                // invokeSubsystem; otherwise it holds its outputs (scatter only).
                return ss.functionCallInvoking;
            }
            if (ss.ctrlKind == 0 || ss.ctrlKind == 4 || ss.ctrlPortIdx < 0) {
                return true; // no control / resettable: always execute
            }
            SigView ctrl = getInputSig(ctx, b.nid, ss.ctrlPortIdx);
            if (ss.ctrlKind == 1 || ss.ctrlKind == 5) {
                // enable (1) and action (5): active while the control is > 0.
                const int w = std::max(1, ctrl.width);
                for (int i = 0; i < w; ++i) {
                    if (sigAt(ctrl, i) > 0.0) {
                        return true;
                    }
                }
                return false;
            }
            if (ss.ctrlKind == 3) {
                // enableTrigger: a trigger edge is honored only while enable > 0.
                if (ss.lastEventStamp == ctx.stepIndex) {
                    return false; // already fired at the located edge this step
                }
                bool enabled = false;
                const int w = std::max(1, ctrl.width);
                for (int i = 0; i < w; ++i) {
                    if (sigAt(ctrl, i) > 0.0) {
                        enabled = true;
                        break;
                    }
                }
                if (!enabled || !ss.havePrevCtrl || ss.ctrlPortIdx2 < 0) {
                    return false;
                }
                SigView trig = getInputSig(ctx, b.nid, ss.ctrlPortIdx2);
                return edgeFired(ss.prevCtrl, sigAt(trig, 0), ss.triggerType);
            }
            // trigger
            if (ss.lastEventStamp == ctx.stepIndex) {
                // A solver-located control edge already fired this subsystem at
                // the exact crossing instant within this step (Phase::EVENT);
                // do not fire again at the sample point.
                return false;
            }
            if (!ss.havePrevCtrl) {
                return false; // no edge before the first latch (t0 compatibility)
            }
            return edgeFired(ss.prevCtrl, sigAt(ctrl, 0), ss.triggerType);
        };

        // The gate the current step is INTEGRATED under. An enable is a level that changes
        // at a discrete instant, so one step belongs to one gate. Reading the live control
        // at RK stage time mixed enabled and disabled derivatives inside one step. Only
        // the integration reads this. Sample points still use the live control, which
        // decides whether outputs are held. An action gate is the branch an if/switchCase
        // selects from the CURRENT input, so it is left alone.
        auto subsystemIntegrationGate = [&]() -> bool {
            const auto& ss = *st.subsystem;
            if (ss.ctrlKind == 1 && ss.levelGateLatched) {
                return ss.levelGate;
            }
            return subsystemActive();
        };

        if (phase == Phase::OUTPUT) {
            if (!st.subsystem) {
                ctx.outputs[ctx.outputBase[b.nid]] = 0.0;
                return false;
            }
            if (st.subsystem->feNumIter > 1) {
                scatterForEach(ctx, b, *st.subsystem);
                return false;
            }
            scatterOutputs();
            return false;
        }

        if (phase == Phase::ALGEBRAIC) {
            if (!st.subsystem) {
                return false;
            }
            auto& ss = *st.subsystem;
            if (ss.feNumIter > 1) {
                // A for-each may also carry a control port. Honour the gate, as a for-each
                // composed inside a gated subsystem would.
                if (!subsystemActive()) {
                    scatterForEach(ctx, b, ss); // hold the last concatenation
                    return false;
                }
                if (ss.ctrlKind != 0) {
                    ss.lastActiveStamp = ctx.stepIndex;
                }
                std::vector<double> prevConcat = ss.feConcat;
                runForEach(ctx, b, ss, &ctx);
                scatterForEach(ctx, b, ss);
                bool changed = (prevConcat.size() != ss.feConcat.size());
                for (size_t i = 0; !changed && i < ss.feConcat.size(); ++i) {
                    if (prevConcat[i] != ss.feConcat[i]) {
                        changed = true;
                    }
                }
                return changed;
            }
            if (ss.ctrlKind == 7 || ss.ctrlKind == 8) {
                // For / While Iterator: run the body several times within this major step,
                // re-seeding inner state at entry and accumulating across iterations. Runs once
                // per step (iterStamp guard) and never during a side-effect-free solver probe.
                // Other ALGEBRAIC evaluations hold the computed outputs. See
                // docs/iterator_subsystems.md.
                if (ctx.sideEffectFree || ss.iterStamp == ctx.stepIndex) {
                    scatterOutputs();
                    return false;
                }
                ss.iterStamp = ctx.stepIndex;
                std::vector<double> prevValues = ss.lastValues;
                if (ss.iterResetStates) {
                    resetSubsystemStates(ss, ctx.variables, ctx.dt);
                }
                // External inputs are constant across the iterations.
                std::vector<double> itIn, itInImag;
                std::vector<long long> itInI64;
                gatherSubsystemInputs(ctx, b, ss, itIn, itInImag, itInI64);
                // Iteration count: fixed (For internal), from an input port (For
                // external), or the safety cap (While upper bound).
                int nIter;
                if (ss.ctrlKind == 7) {
                    nIter = ss.iterCount;
                    if (ss.iterSource == 1 && ss.iterCountPortIdx >= 0) {
                        nIter = std::max(
                            0, (int)(sigAt(getInputSig(ctx, b.nid, ss.iterCountPortIdx), 0) + 0.5));
                    }
                } else {
                    nIter = ss.maxIterations;
                }
                ss.iterCount = nIter; // surfaced to iteratorNumber as the total
                int run = 0;
                for (int it = 1; it <= nIter; ++it) {
                    ss.iterIndex = it;
                    runSubsystemAlgebraic(ss, itIn, ctx.variables, ctx.dt, ctx.t,
                        itInImag.empty() ? nullptr : &itInImag,
                        itInI64.empty() ? nullptr : &itInI64, &ctx);
                    advanceSubsystemState(ss, ctx.variables, ctx.dt, ctx.t, &ctx);
                    run = it;
                    if (ss.ctrlKind == 8) {
                        // do-while: continue while the condition holds. The iteratorCondition block
                        // stashes its input in its BlockState during the pass. A missing block or
                        // an unconnected input holds the loop (runs the full cap).
                        double cond = 1.0;
                        if (ss.iterCondNid >= 0 && ss.iterCondNid < (int)ss.blockStateVec.size()) {
                            cond = ss.blockStateVec[ss.iterCondNid].output;
                        }
                        if (cond == 0.0) {
                            break;
                        }
                    }
                }
                ss.lastIterationsRun = run;
                ss.iterIndex = 0;
                scatterOutputs();
                bool changed = (prevValues.size() != ss.lastValues.size());
                for (size_t i = 0; !changed && i < ss.lastValues.size(); ++i) {
                    if (prevValues[i] != ss.lastValues[i]) {
                        changed = true;
                    }
                }
                return changed;
            }
            if (!subsystemActive()) {
                // Disabled: hold the external outputs at their last value and do
                // not run/advance the inner diagram.
                scatterOutputs();
                return false;
            }
            // Active this major step: stamp it so a downstream Merge can pick this
            // branch (a control port with ctrlKind != 0 means conditional).
            if (ss.ctrlKind != 0) {
                ss.lastActiveStamp = ctx.stepIndex;
            }
            // Resettable: on a reset edge, re-seed the inner states to their
            // initial conditions before the run (idempotent across this step's
            // Gauss-Seidel iterations since prevCtrl is latched at UPDATE).
            if (ss.ctrlKind == 4 && ss.havePrevCtrl && ss.ctrlPortIdx >= 0
                && edgeFired(ss.prevCtrl, sigAt(getInputSig(ctx, b.nid, ss.ctrlPortIdx), 0),
                    ss.triggerType)) {
                resetSubsystemStates(ss, ctx.variables, ctx.dt);
                // Under a solver the inner continuous states live in the global vector, not in the
                // inner block states. Re-seeding only the latter leaves the solver integrating
                // from its old values, so publish the re-seeded states to the global slices too.
                // Only on a real pass: rhs evaluates at sub-stage points where x is the solver's
                // scratch.
                if (ctx.x != nullptr && !ctx.sideEffectFree) {
                    gatherContinuousState(ss.blocks, ss.blockStateVec, ctx.x, ctx.nx);
                }
            }
            std::vector<double> inVals, inValsImag;
            std::vector<long long> inValsI64;
            gatherSubsystemInputs(ctx, b, ss, inVals, inValsImag, inValsI64);

            std::vector<double> prevValues = ss.lastValues;
            runSubsystemAlgebraic(ss, inVals, ctx.variables, ctx.dt, ctx.t,
                inValsImag.empty() ? nullptr : &inValsImag,
                inValsI64.empty() ? nullptr : &inValsI64, &ctx);
            scatterOutputs();

            bool anyChanged = (prevValues.size() != ss.lastValues.size());
            for (size_t i = 0; !anyChanged && i < ss.lastValues.size(); ++i) {
                if (prevValues[i] != ss.lastValues[i]) {
                    anyChanged = true;
                }
            }
            return anyChanged;
        }

        if (phase == Phase::DERIVATIVE || phase == Phase::ZERO_CROSSING) {
            // Global-solver only (ctx.x wired). Re-evaluate the inner OUTPUT + ALGEBRAIC at
            // the scattered global state, then run the inner continuous phase so derivatives
            // and zero-crossing surfaces land in the global slices. Self-contained, so correct
            // in both the RHS and roots callbacks.
            if (!st.subsystem || ctx.x == nullptr) {
                return false;
            }
            auto& ss = *st.subsystem;
            if (ss.ctrlKind == 7 || ss.ctrlKind == 8) {
                // An iterator subsystem carries no solver-integrated state (a
                // continuous inner state is rejected at build time), so it
                // contributes no derivative / zero-crossing surface.
                return false;
            }
            if (phase == Phase::ZERO_CROSSING && (ss.ctrlKind == 2 || ss.ctrlKind == 3)) {
                // Control-edge surface: the trigger port's signal itself. Written even while the
                // gate is closed, since localizing the opening edge is the point: a variable-step
                // solver stops at the crossing instead of latching the edge at the next major
                // step (O(dt) error).
                double* g = blockG(ctx, b.nid);
                if (g != nullptr) {
                    const int port = (ss.ctrlKind == 3) ? ss.ctrlPortIdx2 : ss.ctrlPortIdx;
                    g[0] = (port >= 0) ? sigAt(getInputSig(ctx, b.nid, port), 0) : 1.0;
                }
            }
            if (ss.feNumIter > 1) {
                // For-each: a stateless / discrete-only body has no continuous state to
                // differentiate. A continuous body runs the phase once per iteration. Each
                // iteration's inner blocks address their own global x / xdot slice (N * nx_inner
                // layout).
                if (ss.feHasContinuous && subsystemIntegrationGate()) {
                    std::vector<double> inVals;
                    for (int i = 0; i < ss.feNumIter; ++i) {
                        gatherForEachIterationInputs(ctx, b, ss, i, inVals);
                        runSubsystemAlgebraic(feHolder(ss, i), inVals, ctx.variables, ctx.dt, ctx.t,
                            nullptr, nullptr, &ctx, phase);
                    }
                } else if (ss.feHasContinuous && phase == Phase::DERIVATIVE && ctx.xdot) {
                    // Gated shut: every replica's slice holds where it is.
                    for (const auto& rep : ss.feReplicas) {
                        if (!rep) {
                            continue;
                        }
                        for (const auto& innerSt : rep->blockStateVec) {
                            if (innerSt.xOffset >= 0) {
                                for (int k = 0; k < innerSt.xLength; ++k) {
                                    ctx.xdot[innerSt.xOffset + k] = 0.0;
                                }
                            }
                        }
                    }
                }
                return false;
            }
            if (!subsystemIntegrationGate()) {
                // Disabled: freeze the internal continuous states (their
                // derivative is 0), so the solver holds them exactly.
                if (phase == Phase::DERIVATIVE && ctx.xdot) {
                    for (const auto& innerSt : ss.blockStateVec) {
                        if (innerSt.xOffset >= 0) {
                            for (int k = 0; k < innerSt.xLength; ++k) {
                                ctx.xdot[innerSt.xOffset + k] = 0.0;
                            }
                        }
                    }
                }
                return false;
            }
            std::vector<double> inVals, inValsImag;
            std::vector<long long> inValsI64;
            gatherSubsystemInputs(ctx, b, ss, inVals, inValsImag, inValsI64);
            runSubsystemAlgebraic(ss, inVals, ctx.variables, ctx.dt, ctx.t,
                inValsImag.empty() ? nullptr : &inValsImag,
                inValsI64.empty() ? nullptr : &inValsI64, &ctx, phase);
            // Publish what the run produced. A derivative evaluation is pure, but the
            // ENCLOSING diagram reads these values. Without this, a nested subsystem published
            // the outputs of an earlier evaluation, and the value its parent holds when a gate
            // closes came from the middle of the step.
            scatterOutputs();
            return false;
        }

        if (phase == Phase::EVENT) {
            // Solver-located control edge: fire a triggered subsystem at the exact crossing
            // instant. Only meaningful under the global solver (ctx.x wired). The legacy
            // fixed-step loop dispatches EVENT too, but the ALGEBRAIC gate already handles the
            // edge there, so this is inert.
            if (!st.subsystem || ctx.x == nullptr) {
                return false;
            }
            auto& ss = *st.subsystem;
            if (ss.ctrlKind != 2 && ss.ctrlKind != 3) {
                return false;
            }
            const int port = (ss.ctrlKind == 3) ? ss.ctrlPortIdx2 : ss.ctrlPortIdx;
            if (port < 0) {
                return false;
            }
            const double cur = sigAt(getInputSig(ctx, b.nid, port), 0);
            // The solver stops only on crossings in the configured direction (rootDirection),
            // but a root on any other surface dispatches EVENT here too. Confirm a genuine
            // sign change of OUR control against the last latched value. At the located root
            // the control is ~0, possibly an epsilon on the pre-edge side (bisection stops
            // within 1e-12), so the post-edge test carries a small tolerance.
            const double prev = ss.havePrevCtrl ? ss.prevCtrl : 0.0;
            const double tol = 1e-9 * (1.0 + std::fabs(prev));
            const bool rose = (prev < 0.0 && cur >= -tol) || (prev <= 0.0 && cur > 0.0);
            const bool fell = (prev > 0.0 && cur <= tol) || (prev >= 0.0 && cur < 0.0);
            bool fired
                = (ss.triggerType == 1) ? fell : ((ss.triggerType == 2) ? (rose || fell) : rose);
            if (fired && ss.ctrlKind == 3) {
                // enableTrigger honors the edge only while enable > 0.
                SigView en = getInputSig(ctx, b.nid, ss.ctrlPortIdx);
                bool enabled = false;
                const int w = std::max(1, en.width);
                for (int i = 0; i < w; ++i) {
                    if (sigAt(en, i) > 0.0) {
                        enabled = true;
                        break;
                    }
                }
                fired = enabled;
            }
            if (!fired) {
                return false;
            }
            // Run the body once at the located instant, the same sequence an
            // active sample point runs (ALGEBRAIC then UPDATE), and stamp the
            // step so the sample-point gate does not fire a second time.
            ss.lastActiveStamp = ctx.stepIndex;
            ss.lastEventStamp = ctx.stepIndex;
            std::vector<double> inVals, inValsImag;
            std::vector<long long> inValsI64;
            gatherSubsystemInputs(ctx, b, ss, inVals, inValsImag, inValsI64);
            runSubsystemAlgebraic(ss, inVals, ctx.variables, ctx.dt, ctx.t,
                inValsImag.empty() ? nullptr : &inValsImag,
                inValsI64.empty() ? nullptr : &inValsI64, &ctx);
            scatterOutputs();
            advanceSubsystemState(ss, ctx.variables, ctx.dt, ctx.t, &ctx);
            // Re-latch the control on the post-edge side so this edge is spent. A located
            // rising fire latches a strictly positive value: the gate treats prev <= 0 ->
            // cur > 0 as an edge, so latching the raw ~0 crossing value would fire the same
            // edge again at the next sample. Also covers two crossings within one step.
            const bool firedRising = (ss.triggerType == 0) || (ss.triggerType == 2 && rose);
            const double side = (std::numeric_limits<double>::min)();
            ss.prevCtrl = firedRising ? (std::max)(cur, side) : (std::min)(cur, -side);
            ss.havePrevCtrl = true;
            return false;
        }

        if (phase == Phase::UPDATE) {
            if (!st.subsystem) {
                return false;
            }
            auto& ss = *st.subsystem;
            if (ss.ctrlKind == 7 || ss.ctrlKind == 8) {
                // Iterator: the ALGEBRAIC loop already advanced inner state once
                // per iteration this step, so there is nothing to advance here.
                return false;
            }
            if (ss.feNumIter > 1) {
                // For-each: a stateless body has nothing to advance. A discrete per-iteration body
                // advances each iteration's holder, with inputs retained from the ALGEBRAIC pass.
                // A closed gate advances none of them.
                if (!subsystemActive()) {
                    return false;
                }
                for (int i = 0; !ss.feReplicas.empty() && i < ss.feNumIter; ++i) {
                    advanceSubsystemState(feHolder(ss, i), ctx.variables, ctx.dt, ctx.t, &ctx);
                }
                return false;
            }
            if (subsystemActive()) {
                advanceSubsystemState(ss, ctx.variables, ctx.dt, ctx.t, &ctx);
            }
            // Latch the level for the step this UPDATE opens, so every stage of
            // that step integrates under one gate.
            if (ss.ctrlKind == 1) {
                ss.levelGate = subsystemActive();
                ss.levelGateLatched = true;
            }
            // Latch the trigger control once per major step (UPDATE runs once), so the next
            // step's edge is measured against it and stays stable across Gauss-Seidel
            // iterations. trigger latches its own port, enableTrigger latches the trigger port
            // (ctrlPortIdx2). A solver-located edge keeps its post-edge latch, otherwise the
            // same edge could fire again at the next sample.
            if (ss.lastEventStamp == ctx.stepIndex && (ss.ctrlKind == 2 || ss.ctrlKind == 3)) {
                return false;
            }
            if ((ss.ctrlKind == 2 || ss.ctrlKind == 4) && ss.ctrlPortIdx >= 0) {
                ss.prevCtrl = edgeZeroSideLatch(ss.prevCtrl,
                    sigAt(getInputSig(ctx, b.nid, ss.ctrlPortIdx), 0), ss.havePrevCtrl);
                ss.havePrevCtrl = true;
            } else if (ss.ctrlKind == 3 && ss.ctrlPortIdx2 >= 0) {
                ss.prevCtrl = edgeZeroSideLatch(ss.prevCtrl,
                    sigAt(getInputSig(ctx, b.nid, ss.ctrlPortIdx2), 0), ss.havePrevCtrl);
                ss.havePrevCtrl = true;
            }
            return false;
        }

        return false;
    }
    //=============================================================================
    // Function-call generator: each major step it drives every callee subsystem wired
    // to its event output, invoking each NumberOfIterations times through
    // SimCtx::invokeSubsystem. The callee list is resolved once by the sim loop
    // (BlockState::functionCallCallees). It fires once per major step
    // (lastInvokeStamp). The empty invoke target during recordAt/rhs/roots keeps those
    // side-effect-free passes from firing callees.
    static bool
    handleFunctionCallGenerator(SimCtx& ctx, const Block& b, Phase phase)
    {
        if (phase != Phase::ALGEBRAIC) {
            return false;
        }
        if (!ctx.invokeSubsystem || ctx.sideEffectFree) {
            // No hook installed, or a side-effect-free solver evaluation (rhs /
            // roots): fire nothing AND touch no state, so lastInvokeStamp is not
            // consumed here; the generator still fires once in recordAt.
            return false;
        }
        BlockState& st = ctx.blockState[b.nid];
        if (st.lastInvokeStamp == ctx.stepIndex) {
            return false; // already fired this major step
        }
        st.lastInvokeStamp = ctx.stepIndex;
        nflow::BlockDescriptor bd(b, ctx.variables);
        int iters = (int)bd.paramDouble("NumberOfIterations", 1.0);
        if (iters < 1) {
            iters = 1;
        }
        for (int calleeNid : st.functionCallCallees) {
            ctx.invokeSubsystem(calleeNid, iters);
        }
        // The event output carries no numeric meaning; expose the fire flag so a
        // scope/probe on the wire reads 1.0 while firing.
        ctx.outputs[ctx.outputBase[b.nid]] = 1.0;
        return false;
    }
    //=============================================================================
    // Function-call split: a compile-time event router (1 event input, N ordered
    // event outputs). The sim loop resolves each split into the ordered list of
    // callees driven by the upstream generator, so the block itself never runs.
    static bool
    handleFunctionCallSplit(SimCtx&, const Block&, Phase)
    {
        return false;
    }
    //=============================================================================
    void
    registerSubsystemHandlers(BlockRegistry& reg)
    {
        BlockMetadata ssMeta;
        ssMeta.acceptsVectorInputs = true;
        // Output-port widths are the widths of the signals feeding the subsystem's
        // external-output sinks, from a preview of the inner signal layout seeded with the
        // outer input widths from inSigs.
        ssMeta.resolveDims = [](const Block& b, const ValMap& vars,
                                 const std::vector<PortSig>& inSigs, std::vector<PortSig>& outSigs,
                                 std::string& err) {
            if (!b.params.contains(nflow::kSubsystem) || !b.params[nflow::kSubsystem].is_object()) {
                return true;
            }
            std::vector<int> inW(inSigs.size(), 1);
            for (size_t i = 0; i < inSigs.size(); ++i) {
                inW[i] = std::max(1, inSigs[i].width());
            }
            // For-each: divide the partitioned ports to the per-iteration width
            // before previewing the inner layout, then publish feN * innerWidth.
            std::vector<PortSig> inSigsMut(inSigs.begin(), inSigs.end());
            std::vector<char> isPartition;
            const int feN = forEachInputDivision(
                b.params, (int)inSigs.size(), inW, &inSigsMut, isPartition, err);
            if (feN < 0) {
                return false; // configuration error (err set)
            }
            std::string innerLayoutErr;
            auto ss = buildSubsystemInnerState(
                b.params[nflow::kSubsystem], vars, 0.01, inW, &inSigsMut, &innerLayoutErr);
            if (!innerLayoutErr.empty()) {
                // The inner layout is invalid (a width conflict, say). Refuse it, as the top level
                // does.
                err = innerLayoutErr;
                return false;
            }
            bool feHasState = false;
            if (feN > 1 && !forEachBodyIsSupported(*ss, feHasState, err)) {
                return false;
            }
            for (int k = 0; k < (int)outSigs.size() && k < (int)ss->extOutWidth.size(); ++k) {
                const bool haveSig
                    = (k < (int)ss->extOutSig.size() && ss->extOutSig[(size_t)k].width() > 0);
                const int innerW
                    = haveSig ? ss->extOutSig[(size_t)k].width() : std::max(1, ss->extOutWidth[k]);
                if (feN > 1) {
                    // Concatenate feN iterations -> outer width feN * innerW (real
                    // double; complex / stateful bodies were rejected above).
                    outSigs[k].setVector(feN * innerW);
                } else if (haveSig) {
                    // Adopt the full descriptor of the inner signal feeding the
                    // output sink (shape, type, complexity).
                    const PortSig& sig = ss->extOutSig[(size_t)k];
                    outSigs[k].copyShape(sig);
                    outSigs[k].type = sig.type;
                    outSigs[k].isComplex = sig.isComplex;
                } else {
                    outSigs[k].setVector(std::max(1, ss->extOutWidth[k]));
                }
            }
            return true;
        };
        // Trigger control-edge localization: a triggered / enableTrigger subsystem exposes
        // ONE zero-crossing surface (the trigger port's signal), so a variable-step solver
        // stops at the edge and EVENT fires the body at the located instant. Other kinds
        // declare no surface.
        auto ctrlKindOf = [](const Block& b) -> std::string {
            if (b.params.contains("controlPort") && b.params["controlPort"].is_object()) {
                return b.params["controlPort"].value("kind", std::string());
            }
            return std::string();
        };
        ssMeta.zeroCrossingCount = [ctrlKindOf](const Block& b, const ValMap&) -> int {
            const std::string kind = ctrlKindOf(b);
            return (kind == "trigger" || kind == "enableTrigger") ? 1 : 0;
        };
        ssMeta.zeroCrossingDirections
            = [ctrlKindOf](const Block& b, const ValMap&, std::vector<int>& dirs) {
                  const std::string kind = ctrlKindOf(b);
                  if (dirs.empty() || (kind != "trigger" && kind != "enableTrigger")) {
                      return;
                  }
                  const std::string tt
                      = b.params["controlPort"].value("triggerType", std::string("rising"));
                  dirs[0] = (tt == "falling") ? -1 : ((tt == "either") ? 0 : 1);
              };
        reg.registerHandler("subsystem", handleSubsystem,
            { Phase::INIT, Phase::OUTPUT, Phase::ALGEBRAIC, Phase::UPDATE, Phase::DERIVATIVE,
                Phase::ZERO_CROSSING, Phase::EVENT },
            "builtin.utility", ssMeta);
        reg.registerCodegen("subsystem", "c", getCodeGenCSubsystem());
        reg.registerCodegen("subsystem", "rust", getCodeGenRustSubsystem());

        BlockMetadata fcgMeta;
        fcgMeta.directFeedthrough = false;
        reg.registerHandler("functionCallGenerator", handleFunctionCallGenerator,
            { Phase::ALGEBRAIC }, "builtin.utility", fcgMeta);

        // Compile-time router: registered (so it is not an unknown block) with no
        // phases, so it never enters the schedule.
        reg.registerHandler("functionCallSplit", handleFunctionCallSplit, {}, "builtin.utility");

        // Generated code carries no call signal: a callee invoked once per step is inlined
        // where the diagram order puts it. The output is still emitted, so anything
        // reading it sees a defined value.
        {
            BlockCodegenTemplate fcg;
            fcg.step = "out_{id} = 1.0;";
            reg.registerCodegen(
                "functionCallGenerator", BlockRegistry::BlockCodegenPair { fcg, fcg });
            reg.registerCodegen("functionCallSplit", BlockRegistry::BlockCodegenPair { fcg, fcg });
        }
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenCSubsystem()
{
    // Not implemented here: the C generator keeps its native path, because its
    // emission is entangled with generator-wide analysis.
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
Nelson::NFlow::BlockCodegenTemplate
Nelson::NFlow::getCodeGenRustSubsystem()
{
    // Not implemented here: the Rust generator keeps its native path, because its
    // emission is entangled with generator-wide analysis.
    return BlockCodegenTemplate::unimplemented();
}
//=============================================================================
