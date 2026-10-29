//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SubsystemInternal.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include "NFlowBlockDescriptor.hpp"
#include <cmath>
#include <algorithm>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    std::unique_ptr<BlockState::SubsystemState>
    buildSubsystemInnerState(const json& spec,
        const std::unordered_map<std::string, double>& outerVars, double dt,
        const std::vector<int>& outerInWidths, const std::vector<PortSig>* outerInSigs,
        std::string* layoutError)
    {
        auto& reg = BlockRegistry::instance();
        auto ss = std::make_unique<BlockState::SubsystemState>();

        if (spec.contains(nflow::kBlocks) && spec[nflow::kBlocks].is_array()) {
            for (const auto& bj : spec[nflow::kBlocks]) {
                Block b;
                b.id = bj.value(nflow::kId, "");
                b.type = bj.value(nflow::kType, "");
                b.inputs = bj.value(nflow::kInputs, 0);
                b.outputs = bj.value(nflow::kOutputs, 1);
                asJson(b.params) = bj.value(nflow::kParams, json::object());
                if (!b.id.empty() && !b.type.empty()) {
                    ss->blocks.push_back(std::move(b));
                }
            }
        }

        if (spec.contains(nflow::kConnections) && spec[nflow::kConnections].is_array()) {
            std::unordered_map<std::string, int> inCount, outCount;
            for (const Block& b : ss->blocks) {
                inCount[b.id] = 0;
                outCount[b.id] = 0;
            }
            for (const auto& cj : spec[nflow::kConnections]) {
                std::string from = cj.value(nflow::kFrom, "");
                std::string to = cj.value(nflow::kTo, "");
                int fi = cj.value(nflow::kFromIndex, 0);
                int ti = cj.value(nflow::kToIndex, 0);
                if (inCount.count(to)) {
                    inCount[to] = std::max(inCount[to], ti + 1);
                }
                if (outCount.count(from)) {
                    outCount[from] = std::max(outCount[from], fi + 1);
                }
            }
            for (Block& b : ss->blocks) {
                b.inputs = std::max(1, std::max(b.inputs, inCount[b.id]));
                b.outputs = std::max(1, std::max(b.outputs, outCount[b.id]));
            }
        } else {
            for (Block& b : ss->blocks) {
                b.inputs = std::max(1, b.inputs);
                b.outputs = std::max(1, b.outputs);
            }
        }

        // Slot allocation and inputIds come from the inner signal layout built
        // below, after external lookups and param resolution (dimension
        // propagation depends on both).
        for (int i = 0; i < (int)ss->blocks.size(); ++i) {
            ss->blocks[i].nid = i;
            ss->nameToId[ss->blocks[i].id] = i;
        }
        ss->blockStateVec.resize(ss->blocks.size());

        for (const Block& b : ss->blocks) {
            if (b.type != nflow::kLabelSink) {
                continue;
            }
            if (!asJson(b.params).contains(nflow::kGotoTag) || !asJson(b.params)[nflow::kGotoTag].is_string()) {
                continue;
            }
            std::string name = asJson(b.params)[nflow::kGotoTag].get<std::string>();
            if (!name.empty()) {
                ss->labelSinks[name] = b.id;
            }
        }

        if (spec.contains(nflow::kExternalInputs) && spec[nflow::kExternalInputs].is_array()) {
            int idx = 0;
            for (const auto& entry : spec[nflow::kExternalInputs]) {
                std::string eid = entry.value(nflow::kId, "");
                if (!eid.empty()) {
                    ss->inputLookup[eid] = idx;
                    if (entry.contains(nflow::kName) && entry[nflow::kName].is_string()) {
                        ss->labelSourceBlocks[eid] = entry[nflow::kName].get<std::string>();
                    }
                }
                ++idx;
            }
        }
        if (spec.contains(nflow::kExternalOutputs) && spec[nflow::kExternalOutputs].is_array()) {
            int idx = 0;
            for (const auto& entry : spec[nflow::kExternalOutputs]) {
                std::string eid = entry.value(nflow::kId, "");
                if (!eid.empty()) {
                    ss->outputLookup[eid] = idx;
                }
                ++idx;
            }
        }

        for (Block& b : ss->blocks) {
            if (b.type == nflow::kSubsystem || b.type == nflow::kLabelSource
                || b.type == nflow::kLabelSink) {
                continue;
            }
            nflow::BlockDescriptor bd(b, outerVars);
            for (auto& [key, val] : asJson(b.params).items()) {
                if (key == nflow::kSumInputs || key == nflow::kCondition || key == nflow::kExpr) {
                    continue;
                }
                if (val.is_string()) {
                    // Bake in a numeric result only when the string is a number,
                    // known constant, outer variable or math expression.
                    // resolveDouble returns NaN for a non-numeric string (an operator
                    // keyword like "lt"), whereas paramDouble would collapse it to 0.0
                    // and clobber string params such as `relop` and `operator`.
                    // See test_nflow_subsystem_relop.
                    double resolved
                        = resolveDouble(val, outerVars, std::numeric_limits<double>::quiet_NaN());
                    if (std::isfinite(resolved)) {
                        val = resolved;
                    }
                } else if (val.is_array()) {
                    // Only numeric-ish arrays are resolved to a double vector. A
                    // structured array (objects or nested arrays, e.g. a
                    // physicalIsland `components` netlist) must pass through
                    // untouched: coercing it would strip every component and leave
                    // a singular island. See test_nflow_island_solver_modes.
                    bool structured = false;
                    for (const auto& el : val) {
                        if (el.is_object() || el.is_array()) {
                            structured = true;
                            break;
                        }
                    }
                    if (structured) {
                        continue;
                    }
                    std::vector<double> lst = bd.paramList(key);
                    if (!lst.empty()) {
                        json arr = json::array();
                        for (double x : lst) {
                            arr.push_back(x);
                        }
                        val = arr;
                    } else {
                        for (auto& el : val) {
                            if (el.is_string()) {
                                double r = resolveDouble(
                                    el, outerVars, std::numeric_limits<double>::quiet_NaN());
                                if (std::isfinite(r)) {
                                    el = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Inner signal layout: dimensions and types propagate over the inner graph.
        // External-input ports are seeded with the outer signal width so vector
        // signals cross the boundary.
        {
            std::vector<SignalConn> conns;
            if (spec.contains(nflow::kConnections) && spec[nflow::kConnections].is_array()) {
                for (const auto& cj : spec[nflow::kConnections]) {
                    auto fromIt = ss->nameToId.find(cj.value(nflow::kFrom, ""));
                    auto toIt = ss->nameToId.find(cj.value(nflow::kTo, ""));
                    if (fromIt == ss->nameToId.end() || toIt == ss->nameToId.end()) {
                        continue;
                    }
                    conns.push_back({ fromIt->second, cj.value(nflow::kFromIndex, 0), toIt->second,
                        cj.value(nflow::kToIndex, 0) });
                }
            }
            std::unordered_map<BlockId, int> seed;
            std::unordered_map<BlockId, PortSig> seedSigs;
            for (const auto& [blockId, outerIdx] : ss->inputLookup) {
                auto nit = ss->nameToId.find(blockId);
                if (nit != ss->nameToId.end()) {
                    const int wsz = (outerIdx < (int)outerInWidths.size())
                        ? std::max(1, outerInWidths[outerIdx])
                        : 1;
                    seed[nit->second] = wsz;
                    if (outerInSigs && outerIdx < (int)outerInSigs->size()) {
                        seedSigs[nit->second] = (*outerInSigs)[(size_t)outerIdx];
                    }
                }
            }
            SignalLayout layout = buildSignalLayout(ss->blocks, conns, ss->labelSinks, ss->nameToId,
                outerVars, &seed, nullptr, seedSigs.empty() ? nullptr : &seedSigs);
            if (!layout.ok && layoutError != nullptr && layoutError->empty()) {
                // Surface the inner layout's own diagnostic instead of building
                // on it: the buffers below are sized from a layout that failed.
                *layoutError = layout.error;
            }
            ss->sigs = layout.sigs;
            ss->portBase = layout.portBase;
            ss->inputSigs = layout.inputSigs;
            // One slot past the layout, as the top-level driver allocates: a block
            // with no output port has outputBase == totalSlots as its "nowhere"
            // marker and the phases write through it, so an exactly sized buffer
            // was overrun by one element.
            const int slotsWithSink = std::max(1, layout.totalSlots) + 1;
            ss->outputsVec.assign(slotsWithSink, 0.0);
            ss->outputsImagVec.assign(layout.hasComplex ? slotsWithSink : 0, 0.0);
            ss->outputsI64Vec.assign(layout.hasExact64 ? slotsWithSink : 0, 0);
            ss->outputBase.assign(ss->blocks.size(), 0);
            for (int i = 0; i < (int)ss->blocks.size(); ++i) {
                ss->outputBase[i] = (ss->blocks[i].outputs > 0)
                    ? layout.sigs[layout.portBase[i]].offset
                    : layout.totalSlots;
            }
            ss->inputIds.assign(ss->blocks.size(), {});
            for (int i = 0; i < (int)ss->blocks.size(); ++i) {
                ss->inputIds[i].assign(ss->inputSigs[i].size(), -1);
                for (size_t p = 0; p < ss->inputSigs[i].size(); ++p) {
                    if (ss->inputSigs[i][p] >= 0) {
                        ss->inputIds[i][p] = layout.sigs[ss->inputSigs[i][p]].offset;
                    }
                }
            }
            // External-input port slice layout (into the flat outerInputs).
            const int nExtIn = (int)ss->inputLookup.size();
            ss->extInWidth.assign(nExtIn, 1);
            ss->extInOffset.assign(nExtIn, 0);
            for (const auto& [blockId, outerIdx] : ss->inputLookup) {
                if (outerIdx >= 0 && outerIdx < nExtIn) {
                    ss->extInWidth[outerIdx] = (outerIdx < (int)outerInWidths.size())
                        ? std::max(1, outerInWidths[outerIdx])
                        : 1;
                }
            }
            for (int k = 1; k < nExtIn; ++k) {
                ss->extInOffset[k] = ss->extInOffset[k - 1] + ss->extInWidth[k - 1];
            }
            // External-output port slice layout (into lastValues); the width is
            // the resolved width of the signal feeding the output sink.
            const int nExtOut = (int)ss->outputLookup.size();
            ss->extOutWidth.assign(nExtOut, 1);
            ss->extOutOffset.assign(nExtOut, 0);
            ss->extOutSig.assign((size_t)nExtOut, PortSig {});
            for (const auto& [sinkId, outIdx] : ss->outputLookup) {
                auto nit = ss->nameToId.find(sinkId);
                int wsz = 1;
                PortSig sig;
                if (nit != ss->nameToId.end()) {
                    const BlockId sinkNid = nit->second;
                    if (!ss->inputSigs[sinkNid].empty() && ss->inputSigs[sinkNid][0] >= 0) {
                        sig = ss->sigs[(size_t)ss->inputSigs[sinkNid][0]];
                        wsz = sig.width();
                    }
                }
                if (outIdx >= 0 && outIdx < nExtOut) {
                    ss->extOutWidth[outIdx] = std::max(1, wsz);
                    ss->extOutSig[(size_t)outIdx] = sig;
                }
            }
            int lvTotal = 0;
            for (int k = 0; k < nExtOut; ++k) {
                ss->extOutOffset[k] = lvTotal;
                lvTotal += ss->extOutWidth[k];
            }
            ss->lastValues.assign(std::max(1, lvTotal), 0.0);
            ss->lastValuesImag.assign(ss->outputsImagVec.empty() ? 0 : std::max(1, lvTotal), 0.0);
            ss->lastValuesI64.assign(ss->outputsI64Vec.empty() ? 0 : std::max(1, lvTotal), 0);
        }

        for (int i = 0; i < (int)ss->blocks.size(); ++i) {
            const std::string& type = ss->blocks[i].type;
            bool isExtIn = ss->inputLookup.count(ss->blocks[i].id) > 0;
            if (reg.getHandler(type) == nullptr) {
                continue;
            }
            if (reg.hasPhase(type, Phase::INIT)) {
                ss->initIdx.push_back(i);
            }
            if (!isExtIn && reg.hasPhase(type, Phase::OUTPUT)) {
                ss->outputIdx.push_back(i);
            }
            if (!isExtIn && reg.hasPhase(type, Phase::ALGEBRAIC)) {
                ss->algIdx.push_back(i);
            }
            if (reg.hasPhase(type, Phase::AFTER_STEP)) {
                ss->afterStepIdx.push_back(i);
            }
            if (reg.hasPhase(type, Phase::UPDATE)) {
                ss->updateIdx.push_back(i);
            }
            if (type == nflow::kLabelSource || type == nflow::kLabelSink) {
                ss->hasLabelBlocks = true;
            }
        }
        ss->algOrdered = topoSortAlgebraic(ss->algIdx, ss->blocks, ss->inputIds, ss->outputBase);
        ss->algHasCycle = (ss->algOrdered.size() != ss->algIdx.size());
        if (ss->algHasCycle) {
            ss->algOrdered = ss->algIdx;
        }

        SimCtx innerCtx { ss->blocks, ss->inputIds, ss->outputBase, ss->labelSinks, ss->nameToId,
            ss->blockStateVec, ss->outputsVec, outerVars, dt, 0.0 };
        innerCtx.sigs = &ss->sigs;
        innerCtx.portBase = &ss->portBase;
        innerCtx.inputSigs = &ss->inputSigs;
        if (!ss->outputsImagVec.empty()) {
            innerCtx.outputsImag = &ss->outputsImagVec;
        }
        if (!ss->outputsI64Vec.empty()) {
            innerCtx.outputsI64 = &ss->outputsI64Vec;
        }
        for (int i : ss->initIdx) {
            auto fn = reg.getHandler(ss->blocks[i].type);
            if (fn) {
                fn(innerCtx, ss->blocks[i], Phase::INIT);
            }
        }

        return ss;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
