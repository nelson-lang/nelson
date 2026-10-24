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
#include "SimEngineBusTypes.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "BlockRegistry.hpp"
#include "FieldNames.hpp"
#include <cstdio>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    double
    clampVal(double v, double lo, double hi)
    {
        if (lo > hi) {
            std::swap(lo, hi);
        }
        return std::min(std::max(v, lo), hi);
    }
    //=============================================================================
    double
    resolveDouble(
        const json& v, const std::unordered_map<std::string, double>& vars, double fallback)
    {
        if (v.is_number()) {
            return v.get<double>();
        }
        if (v.is_string()) {
            const std::string& s = v.get_ref<const std::string&>();
            nflow::VarMap vm;
            for (const auto& kv : vars) {
                vm[kv.first] = kv.second;
            }
            double r = nflow::evalExpression(s, vm);
            if (std::isnan(r)) {
                // Not an expression: try a variable name, then a plain number.
                auto it = vars.find(s);
                if (it != vars.end()) {
                    return it->second;
                }
                try {
                    return std::stod(s);
                } catch (...) {
                }
                return fallback;
            }
            return r;
        }
        return fallback;
    }
    //=============================================================================
    double
    param(const Block& b, const std::unordered_map<std::string, double>& vars, const char* key,
        double fallback)
    {
        if (!b.params.contains(key)) {
            return fallback;
        }
        return resolveDouble(b.params[key], vars, fallback);
    }
    //=============================================================================
    bool
    buildTfState(const json& numJ, const json& denJ, std::vector<double>& A_last_row,
        std::vector<double>& C_vec, double& D, std::vector<double>& initState, int& n,
        const std::unordered_map<std::string, double>& vars)
    {
        return nflow::buildTfState(numJ, denJ, A_last_row, C_vec, D, initState, n, vars);
    }
    //=============================================================================
    double
    tfOutput(const std::vector<double>& C, const std::vector<double>& state, double D, double input)
    {
        return nflow::tfOutput(C, state, D, input);
    }
    //=============================================================================
    std::vector<double>
    tfRK4(const std::vector<double>& A_last, const std::vector<double>& state, double input,
        double dt)
    {
        return nflow::tfRK4(A_last, state, input, dt);
    }
    //=============================================================================
    void
    tfDerivative(const std::vector<double>& A_last, const std::vector<double>& state, double input,
        std::vector<double>& xdotOut)
    {
        nflow::tfDerivative(A_last, state, input, xdotOut);
    }
    //=============================================================================
    double
    evalDiscreteTf(const std::vector<double>& num, const std::vector<double>& den,
        const std::vector<double>& xHist, const std::vector<double>& yHist)
    {
        return nflow::evalDiscreteTf(num, den, xHist, yHist);
    }
    //=============================================================================
    // A functionCall subsystem is never scheduled (it fires only when its
    // generator drives it), so its output slots have no scheduled owner and a
    // downstream algebraic block reading them would sort with no predecessor.
    // Redirect the slot ownership to the driving generator (the owner of the
    // control-port input, the last input port).
    static void
    remapFunctionCallOwners(std::vector<BlockId>& slotOwner, const std::vector<Block>& blocks,
        const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase,
        int totalSlots)
    {
        for (int i = 0; i < (int)blocks.size(); ++i) {
            const Block& b = blocks[i];
            if (b.type != "subsystem" || !b.params.contains("controlPort")) {
                continue;
            }
            const auto& cp = b.params["controlPort"];
            if (!cp.is_object() || cp.value("kind", std::string()) != "functionCall") {
                continue;
            }
            const BlockId nid = b.nid;
            if (nid < 0 || nid >= (int)inputIds.size() || inputIds[nid].empty()) {
                continue;
            }
            const int ctrlSlot = inputIds[nid].back(); // control port = last input port
            if (ctrlSlot < 0 || ctrlSlot >= totalSlots) {
                continue;
            }
            const BlockId genNid = slotOwner[ctrlSlot];
            if (genNid == INVALID_ID) {
                continue;
            }
            for (int p = 0; p < b.outputs; ++p) {
                const int s = outputBase[nid] + p;
                if (s >= 0 && s < totalSlots) {
                    slotOwner[s] = genNid;
                }
            }
        }
    }
    //=============================================================================
    std::vector<int>
    topoSortAlgebraic(const std::vector<int>& algIndices, const std::vector<Block>& blocks,
        const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase)
    {
        int totalSlots = 0;
        for (int i = 0; i < (int)blocks.size(); ++i) {
            if (outputBase[i] < 0) {
                continue; // the layout allocated no output slot for this block
            }
            int end = outputBase[i] + blocks[i].outputs;
            if (end > totalSlots) {
                totalSlots = end;
            }
        }
        std::vector<BlockId> slotOwner(totalSlots, INVALID_ID);
        for (int i = 0; i < (int)blocks.size(); ++i) {
            for (int p = 0; p < blocks[i].outputs; ++p) {
                int s = outputBase[i] + p;
                // A block with no allocated slot has outputBase -1, and a block
                // that declares no port count defaults to one output, so s can be
                // negative. Guard both ends, as remapFunctionCallOwners does.
                if (s >= 0 && s < totalSlots) {
                    slotOwner[s] = blocks[i].nid;
                }
            }
        }
        remapFunctionCallOwners(slotOwner, blocks, inputIds, outputBase, totalSlots);

        std::unordered_map<BlockId, int> nidToPos;
        nidToPos.reserve(algIndices.size());
        for (int pos = 0; pos < (int)algIndices.size(); ++pos) {
            nidToPos[blocks[algIndices[pos]].nid] = pos;
        }

        std::vector<int> indegree(algIndices.size(), 0);
        std::vector<std::vector<int>> outEdges(algIndices.size());

        for (int targetPos = 0; targetPos < (int)algIndices.size(); ++targetPos) {
            BlockId targetNid = blocks[algIndices[targetPos]].nid;
            if (targetNid < 0 || targetNid >= (int)inputIds.size()) {
                continue;
            }
            for (int srcSlot : inputIds[targetNid]) {
                if (srcSlot < 0 || srcSlot >= totalSlots) {
                    continue;
                }
                BlockId srcNid = slotOwner[srcSlot];
                if (srcNid == INVALID_ID || srcNid == targetNid) {
                    continue;
                }
                auto sit = nidToPos.find(srcNid);
                if (sit == nidToPos.end()) {
                    continue;
                }
                int srcPos = sit->second;
                bool dup = false;
                for (int x : outEdges[srcPos]) {
                    if (x == targetPos) {
                        dup = true;
                        break;
                    }
                }
                if (!dup) {
                    outEdges[srcPos].push_back(targetPos);
                    ++indegree[targetPos];
                }
            }
        }

        // A data store carries no wire, so nothing above orders its two ends.
        // A read answers with what this step wrote, so every write of a store
        // must run before every read of it. Without this edge a read has no
        // input, comes first and returns the value of the previous step.
        {
            auto storeName = [&](int pos) -> std::string {
                const Block& blk = blocks[algIndices[pos]];
                auto it = blk.params.find("DataStoreName");
                return (it != blk.params.end() && it->is_string()) ? it->get<std::string>()
                                                                   : std::string("A");
            };
            for (int w = 0; w < (int)algIndices.size(); ++w) {
                if (blocks[algIndices[w]].type != "dataStoreWrite") {
                    continue;
                }
                const std::string wname = storeName(w);
                for (int r = 0; r < (int)algIndices.size(); ++r) {
                    if (r == w || blocks[algIndices[r]].type != "dataStoreRead"
                        || storeName(r) != wname) {
                        continue;
                    }
                    bool dup = false;
                    for (int x : outEdges[w]) {
                        if (x == r) {
                            dup = true;
                            break;
                        }
                    }
                    if (!dup) {
                        outEdges[w].push_back(r);
                        ++indegree[r];
                    }
                }
            }
        }

        std::vector<int> queue;
        for (int i = 0; i < (int)algIndices.size(); ++i) {
            if (indegree[i] == 0) {
                queue.push_back(i);
            }
        }

        std::vector<int> ordered;
        int readPos = 0;
        while (readPos < (int)queue.size()) {
            int pos = queue[readPos++];
            ordered.push_back(algIndices[pos]);
            for (int next : outEdges[pos]) {
                if (--indegree[next] == 0) {
                    queue.push_back(next);
                }
            }
        }

        if ((int)ordered.size() != (int)algIndices.size()) {
            return std::vector<int>(algIndices.begin(), algIndices.end());
        }
        return ordered;
    }
    //=============================================================================
    std::vector<AlgebraicComponent>
    computeAlgebraicSCCs(const std::vector<int>& algIndices, const std::vector<Block>& blocks,
        const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase)
    {
        const int N = (int)algIndices.size();
        std::vector<AlgebraicComponent> result;
        if (N == 0) {
            return result;
        }

        int totalSlots = 0;
        for (int i = 0; i < (int)blocks.size(); ++i) {
            if (outputBase[i] < 0) {
                continue; // the layout allocated no output slot for this block
            }
            int end = outputBase[i] + blocks[i].outputs;
            if (end > totalSlots) {
                totalSlots = end;
            }
        }
        std::vector<BlockId> slotOwner(totalSlots, INVALID_ID);
        for (int i = 0; i < (int)blocks.size(); ++i) {
            for (int p = 0; p < blocks[i].outputs; ++p) {
                int s = outputBase[i] + p;
                // A block with no allocated slot has outputBase -1, and a block
                // that declares no port count defaults to one output, so s can be
                // negative. Guard both ends, as remapFunctionCallOwners does.
                if (s >= 0 && s < totalSlots) {
                    slotOwner[s] = blocks[i].nid;
                }
            }
        }
        remapFunctionCallOwners(slotOwner, blocks, inputIds, outputBase, totalSlots);
        std::unordered_map<BlockId, int> nidToPos;
        nidToPos.reserve(N);
        for (int pos = 0; pos < N; ++pos) {
            nidToPos[blocks[algIndices[pos]].nid] = pos;
        }

        // Directed edges src -> target (target reads an output owned by src).
        // A block wired to its own input is flagged as a self-loop: a single
        // block that is itself a 1-block algebraic loop needing iteration.
        std::vector<std::vector<int>> adj(N);
        std::vector<char> selfLoop(N, 0);
        for (int targetPos = 0; targetPos < N; ++targetPos) {
            BlockId targetNid = blocks[algIndices[targetPos]].nid;
            if (targetNid < 0 || targetNid >= (int)inputIds.size()) {
                continue;
            }
            for (int srcSlot : inputIds[targetNid]) {
                if (srcSlot < 0 || srcSlot >= totalSlots) {
                    continue;
                }
                BlockId srcNid = slotOwner[srcSlot];
                if (srcNid == INVALID_ID) {
                    continue;
                }
                if (srcNid == targetNid) {
                    selfLoop[targetPos] = 1;
                    continue;
                }
                auto sit = nidToPos.find(srcNid);
                if (sit == nidToPos.end()) {
                    continue;
                }
                int srcPos = sit->second;
                bool dup = false;
                for (int x : adj[srcPos]) {
                    if (x == targetPos) {
                        dup = true;
                        break;
                    }
                }
                if (!dup) {
                    adj[srcPos].push_back(targetPos);
                }
            }
        }
        // Same data store rule as topoSortAlgebraic: every write of a store runs
        // before every read of it. This is the graph the solve walks.
        {
            auto storeName = [&](int pos) -> std::string {
                const Block& blk = blocks[algIndices[pos]];
                auto it = blk.params.find("DataStoreName");
                return (it != blk.params.end() && it->is_string()) ? it->get<std::string>()
                                                                   : std::string("A");
            };
            for (int w = 0; w < N; ++w) {
                if (blocks[algIndices[w]].type != "dataStoreWrite") {
                    continue;
                }
                const std::string wname = storeName(w);
                for (int r = 0; r < N; ++r) {
                    if (r == w || blocks[algIndices[r]].type != "dataStoreRead"
                        || storeName(r) != wname) {
                        continue;
                    }
                    bool dup = false;
                    for (int x : adj[w]) {
                        if (x == r) {
                            dup = true;
                            break;
                        }
                    }
                    if (!dup) {
                        adj[w].push_back(r);
                    }
                }
            }
        }

        // Iterative Tarjan (explicit call stack: no recursion depth limit).
        std::vector<int> index(N, -1), low(N, 0);
        std::vector<char> onStack(N, 0);
        std::vector<int> tstack;
        int idxCounter = 0;
        std::vector<AlgebraicComponent> revComps; // reverse-topological order

        struct Frame
        {
            int v;
            int childIdx;
        };
        std::vector<Frame> call;
        for (int start = 0; start < N; ++start) {
            if (index[start] != -1) {
                continue;
            }
            call.push_back({ start, 0 });
            while (!call.empty()) {
                Frame& f = call.back();
                const int v = f.v;
                if (f.childIdx == 0) {
                    index[v] = low[v] = idxCounter++;
                    tstack.push_back(v);
                    onStack[v] = 1;
                }
                if (f.childIdx < (int)adj[v].size()) {
                    const int w = adj[v][f.childIdx];
                    ++f.childIdx;
                    if (index[w] == -1) {
                        call.push_back({ w, 0 }); // "recurse" into w
                    } else if (onStack[w] && index[w] < low[v]) {
                        low[v] = index[w];
                    }
                } else {
                    if (low[v] == index[v]) {
                        AlgebraicComponent comp;
                        while (true) {
                            int w = tstack.back();
                            tstack.pop_back();
                            onStack[w] = 0;
                            comp.blocks.push_back(algIndices[w]);
                            if (w == v) {
                                break;
                            }
                        }
                        comp.cyclic = comp.blocks.size() > 1 || selfLoop[v] != 0;
                        revComps.push_back(std::move(comp));
                    }
                    call.pop_back();
                    if (!call.empty() && low[v] < low[call.back().v]) {
                        low[call.back().v] = low[v]; // propagate up (post-return)
                    }
                }
            }
        }
        // Tarjan emits SCCs sinks-first; reverse to evaluate dependencies first.
        result.assign(revComps.rbegin(), revComps.rend());
        return result;
    }
    //=============================================================================
    static bool
    sameDims(const PortSig& a, const PortSig& b)
    {
        return a.dims == b.dims && a.type == b.type && a.isComplex == b.isComplex
            && a.busTypeId == b.busTypeId;
    }
    //=============================================================================
    // Human-readable shape ("3x1", "2x3x4") for diagnostics.
    static std::string
    dimsString(const PortSig& ps)
    {
        std::string s;
        const int n = std::max(2, ps.numDims());
        for (int i = 0; i < n; ++i) {
            if (i > 0) {
                s += "x";
            }
            s += std::to_string(i < ps.numDims() ? ps.dims[i] : 1);
        }
        return s;
    }
    //=============================================================================
    // Element-layout compatibility: shapes are compared padded with trailing 1s,
    // so [n] (unoriented) and [n x 1] (column) stay interchangeable.
    static bool
    shapesCompatible(const std::vector<int>& a, const std::vector<int>& b)
    {
        const size_t n = std::max(a.size(), b.size());
        for (size_t i = 0; i < n; ++i) {
            const int da = (i < a.size()) ? a[i] : 1;
            const int db = (i < b.size()) ? b[i] : 1;
            if (da != db) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    // Default dimension rule: scalar inputs broadcast; the first non-scalar
    // input's full N-D shape is adopted and every other non-scalar input must
    // be layout-compatible with it (no implicit N-D broadcasting).
    // conflictPorts receives the two offending ports.
    static bool
    defaultDimsRule(const std::vector<PortSig>& inSigs, const std::vector<int>& connected,
        PortSig& shape, int& conflictA, int& conflictB)
    {
        shape = PortSig {};
        int shapePort = -1;
        for (size_t p = 0; p < inSigs.size(); ++p) {
            if (p < connected.size() && connected[p] < 0) {
                continue; // unconnected
            }
            const PortSig& in = inSigs[p];
            if (in.width() <= 1) {
                continue; // scalar broadcasts
            }
            if (shape.width() <= 1) {
                shape.copyShape(in);
                shapePort = (int)p;
            } else if (!shapesCompatible(shape.dims, in.dims)) {
                conflictA = shapePort;
                conflictB = (int)p;
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    SignalLayout
    buildSignalLayout(std::vector<Block>& blocks, const std::vector<SignalConn>& conns,
        const std::unordered_map<std::string, std::string>& labelSinks,
        const std::unordered_map<std::string, BlockId>& nameToId, const ValMap& variables,
        const std::unordered_map<BlockId, int>* seedOutWidths, BusRegistry* busRegistry,
        const std::unordered_map<BlockId, PortSig>* seedOutSigs)
    {
        SignalLayout out;
        auto& reg = BlockRegistry::instance();
        const int nBlocks = (int)blocks.size();

        // Global port index (gpi) table + gpi -> owner block.
        out.portBase.assign(nBlocks, 0);
        int totalPorts = 0;
        for (int i = 0; i < nBlocks; ++i) {
            out.portBase[i] = totalPorts;
            totalPorts += std::max(0, blocks[i].outputs);
        }
        out.sigs.assign(totalPorts, PortSig {});
        std::vector<BlockId> portOwner(totalPorts, INVALID_ID);
        for (int i = 0; i < nBlocks; ++i) {
            for (int p = 0; p < std::max(0, blocks[i].outputs); ++p) {
                portOwner[out.portBase[i] + p] = i;
            }
        }

        out.inputSigs.resize(nBlocks);
        for (int i = 0; i < nBlocks; ++i) {
            out.inputSigs[i].assign(std::max(0, blocks[i].inputs), -1);
        }
        for (const SignalConn& c : conns) {
            if (c.fromNid < 0 || c.fromNid >= nBlocks || c.toNid < 0 || c.toNid >= nBlocks) {
                continue;
            }
            if (c.fromPort < 0 || c.fromPort >= std::max(0, blocks[c.fromNid].outputs)) {
                continue;
            }
            auto& row = out.inputSigs[c.toNid];
            if (c.toPort < 0) {
                continue;
            }
            if (c.toPort >= (int)row.size()) {
                row.resize(c.toPort + 1, -1);
            }
            row[c.toPort] = out.portBase[c.fromNid] + c.fromPort;
        }

        std::vector<BlockMetadata> meta(nBlocks);
        for (int i = 0; i < nBlocks; ++i) {
            meta[i] = reg.metadataOf(blocks[i].type);
        }

        auto gatherIn = [&](int i, std::vector<PortSig>& in) {
            const auto& row = out.inputSigs[i];
            in.assign(row.size(), PortSig {});
            for (size_t p = 0; p < row.size(); ++p) {
                if (row[p] >= 0) {
                    in[p] = out.sigs[row[p]];
                }
            }
        };

        // Resolve the goto/from short-circuit: labelSource dims = the dims of
        // whatever feeds the matching labelSink. Returns gpi or -1.
        auto labelSourceFeedGpi = [&](const Block& b) -> int {
            if (b.params.contains(nflow::kIsExternalPort)
                && b.params[nflow::kIsExternalPort].is_boolean()
                && b.params[nflow::kIsExternalPort].get<bool>()) {
                return -1;
            }
            if (!b.params.contains(nflow::kGotoTag) || !b.params[nflow::kGotoTag].is_string()) {
                return -1;
            }
            auto sit = labelSinks.find(b.params[nflow::kGotoTag].get<std::string>());
            if (sit == labelSinks.end()) {
                return -1;
            }
            auto nit = nameToId.find(sit->second);
            if (nit == nameToId.end()) {
                return -1;
            }
            const BlockId sinkNid = nit->second;
            if (sinkNid < 0 || sinkNid >= nBlocks || out.inputSigs[sinkNid].empty()) {
                return -1;
            }
            return out.inputSigs[sinkNid][0];
        };

        // Fixed-point forward propagation. Dims only grow (scalar -> vector), so a
        // bounded pass count either converges or flags a pathological diagram.
        const int maxPasses = nBlocks + 8;
        bool changed = true;
        int pass = 0;
        while (changed && pass++ < maxPasses) {
            changed = false;
            for (int i = 0; i < nBlocks; ++i) {
                Block& b = blocks[i];
                const int nOut = std::max(0, b.outputs);
                if (nOut == 0) {
                    continue;
                }
                std::vector<PortSig> inSigs;
                gatherIn(i, inSigs);
                std::vector<PortSig> outSigs(nOut);
                for (int p = 0; p < nOut; ++p) {
                    outSigs[p] = out.sigs[out.portBase[i] + p];
                }

                if (b.type == nflow::kLabelSource) {
                    // External-input port: width pinned by the outer signal.
                    if (seedOutSigs) {
                        // Full-descriptor seed at a subsystem boundary.
                        auto sit2 = seedOutSigs->find(i);
                        if (sit2 != seedOutSigs->end()) {
                            outSigs[0].copyShape(sit2->second);
                            outSigs[0].type = sit2->second.type;
                            outSigs[0].isComplex = sit2->second.isComplex;
                        }
                    } else if (seedOutWidths) {
                        auto sit = seedOutWidths->find(i);
                        if (sit != seedOutWidths->end()) {
                            outSigs[0].setVector(std::max(1, sit->second));
                        }
                    }
                    const int gpi = labelSourceFeedGpi(b);
                    if (gpi >= 0) {
                        const PortSig& src = out.sigs[gpi];
                        outSigs[0].copyShape(src);
                        outSigs[0].type = src.type;
                        outSigs[0].isComplex = src.isComplex;
                        // goto/from carries the bus flavor of its feed so a
                        // busSelector can read members after the label hop.
                        outSigs[0].busTypeId = src.busTypeId;
                    } else {
                        // Declared type: a named bus (struct-typed input at the
                        // generated interface) or a scalar data type (stamped per
                        // element by the codegen expansion).
                        if (busRegistry && b.params.contains(nflow::kBusType)
                            && b.params[nflow::kBusType].is_string()
                            && !b.params[nflow::kBusType].get<std::string>().empty()) {
                            const std::string bn = b.params[nflow::kBusType].get<std::string>();
                            const int busId = busRegistry->idOf(bn);
                            if (busId < 0) {
                                out.ok = false;
                                out.error = "block '" + b.id + "': unknown bus type '" + bn
                                    + "' (declare it in the model-level busTypes array)";
                                out.errorBlocks = { b.id };
                                return out;
                            }
                            outSigs[0].busTypeId = busId;
                            outSigs[0].setVector(std::max(1, busRegistry->packedWidth(busId)));
                        } else if (b.params.contains(nflow::kOutDataType)
                            && b.params[nflow::kOutDataType].is_string()) {
                            SigType st;
                            if (sigTypeFromString(
                                    b.params[nflow::kOutDataType].get<std::string>(), st)) {
                                outSigs[0].type = st;
                            }
                        }
                    }
                } else if (meta[i].resolveBusDims && busRegistry) {
                    std::string err;
                    if (!meta[i].resolveBusDims(b, variables, inSigs, outSigs, *busRegistry, err)) {
                        out.ok = false;
                        out.error = "block '" + b.id + "': " + err;
                        out.errorBlocks = { b.id };
                        return out;
                    }
                } else if (meta[i].resolveDims) {
                    std::string err;
                    if (!meta[i].resolveDims(b, variables, inSigs, outSigs, err)) {
                        out.ok = false;
                        out.error = "block '" + b.id + "': " + err;
                        out.errorBlocks = { b.id };
                        return out;
                    }
                } else {
                    PortSig shape;
                    int cA = -1, cB = -1;
                    if (!defaultDimsRule(inSigs, out.inputSigs[i], shape, cA, cB)) {
                        const std::string srcA = (cA >= 0 && out.inputSigs[i][cA] >= 0)
                            ? blocks[portOwner[out.inputSigs[i][cA]]].id
                            : "?";
                        const std::string srcB = (cB >= 0 && out.inputSigs[i][cB] >= 0)
                            ? blocks[portOwner[out.inputSigs[i][cB]]].id
                            : "?";
                        out.ok = false;
                        out.error = "signal width mismatch at block '" + b.id + "': input "
                            + std::to_string(cA + 1) + " (from '" + srcA + "', ["
                            + dimsString(inSigs[cA]) + "]) vs input " + std::to_string(cB + 1)
                            + " (from '" + srcB + "', [" + dimsString(inSigs[cB]) + "])";
                        out.errorBlocks = { b.id, srcA, srcB };
                        return out;
                    }
                    if (shape.width() > 1) {
                        for (int p = 0; p < nOut; ++p) {
                            outSigs[p].copyShape(shape);
                        }
                    }
                    // Unify the connected input types; the result flows to every
                    // output. Mixed types are validated after the fixed point so
                    // both endpoints can be named.
                    SigType unified = SigType::Double;
                    bool haveType = false;
                    for (size_t p = 0; p < inSigs.size(); ++p) {
                        if (p < out.inputSigs[i].size() && out.inputSigs[i][p] < 0) {
                            continue;
                        }
                        if (!haveType) {
                            unified = inSigs[p].type;
                            haveType = true;
                        } else if (inSigs[p].type != unified && !meta[i].allowMixedInputTypes) {
                            // Leave resolution to the validation pass; use the
                            // first type meanwhile so iteration stays stable.
                        }
                    }
                    if (haveType) {
                        for (int p = 0; p < nOut; ++p) {
                            outSigs[p].type = unified;
                        }
                    }
                }
                // Logic and comparison blocks force a Boolean output.
                if (meta[i].fixedOutSigType >= 0) {
                    for (int p = 0; p < nOut; ++p) {
                        outSigs[p].type = static_cast<SigType>(meta[i].fixedOutSigType);
                    }
                }
                // Complexity is OR-propagated by default (any complex input makes
                // every output complex), overridden by forcesComplexOut
                // (conversion blocks, abs). The goto/from short-circuit already
                // copied its source's flag, and the bus-aware rules own the
                // complexity of their outputs.
                if (b.type != nflow::kLabelSource && meta[i].forcesComplexOut != 2
                    && !(meta[i].resolveBusDims && busRegistry)) {
                    bool cx = false;
                    if (meta[i].forcesComplexOut == 1) {
                        cx = true;
                    } else if (meta[i].forcesComplexOut != 0) {
                        for (size_t p = 0; p < inSigs.size(); ++p) {
                            if (p < out.inputSigs[i].size() && out.inputSigs[i][p] < 0) {
                                continue;
                            }
                            cx = cx || inSigs[p].isComplex;
                        }
                    }
                    for (int p = 0; p < nOut; ++p) {
                        // A bus port carries no complexity flag of its own
                        // (member complexity lives in the registry).
                        if (outSigs[p].busTypeId < 0) {
                            outSigs[p].isComplex = cx;
                        }
                    }
                }

                for (int p = 0; p < nOut; ++p) {
                    PortSig& cur = out.sigs[out.portBase[i] + p];
                    if (!sameDims(cur, outSigs[p])) {
                        cur.copyShape(outSigs[p]);
                        cur.type = outSigs[p].type;
                        cur.isComplex = outSigs[p].isComplex;
                        cur.busTypeId = outSigs[p].busTypeId;
                        changed = true;
                    }
                }
            }
        }
        if (changed) {
            out.ok = false;
            out.error = "signal dimension propagation did not converge";
            return out;
        }

        // Validation: vector signals may only feed width-generalized handlers,
        // typed signals must agree (no implicit casts) and continuous-dynamics
        // blocks require Double inputs.
        for (int i = 0; i < nBlocks; ++i) {
            const auto& row = out.inputSigs[i];
            if (!meta[i].acceptsVectorInputs) {
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    const PortSig& in = out.sigs[row[p]];
                    if (in.width() > 1) {
                        const std::string srcId = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "block '" + blocks[i].id + "' (" + blocks[i].type
                            + ") does not support vector signals: input " + std::to_string(p + 1)
                            + " receives a [" + dimsString(in) + "] signal from '" + srcId + "'";
                        out.errorBlocks = { blocks[i].id, srcId };
                        return out;
                    }
                }
            }
            if (meta[i].requiresDoubleInputs) {
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    const PortSig& in = out.sigs[row[p]];
                    if (in.type != SigType::Double || in.isComplex) {
                        const std::string srcId = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "block '" + blocks[i].id + "' (" + blocks[i].type
                            + ") requires real double signals: input " + std::to_string(p + 1)
                            + " receives "
                            + (in.isComplex ? std::string("complex ") : std::string())
                            + sigTypeName(in.type) + " from '" + srcId + "' (insert a "
                            + (in.isComplex ? "complexToRealImag" : "convert") + " block)";
                        out.errorBlocks = { blocks[i].id, srcId };
                        return out;
                    }
                }
            }
            if (blocks[i].type == "busSelector" && busRegistry) {
                // A selector fed by a non-bus signal would silently emit
                // zeros: reject with the actual source named.
                if (!row.empty() && row[0] >= 0 && out.sigs[row[0]].busTypeId < 0) {
                    const std::string srcId = blocks[portOwner[row[0]]].id;
                    out.ok = false;
                    out.error = "block '" + blocks[i].id
                        + "': busSelector input is not a bus (receives a plain signal from '"
                        + srcId + "')";
                    out.errorBlocks = { blocks[i].id, srcId };
                    return out;
                }
            }
            if (!meta[i].acceptsBusInputs) {
                // Bus signals may only feed bus-aware blocks; anything else
                // would read the packed region as a plain vector.
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    const PortSig& in = out.sigs[row[p]];
                    if (in.busTypeId >= 0) {
                        const std::string srcId = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "block '" + blocks[i].id + "' (" + blocks[i].type
                            + ") does not support bus signals: input " + std::to_string(p + 1)
                            + " receives a bus from '" + srcId + "' (insert a busSelector block)";
                        out.errorBlocks = { blocks[i].id, srcId };
                        return out;
                    }
                }
            }
            if (!meta[i].acceptsComplexInputs) {
                // Complex signals may only feed complex-aware blocks; the
                // real lane alone would be silently wrong.
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    const PortSig& in = out.sigs[row[p]];
                    if (in.isComplex) {
                        const std::string srcId = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "block '" + blocks[i].id + "' (" + blocks[i].type
                            + ") does not support complex signals: input " + std::to_string(p + 1)
                            + " receives a complex signal from '" + srcId
                            + "' (insert a complexToRealImag block)";
                        out.errorBlocks = { blocks[i].id, srcId };
                        return out;
                    }
                }
            }
            if (!meta[i].supportsExact64) {
                // Exact-64 signals may only feed blocks with an integer-space
                // path; a silent double round-trip would lose precision
                // above 2^53.
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    const PortSig& in = out.sigs[row[p]];
                    if (sigTypeIsExact64(in.type)) {
                        const std::string srcId = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "block '" + blocks[i].id + "' (" + blocks[i].type
                            + ") does not support " + sigTypeName(in.type) + " signals: input "
                            + std::to_string(p + 1) + " receives " + sigTypeName(in.type)
                            + " from '" + srcId + "' (insert a convert block)";
                        out.errorBlocks = { blocks[i].id, srcId };
                        return out;
                    }
                }
            }
            if (!meta[i].allowMixedInputTypes && !meta[i].resolveDims
                && blocks[i].type != nflow::kLabelSource) {
                // Default-rule blocks: all connected inputs must share one type.
                int firstPort = -1;
                for (size_t p = 0; p < row.size(); ++p) {
                    if (row[p] < 0) {
                        continue;
                    }
                    if (firstPort < 0) {
                        firstPort = (int)p;
                        continue;
                    }
                    const SigType tA = out.sigs[row[firstPort]].type;
                    const SigType tB = out.sigs[row[p]].type;
                    if (tA != tB) {
                        const std::string srcA = blocks[portOwner[row[firstPort]]].id;
                        const std::string srcB = blocks[portOwner[row[p]]].id;
                        out.ok = false;
                        out.error = "signal type mismatch at block '" + blocks[i].id + "': input "
                            + std::to_string(firstPort + 1) + " is " + sigTypeName(tA) + " (from '"
                            + srcA + "') but input " + std::to_string(p + 1) + " is "
                            + sigTypeName(tB) + " (from '" + srcB + "'); insert a convert block";
                        out.errorBlocks = { blocks[i].id, srcA, srcB };
                        return out;
                    }
                }
            }
        }

        int slot = 0;
        for (int i = 0; i < nBlocks; ++i) {
            const int nOut = std::max(0, blocks[i].outputs);
            for (int p = 0; p < nOut; ++p) {
                PortSig& ps = out.sigs[out.portBase[i] + p];
                ps.offset = slot;
                slot += ps.width();
                out.hasComplex = out.hasComplex || portUsesImag(ps);
                out.hasExact64 = out.hasExact64 || portUsesI64(ps);
            }
        }
        out.totalSlots = slot;

        for (int i = 0; i < nBlocks; ++i) {
            Block& b = blocks[i];
            const int nOut = std::max(0, b.outputs);
            b.outWidths.assign(nOut, 1);
            for (int p = 0; p < nOut; ++p) {
                b.outWidths[p] = out.sigs[out.portBase[i] + p].width();
            }
            const auto& row = out.inputSigs[i];
            b.inWidths.assign(row.size(), 1);
            for (size_t p = 0; p < row.size(); ++p) {
                if (row[p] >= 0) {
                    b.inWidths[p] = out.sigs[row[p]].width();
                }
            }
        }
        return out;
    }
    //=============================================================================
    std::string
    doubleToStr(double v)
    {
        return nflow::doubleToStr(v);
    }
    //=============================================================================
    std::string
    buildOutputJson(const std::string& status, const std::string& reason,
        const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        const std::string& extraFields)
    {
        std::string out;
        out.reserve(1 << 20);

        out += "{\"status\":\"";
        out += status;
        out += "\"";
        if (!reason.empty()) {
            out += ",\"reason\":\"";
            for (char c : reason) {
                if (c == '"') {
                    out += "\\\"";
                } else if (c == '\\') {
                    out += "\\\\";
                } else {
                    out += c;
                }
            }
            out += "\"";
        }
        out += ",\"scopes\":[";

        bool firstScope = true;
        for (const Block& b : blocks) {
            if (b.nid < 0 || b.nid >= static_cast<int>(blockState.size())) {
                continue;
            }
            const BlockState& st = blockState[b.nid];

            if (b.type == "scope") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"scope\"";
                out += ",\"times\":[";
                for (int i = 0; i < (int)time.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(time[i]);
                }
                out += "]";
                out += ",\"series\":[";
                for (int s = 0; s < (int)st.scopeSeries.size(); ++s) {
                    if (s > 0) {
                        out += ",";
                    }
                    out += "[";
                    const auto& ser = st.scopeSeries[s];
                    for (int k = 0; k < (int)ser.size(); ++k) {
                        if (k > 0) {
                            out += ",";
                        }
                        out += doubleToStr(ser[k]);
                    }
                    out += "]";
                }
                out += "]";
                out += ",\"connected\":[";
                const auto& ins = (b.nid >= 0 && b.nid < (int)inputIds.size())
                    ? inputIds[b.nid]
                    : std::vector<int> {};
                // One flag per series; a vector port of width w contributes w
                // series sharing the port's connected flag.
                int n = (int)st.scopeSeries.size();
                int s = 0;
                for (int i = 0; s < n; ++i) {
                    bool inRange = (i < (int)ins.size());
                    bool conn = inRange && ins[i] >= 0;
                    int w = inRange ? b.inWidth(i) : (n - s);
                    for (int k = 0; k < w && s < n; ++k, ++s) {
                        if (s > 0) {
                            out += ",";
                        }
                        out += conn ? "true" : "false";
                    }
                }
                out += "]";
                out += "}";
            } else if (b.type == "toWorkspace") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                auto appendEscaped = [&out](const std::string& text) {
                    for (char c : text) {
                        if (c == '"') {
                            out += "\\\"";
                        } else if (c == '\\') {
                            out += "\\\\";
                        } else {
                            out += c;
                        }
                    }
                };
                std::string varName = "simout";
                std::string saveFormat = "Structure With Time";
                if (b.params.is_object()) {
                    if (b.params.contains(nflow::kVariableName)
                        && b.params[nflow::kVariableName].is_string()) {
                        varName = b.params[nflow::kVariableName].get<std::string>();
                    }
                    if (b.params.contains(nflow::kSaveFormat)
                        && b.params[nflow::kSaveFormat].is_string()) {
                        saveFormat = b.params[nflow::kSaveFormat].get<std::string>();
                    }
                }
                out += "{\"id\":\"";
                appendEscaped(b.id);
                out += "\",\"type\":\"toWorkspace\",\"name\":\"";
                appendEscaped(varName);
                out += "\",\"saveFormat\":\"";
                appendEscaped(saveFormat);
                out += "\",\"blockName\":\"";
                appendEscaped(b.id);
                out += "\",\"width\":";
                out += std::to_string((int)st.scopeSeries.size());
                out += ",\"times\":[";
                for (int i = 0; i < (int)st.xSeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.xSeries[i]);
                }
                out += "]";
                // per-sample layout: values[k * width + s]
                out += ",\"values\":[";
                bool firstValue = true;
                for (int k = 0; k < (int)st.xSeries.size(); ++k) {
                    for (int s = 0; s < (int)st.scopeSeries.size(); ++s) {
                        if (!firstValue) {
                            out += ",";
                        }
                        firstValue = false;
                        const auto& ser = st.scopeSeries[s];
                        out += doubleToStr(k < (int)ser.size() ? ser[k] : 0.0);
                    }
                }
                out += "]";
                // Typed lanes: imaginary parts of a complex signal, exact
                // 64-bit samples (full digits) of an int64/uint64 signal.
                if (!st.scopeSeriesImag.empty()) {
                    out += ",\"valuesImag\":[";
                    bool firstIv = true;
                    for (int k = 0; k < (int)st.xSeries.size(); ++k) {
                        for (int s = 0; s < (int)st.scopeSeriesImag.size(); ++s) {
                            if (!firstIv) {
                                out += ",";
                            }
                            firstIv = false;
                            const auto& ser = st.scopeSeriesImag[s];
                            out += doubleToStr(k < (int)ser.size() ? ser[k] : 0.0);
                        }
                    }
                    out += "]";
                }
                if (!st.scopeSeriesI64.empty()) {
                    const bool uns = st.scopeExactType == (int)SigType::UInt64;
                    out += ",\"exactType\":\"";
                    out += uns ? "uint64" : "int64";
                    out += "\",\"valuesI64\":[";
                    bool firstEv = true;
                    for (int k = 0; k < (int)st.xSeries.size(); ++k) {
                        for (int s = 0; s < (int)st.scopeSeriesI64.size(); ++s) {
                            if (!firstEv) {
                                out += ",";
                            }
                            firstEv = false;
                            const auto& ser = st.scopeSeriesI64[s];
                            const long long ev = k < (int)ser.size() ? ser[k] : 0;
                            out += uns ? std::to_string(static_cast<unsigned long long>(ev))
                                       : std::to_string(ev);
                        }
                    }
                    out += "]";
                }
                out += "}";
            } else if (b.type == "xyScope") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"xyScope\"";
                out += ",\"x\":[";
                for (int i = 0; i < (int)st.xSeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.xSeries[i]);
                }
                out += "]";
                out += ",\"y\":[";
                for (int i = 0; i < (int)st.ySeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.ySeries[i]);
                }
                out += "]";
                out += ",\"connected\":[";
                const auto& insXY = (b.nid >= 0 && b.nid < (int)inputIds.size())
                    ? inputIds[b.nid]
                    : std::vector<int> {};
                for (int i = 0; i < 2; ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += (i < (int)insXY.size() && insXY[i] >= 0) ? "true" : "false";
                }
                out += "]";
                out += "}";
            } else if (b.type == "xyzScope") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"xyzScope\"";
                out += ",\"x\":[";
                for (int i = 0; i < (int)st.xSeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.xSeries[i]);
                }
                out += "]";
                out += ",\"y\":[";
                for (int i = 0; i < (int)st.ySeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.ySeries[i]);
                }
                out += "]";
                out += ",\"z\":[";
                for (int i = 0; i < (int)st.zSeries.size(); ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += doubleToStr(st.zSeries[i]);
                }
                out += "]";
                out += ",\"connected\":[";
                const auto& insXYZ = (b.nid >= 0 && b.nid < (int)inputIds.size())
                    ? inputIds[b.nid]
                    : std::vector<int> {};
                for (int i = 0; i < 3; ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += (i < (int)insXYZ.size() && insXYZ[i] >= 0) ? "true" : "false";
                }
                out += "]";
                out += "}";
            } else if (b.type == "display") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"display\"";
                out += ",\"value\":";
                if (st.displayValues.size() > 1) {
                    out += "[";
                    for (size_t k = 0; k < st.displayValues.size(); ++k) {
                        if (k > 0) {
                            out += ",";
                        }
                        out += doubleToStr(st.displayValues[k]);
                    }
                    out += "]";
                } else {
                    out += doubleToStr(st.scalar);
                }
                out += "}";
            }
        }

        out += "]";
        out += ",\"dashboardUpdates\":[";
        bool firstDashboard = true;
        for (const Block& b : blocks) {
            if (b.nid < 0 || b.nid >= static_cast<int>(blockState.size())
                || b.type.rfind("dashboard", 0) != 0 || b.type == "dashboardCallbackButton") {
                continue;
            }
            const BlockState& state = blockState[b.nid];
            if (!firstDashboard) {
                out += ",";
            }
            firstDashboard = false;
            const bool observer = b.type == "dashboardScope" || b.type == "dashboardDisplay"
                || b.type == "dashboardGauge" || b.type == "dashboardHalfGauge"
                || b.type == "dashboardQuarterGauge" || b.type == "dashboardLinearGauge"
                || b.type == "dashboardLamp" || b.type == "dashboardMultiStateImage";
            json update = { { "id", b.id }, { "type", b.type },
                { "time", time.empty() ? 0.0 : time.back() },
                { "connected", observer ? state.scalar2 != 0.0 : state.dashboardTouched },
                { "valid", state.dashboardError.empty() },
                { "valueType", sigTypeName(static_cast<SigType>(state.scopeExactType)) } };
            if (!state.dashboardError.empty()) {
                update["reason"] = state.dashboardError;
            }
            if (!state.displayValuesI64.empty()) {
                const bool isUnsigned = state.scopeExactType == static_cast<int>(SigType::UInt64);
                json exact = json::array();
                for (long long value : state.displayValuesI64) {
                    exact.push_back(isUnsigned
                            ? std::to_string(static_cast<unsigned long long>(value))
                            : std::to_string(value));
                }
                update["value"] = exact.size() == 1 ? exact[0] : exact;
            } else if (state.displayValues.size() > 1) {
                update["value"] = state.displayValues;
            } else {
                update["value"] = state.scalar;
            }
            if (!state.displayValuesImag.empty()) {
                update["valueImag"] = state.displayValuesImag.size() == 1
                    ? json(state.displayValuesImag[0])
                    : json(state.displayValuesImag);
            }
            if (b.type == "dashboardScope") {
                // A dashboard scope only displays its TimeSpan window and its
                // history is not exported, so the final payload is bounded to
                // that window when TimeSpan > 0. The trim happens at
                // serialization only: the recorded arrays stay index-stable,
                // which the progress frames' per-block cursor
                // (dashboardPublishedLen) relies on.
                std::size_t fromIdx = 0;
                double timeSpan = 0.0;
                if (b.params.is_object() && b.params.contains("TimeSpan")
                    && b.params["TimeSpan"].is_number()) {
                    timeSpan = b.params["TimeSpan"].get<double>();
                }
                if (timeSpan > 0.0 && !state.xSeries.empty()) {
                    const double cutoff = state.xSeries.back() - timeSpan;
                    fromIdx = (std::size_t)(std::lower_bound(
                                                state.xSeries.begin(), state.xSeries.end(), cutoff)
                        - state.xSeries.begin());
                }
                update["times"]
                    = std::vector<double>(state.xSeries.begin() + fromIdx, state.xSeries.end());
                if (!state.scopeSeriesI64.empty()) {
                    const bool isUnsigned
                        = state.scopeExactType == static_cast<int>(SigType::UInt64);
                    json exactSeries = json::array();
                    for (const auto& row : state.scopeSeriesI64) {
                        json exactRow = json::array();
                        const std::size_t rf = std::min(fromIdx, row.size());
                        for (std::size_t k = rf; k < row.size(); ++k) {
                            exactRow.push_back(isUnsigned
                                    ? std::to_string(static_cast<unsigned long long>(row[k]))
                                    : std::to_string(row[k]));
                        }
                        exactSeries.push_back(std::move(exactRow));
                    }
                    update["series"] = std::move(exactSeries);
                } else {
                    json series = json::array();
                    for (const auto& row : state.scopeSeries) {
                        const std::size_t rf = std::min(fromIdx, row.size());
                        series.push_back(std::vector<double>(row.begin() + rf, row.end()));
                    }
                    update["series"] = std::move(series);
                }
                if (!state.scopeSeriesImag.empty()) {
                    json seriesImag = json::array();
                    for (const auto& row : state.scopeSeriesImag) {
                        const std::size_t rf = std::min(fromIdx, row.size());
                        seriesImag.push_back(std::vector<double>(row.begin() + rf, row.end()));
                    }
                    update["seriesImag"] = std::move(seriesImag);
                }
            }
            out += update.dump();
        }
        out += "]";
        if (!extraFields.empty()) {
            out += ",";
            out += extraFields;
        }
        out += "}";
        return out;
    }
    //=============================================================================
    std::string
    buildProgressJson(long long sequence, double progress, double currentTime, int fromSample,
        int toSample, const std::vector<double>& time, const std::vector<Block>& blocks,
        const std::vector<BlockState>& blockState, const std::vector<std::vector<int>>& inputIds,
        std::vector<std::size_t>* dashboardPublishedLen)
    {
        // An empty window is a legal progress-only frame.
        if (fromSample < 0) {
            fromSample = 0;
        }
        if (toSample > (int)time.size()) {
            toSample = (int)time.size();
        }
        if (toSample < fromSample) {
            toSample = fromSample;
        }

        std::string out;
        // Sized for the window, not the whole history.
        const std::size_t windowLen = (std::size_t)(toSample - fromSample);
        out.reserve(512 + 32 * windowLen);

        out += "{\"version\":2,\"sequence\":";
        out += std::to_string(sequence);
        out += ",\"status\":\"running\",\"progress\":";
        out += doubleToStr(progress);
        out += ",\"currentTime\":";
        out += doubleToStr(currentTime);
        out += ",\"fromSample\":";
        out += std::to_string(fromSample);
        out += ",\"toSample\":";
        out += std::to_string(toSample);
        out += ",\"scopes\":[";

        auto appendSlice = [&out](const std::vector<double>& v, int from, int to) {
            out += "[";
            const int end = std::min<int>(to, (int)v.size());
            for (int i = from; i < end; ++i) {
                if (i > from) {
                    out += ",";
                }
                out += doubleToStr(v[i]);
            }
            out += "]";
        };

        bool firstScope = true;
        for (const Block& b : blocks) {
            if (b.nid < 0 || b.nid >= static_cast<int>(blockState.size())) {
                continue;
            }
            const BlockState& st = blockState[b.nid];

            if (b.type == "scope") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"scope\",\"timesDelta\":";
                appendSlice(time, fromSample, toSample);
                out += ",\"seriesDelta\":[";
                for (int s = 0; s < (int)st.scopeSeries.size(); ++s) {
                    if (s > 0) {
                        out += ",";
                    }
                    appendSlice(st.scopeSeries[s], fromSample, toSample);
                }
                out += "]";
                // One flag per series, exactly as in the final result (a vector
                // port of width w contributes w series sharing the port's flag).
                out += ",\"connected\":[";
                const auto& ins = (b.nid >= 0 && b.nid < (int)inputIds.size())
                    ? inputIds[b.nid]
                    : std::vector<int> {};
                int n = (int)st.scopeSeries.size();
                int s = 0;
                for (int i = 0; s < n; ++i) {
                    bool inRange = (i < (int)ins.size());
                    bool conn = inRange && ins[i] >= 0;
                    int w = inRange ? b.inWidth(i) : (n - s);
                    for (int k = 0; k < w && s < n; ++k, ++s) {
                        if (s > 0) {
                            out += ",";
                        }
                        out += conn ? "true" : "false";
                    }
                }
                out += "]}";
            } else if (b.type == "xyScope" || b.type == "xyzScope") {
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                const bool xyz = b.type == "xyzScope";
                out += "{\"id\":\"";
                out += b.id;
                out += xyz ? "\",\"type\":\"xyzScope\"" : "\",\"type\":\"xyScope\"";
                out += ",\"xDelta\":";
                appendSlice(st.xSeries, fromSample, toSample);
                out += ",\"yDelta\":";
                appendSlice(st.ySeries, fromSample, toSample);
                if (xyz) {
                    out += ",\"zDelta\":";
                    appendSlice(st.zSeries, fromSample, toSample);
                }
                out += ",\"connected\":[";
                const auto& ins = (b.nid >= 0 && b.nid < (int)inputIds.size())
                    ? inputIds[b.nid]
                    : std::vector<int> {};
                const int nPorts = xyz ? 3 : 2;
                for (int i = 0; i < nPorts; ++i) {
                    if (i > 0) {
                        out += ",";
                    }
                    out += (i < (int)ins.size() && ins[i] >= 0) ? "true" : "false";
                }
                out += "]}";
            } else if (b.type == "display") {
                // Latest value only: a display never carries history.
                if (!firstScope) {
                    out += ",";
                }
                firstScope = false;
                out += "{\"id\":\"";
                out += b.id;
                out += "\",\"type\":\"display\",\"value\":";
                if (st.displayValues.size() > 1) {
                    out += "[";
                    for (std::size_t k = 0; k < st.displayValues.size(); ++k) {
                        if (k > 0) {
                            out += ",";
                        }
                        out += doubleToStr(st.displayValues[k]);
                    }
                    out += "]";
                } else {
                    out += doubleToStr(st.scalar);
                }
                out += "}";
            }
            // toWorkspace: final result only, never in a progress frame.
        }
        out += "]";

        if (dashboardPublishedLen) {
            out += ",\"dashboardUpdates\":[";
            bool firstDashboard = true;
            for (const Block& b : blocks) {
                if (b.nid < 0 || b.nid >= static_cast<int>(blockState.size())
                    || b.type.rfind("dashboard", 0) != 0 || b.type == "dashboardCallbackButton") {
                    continue;
                }
                const BlockState& state = blockState[b.nid];
                if (!firstDashboard) {
                    out += ",";
                }
                firstDashboard = false;
                const bool observer = b.type == "dashboardScope" || b.type == "dashboardDisplay"
                    || b.type == "dashboardGauge" || b.type == "dashboardHalfGauge"
                    || b.type == "dashboardQuarterGauge" || b.type == "dashboardLinearGauge"
                    || b.type == "dashboardLamp" || b.type == "dashboardMultiStateImage";
                json update = { { "id", b.id }, { "type", b.type },
                    { "time", time.empty() ? 0.0 : time.back() },
                    { "connected", observer ? state.scalar2 != 0.0 : state.dashboardTouched },
                    { "valid", state.dashboardError.empty() },
                    { "valueType", sigTypeName(static_cast<SigType>(state.scopeExactType)) } };
                if (!state.dashboardError.empty()) {
                    update["reason"] = state.dashboardError;
                }
                if (!state.displayValuesI64.empty()) {
                    const bool isUnsigned
                        = state.scopeExactType == static_cast<int>(SigType::UInt64);
                    json exact = json::array();
                    for (long long value : state.displayValuesI64) {
                        exact.push_back(isUnsigned
                                ? std::to_string(static_cast<unsigned long long>(value))
                                : std::to_string(value));
                    }
                    update["value"] = exact.size() == 1 ? exact[0] : exact;
                } else if (state.displayValues.size() > 1) {
                    update["value"] = state.displayValues;
                } else {
                    update["value"] = state.scalar;
                }
                if (!state.displayValuesImag.empty()) {
                    update["valueImag"] = state.displayValuesImag.size() == 1
                        ? json(state.displayValuesImag[0])
                        : json(state.displayValuesImag);
                }
                if (b.type == "dashboardScope") {
                    // Delta-encode the dashboard stream too: only the samples
                    // recorded since the previous publish, tracked per block.
                    if ((int)dashboardPublishedLen->size() <= b.nid) {
                        dashboardPublishedLen->resize(blockState.size(), 0);
                    }
                    const std::size_t already = (*dashboardPublishedLen)[b.nid];
                    const std::size_t total = state.xSeries.size();
                    const std::size_t from = std::min(already, total);
                    update["delta"] = true;
                    update["times"]
                        = std::vector<double>(state.xSeries.begin() + from, state.xSeries.end());
                    if (!state.scopeSeriesI64.empty()) {
                        const bool isUnsigned
                            = state.scopeExactType == static_cast<int>(SigType::UInt64);
                        json exactSeries = json::array();
                        for (const auto& row : state.scopeSeriesI64) {
                            json exactRow = json::array();
                            const std::size_t rf = std::min(from, row.size());
                            for (std::size_t k = rf; k < row.size(); ++k) {
                                exactRow.push_back(isUnsigned
                                        ? std::to_string(static_cast<unsigned long long>(row[k]))
                                        : std::to_string(row[k]));
                            }
                            exactSeries.push_back(std::move(exactRow));
                        }
                        update["series"] = std::move(exactSeries);
                    } else {
                        json series = json::array();
                        for (const auto& row : state.scopeSeries) {
                            const std::size_t rf = std::min(from, row.size());
                            series.push_back(std::vector<double>(row.begin() + rf, row.end()));
                        }
                        update["series"] = std::move(series);
                    }
                    if (!state.scopeSeriesImag.empty()) {
                        json seriesImag = json::array();
                        for (const auto& row : state.scopeSeriesImag) {
                            const std::size_t rf = std::min(from, row.size());
                            seriesImag.push_back(std::vector<double>(row.begin() + rf, row.end()));
                        }
                        update["seriesImag"] = std::move(seriesImag);
                    }
                    (*dashboardPublishedLen)[b.nid] = total;
                }
                out += update.dump();
            }
            out += "]";
        }
        out += "}";
        return out;
    }
    //=============================================================================
    bool
    parseBusRegistry(const json& diagram, BusRegistry& reg, std::string& err)
    {
        if (!diagram.is_object() || !diagram.contains(nflow::kBusTypes)) {
            return true; // no bus types declared
        }
        const json& list = diagram[nflow::kBusTypes];
        if (!list.is_array()) {
            err = "busTypes must be an array";
            return false;
        }
        // Pass 1: declare every named type (empty member lists) so nested
        // references resolve regardless of declaration order.
        std::vector<int> ids;
        for (const auto& jt : list) {
            BusType t;
            if (jt.is_object() && jt.contains(nflow::kName) && jt[nflow::kName].is_string()) {
                t.name = jt[nflow::kName].get<std::string>();
            }
            if (t.name.empty()) {
                err = "busTypes entries require a non-empty name";
                return false;
            }
            if (reg.idOf(t.name) >= 0) {
                err = "duplicate bus type '" + t.name + "'";
                return false;
            }
            if (jt.contains("nonvirtual") && jt["nonvirtual"].is_boolean()) {
                t.nonvirtual = jt["nonvirtual"].get<bool>();
            }
            ids.push_back(reg.add(std::move(t)));
        }
        for (size_t k = 0; k < list.size(); ++k) {
            const json& jt = list[(int)k];
            if (!jt.is_object() || !jt.contains(nflow::kMembers)
                || !jt[nflow::kMembers].is_array()) {
                continue;
            }
            std::vector<BusMember> members;
            for (const auto& jm : jt[nflow::kMembers]) {
                if (!jm.is_object() || !jm.contains(nflow::kName)
                    || !jm[nflow::kName].is_string()) {
                    err = "bus members require a name";
                    return false;
                }
                BusMember m;
                m.name = jm[nflow::kName].get<std::string>();
                if (jm.contains("bus") && jm["bus"].is_string()) {
                    const int sub = reg.idOf(jm["bus"].get<std::string>());
                    if (sub < 0) {
                        err = "bus member '" + m.name + "' references unknown bus type '"
                            + jm["bus"].get<std::string>() + "'";
                        return false;
                    }
                    m.sig.busTypeId = sub;
                } else {
                    SigType ty = SigType::Double;
                    if (jm.contains(nflow::kType) && jm[nflow::kType].is_string()
                        && !sigTypeFromString(jm[nflow::kType].get<std::string>(), ty)) {
                        err = "bus member '" + m.name + "': unknown type '"
                            + jm[nflow::kType].get<std::string>() + "'";
                        return false;
                    }
                    m.sig.type = ty;
                    if (jm.contains("complex") && jm["complex"].is_boolean()) {
                        m.sig.isComplex = jm["complex"].get<bool>();
                    }
                    if (jm.contains(nflow::kDims) && jm[nflow::kDims].is_array()) {
                        std::vector<int> dims;
                        for (const auto& d : jm[nflow::kDims]) {
                            if (d.is_number()) {
                                dims.push_back((int)d.get<double>());
                            }
                        }
                        if ((int)dims.size() > kMaxSignalDims) {
                            err = "bus member '" + m.name + "' exceeds the "
                                + std::to_string(kMaxSignalDims) + "-dimension limit";
                            return false;
                        }
                        m.sig.setDims(dims);
                    }
                }
                members.push_back(std::move(m));
            }
            reg.types[(size_t)ids[k]].members = std::move(members);
        }
        // Definition cycles make packedWidth degenerate: reject them.
        for (int id : ids) {
            if (!reg.types[(size_t)id].members.empty() && reg.packedWidth(id) <= 0) {
                err = "bus type '" + reg.types[(size_t)id].name
                    + "' has a degenerate (cyclic or empty-width) layout";
                return false;
            }
        }
        return true;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
