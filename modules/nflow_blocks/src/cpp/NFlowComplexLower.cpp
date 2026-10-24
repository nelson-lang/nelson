//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Complex-lowering pass for code generation: rewrites a diagram with complex
// signals into an all-real diagram where every complex wire becomes a re/im
// pair of real wires. Runs BEFORE the vector->scalar expansion (the lowered
// signals keep their vector widths), so the scalar generators never see a
// complex signal.
//
// Lowering rules (one real block group per complex producer):
//   realImagToComplex        -> pure wiring (re = input 0, im = input 1)
//   complexToRealImag        -> pure wiring (port 0 = re, port 1 = im)
//   conjugate                -> re passthrough, im = negate
//   gain / sum / negate      -> the same block duplicated per lane
//   mult (a*b)               -> re = ar*br - ai*bi, im = ai*br + ar*bi
//   divide (a/b)             -> re = (ar*br + ai*bi)/d, im = (ai*br - ar*bi)/d
//                               with d = br*br + bi*bi
//   abs                      -> sqrt(re*re + im*im)  (expression block)
//   magnitudeAngleToComplex  -> re = m*cos(a), im = m*sin(a) (expression)
//   complexToMagnitudeAngle  -> magnitude ok; a CONNECTED angle port is a
//                               clean error (needs a two-input atan2)
//   switch / routing blocks  -> duplicated per lane (real condition shared)
//   labelSink (complex)      -> <tag>__re + <tag>__im sinks
//   scope/display/terminator -> consume the re lane
//=============================================================================
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include <cmath>
#include <set>
#include <unordered_map>
//=============================================================================
namespace nflow {
//=============================================================================
namespace {
    // A lowered wire endpoint: real block id + output port.
    struct WireRef
    {
        std::string id;
        int port = 0;
    };
    //=============================================================================
    struct ComplexLowerer
    {
        const json& diagram;
        CodegenWidths widths;
        std::unordered_map<std::string, json> byId;
        // inputSrc[toId][toPort] = (fromId, fromPort)
        std::unordered_map<std::string, std::vector<WireRef>> inputSrc;
        std::vector<json> outBlocks;
        std::vector<json> outConns;
        std::set<std::string> emitted;
        std::string error;

        explicit ComplexLowerer(const json& d) : diagram(d) { }

        bool
        isComplexOut(const std::string& id, int port) const
        {
            auto it = widths.outComplex.find(id);
            if (it == widths.outComplex.end() || port < 0 || port >= (int)it->second.size()) {
                return false;
            }
            return it->second[port] != 0;
        }

        WireRef
        srcOf(const std::string& id, int port) const
        {
            auto it = inputSrc.find(id);
            if (it == inputSrc.end() || port < 0 || port >= (int)it->second.size()) {
                return {};
            }
            return it->second[port];
        }

        std::string
        typeOf(const std::string& id) const
        {
            auto it = byId.find(id);
            return (it != byId.end()) ? jstr(it->second, nflow::kType) : std::string {};
        }

        // Resolve the real wire feeding lane (0 = re, 1 = im) of complex
        // producer (id, port). Pure-wiring blocks forward to their sources;
        // arithmetic producers point at their emitted lane block.
        WireRef
        laneOf(const std::string& id, int port, int lane)
        {
            const std::string type = typeOf(id);
            // Real-output conversion blocks vanish or split: map their ports
            // onto the lowered graph before the generic real-source path.
            if (type == "complexToRealImag") {
                WireRef s = srcOf(id, 0);
                if (s.id.empty()) {
                    return zeroRef();
                }
                return laneOf(s.id, s.port, (port == 1) ? 1 : 0);
            }
            if (type == "complexToMagnitudeAngle") {
                if (port == 1) {
                    return { id + "__ang", 0 }; // emitted atan2 block
                }
                return { id, 0 }; // emitted sqrt expression block
            }
            if (type == "busSelector") {
                auto mit = selectorPortMap.find(id);
                if (mit != selectorPortMap.end() && port >= 0 && port < (int)mit->second.size()) {
                    const SelPorts& sp = mit->second[(size_t)port];
                    if (sp.imPort >= 0) {
                        return { id, (lane == 1) ? sp.imPort : sp.rePort };
                    }
                    if (lane == 1) {
                        return zeroRef();
                    }
                    return { id, sp.rePort };
                }
                // fall through: selector without complex members keeps its
                // original ports (generic real path below)
            }
            if (!isComplexOut(id, port)) {
                // Real signal consumed by a complex-aware block: the im lane
                // is the shared zero constant.
                if (lane == 1) {
                    return zeroRef();
                }
                return { id, port };
            }
            if (type == "realImagToComplex") {
                WireRef s = srcOf(id, lane); // lane 0 <- input 0, lane 1 <- input 1
                if (s.id.empty()) {
                    return zeroRef();
                }
                return s;
            }
            if (type == "conjugate") {
                WireRef s = srcOf(id, 0);
                if (s.id.empty()) {
                    return zeroRef();
                }
                if (lane == 0) {
                    return laneOf(s.id, s.port, 0);
                }
                return { id + "__im", 0 }; // emitted negate block
            }
            if (type == "labelSource") {
                return { id + (lane == 0 ? "__re" : "__im"), 0 };
            }
            // Arithmetic / duplicated-per-lane producers emit <id>__re/__im.
            return { id + (lane == 0 ? "__re" : "__im"), 0 };
        }

        bool needZero = false;
        WireRef
        zeroRef()
        {
            needZero = true;
            return { "nflow_czero__c", 0 };
        }

        // Lowered busSelector port mapping: origPort -> {rePort, imPort}
        // (imPort = -1 for a real member). Precomputed before emission so
        // consumers resolve regardless of block order.
        struct SelPorts
        {
            int rePort = 0;
            int imPort = -1;
        };
        std::unordered_map<std::string, std::vector<SelPorts>> selectorPortMap;

        void
        precomputeSelectorMaps()
        {
            for (const auto& kv : byId) {
                const json& b = kv.second;
                if (jstr(b, nflow::kType) != "busSelector") {
                    continue;
                }
                const std::string id = kv.first;
                const json& params = jval(b, nflow::kParams);
                if (!params.is_object() || !params.contains(nflow::kSelectedSignals)
                    || !params[nflow::kSelectedSignals].is_array()) {
                    continue;
                }
                bool anyCx = false;
                const int nOut = jint(b, nflow::kOutputs, 1);
                for (int p = 0; p < nOut; ++p) {
                    anyCx = anyCx || isComplexOut(id, p);
                }
                if (!anyCx) {
                    continue;
                }
                std::vector<SelPorts>& map = selectorPortMap[id];
                int next = 0;
                for (int p = 0; p < nOut; ++p) {
                    SelPorts sp;
                    sp.rePort = next++;
                    if (isComplexOut(id, p)) {
                        sp.imPort = next++;
                    }
                    map.push_back(sp);
                }
            }
        }

        void
        addBlock(
            const std::string& id, const std::string& type, int inputs, int outputs, json params)
        {
            json b;
            b[nflow::kId] = id;
            b[nflow::kType] = type;
            b[nflow::kInputs] = inputs;
            b[nflow::kOutputs] = outputs;
            b[nflow::kParams] = std::move(params);
            outBlocks.push_back(std::move(b));
        }

        void
        addConn(const WireRef& from, const std::string& toId, int toPort)
        {
            if (from.id.empty()) {
                return;
            }
            json c;
            c[nflow::kFrom] = from.id;
            c[nflow::kTo] = toId;
            c[nflow::kFromIndex] = from.port;
            c[nflow::kToIndex] = toPort;
            outConns.push_back(std::move(c));
        }

        void
        emitExpr(const std::string& id, const std::string& expr, const WireRef& in)
        {
            json p;
            p[nflow::kExpr] = expr;
            addBlock(id, "expression", 1, 1, std::move(p));
            addConn(in, id, 0);
        }

        void
        emitMult(const std::string& id, const WireRef& a, const WireRef& b)
        {
            addBlock(id, "mult", 2, 1, json::object());
            addConn(a, id, 0);
            addConn(b, id, 1);
        }

        // signs: "++" or "+-"
        void
        emitSum(const std::string& id, const WireRef& a, const WireRef& b, const std::string& signs)
        {
            json p;
            p[nflow::kSumInputs] = signs;
            addBlock(id, "sum", 2, 1, std::move(p));
            addConn(a, id, 0);
            addConn(b, id, 1);
        }

        // Copy the original block under a new id, remapping its inputs to
        // the given lane of each source.
        void
        emitLaneCopy(const json& block, const std::string& newId, int lane)
        {
            const std::string id = jstr(block, nflow::kId);
            json b = block;
            b[nflow::kId] = newId;
            outBlocks.push_back(b);
            auto it = inputSrc.find(id);
            if (it == inputSrc.end()) {
                return;
            }
            const std::string type = jstr(block, nflow::kType);
            for (int p = 0; p < (int)it->second.size(); ++p) {
                WireRef s = it->second[p];
                if (s.id.empty()) {
                    continue;
                }
                // The switch condition (port 1) stays the real signal.
                const bool realPort = (type == "switch" && p == 1);
                WireRef lref = realPort ? laneOf(s.id, s.port, 0) : laneOf(s.id, s.port, lane);
                addConn(lref, newId, p);
            }
        }

        json
        run()
        {
            widths = computeCodegenWidths(diagram);
            json result;
            if (!widths.ok) {
                result["error"] = widths.error;
                return result;
            }
            if (!widths.anyComplex) {
                return diagram; // nothing to lower
            }
            const json& blocksJson
                = diagram.contains(nflow::kBlocks) ? diagram[nflow::kBlocks] : json::array();
            const json& connsJson = diagram.contains(nflow::kConnections)
                ? diagram[nflow::kConnections]
                : json::array();
            for (const auto& b : blocksJson) {
                byId[jstr(b, nflow::kId)] = b;
            }
            for (const auto& c : connsJson) {
                const std::string to = jstr(c, nflow::kTo);
                const int toPort = jint(c, nflow::kToIndex, 0);
                auto& list = inputSrc[to];
                if ((int)list.size() <= toPort) {
                    list.resize(toPort + 1);
                }
                list[toPort] = { jstr(c, nflow::kFrom), jint(c, nflow::kFromIndex, 0) };
            }
            precomputeSelectorMaps();

            for (const auto& b : blocksJson) {
                const std::string id = jstr(b, nflow::kId);
                const std::string type = jstr(b, nflow::kType);
                const bool cxOut = isComplexOut(id, 0);
                if (type == "busCreator") {
                    // Complex members split into two real members (m__re /
                    // m__im); the lowered bus is all-real, and the matching
                    // busSelector rewrite keeps the paths consistent.
                    bool anyCxIn = false;
                    auto iit = inputSrc.find(id);
                    const int nIn = jint(b, nflow::kInputs, 1);
                    if (iit != inputSrc.end()) {
                        for (const auto& s : iit->second) {
                            anyCxIn = anyCxIn || (!s.id.empty() && isComplexOut(s.id, s.port));
                        }
                    }
                    if (anyCxIn) {
                        const json& params = jval(b, nflow::kParams);
                        std::string declared;
                        if (params.is_object() && params.contains(nflow::kBusType)
                            && params[nflow::kBusType].is_string()) {
                            declared = params[nflow::kBusType].get<std::string>();
                        }
                        if (!declared.empty()) {
                            error = "block '" + id
                                + "': named bus types with complex members are not supported "
                                  "by code generation (the re/im split would break the "
                                  "declared layout)";
                            return json { { "error", error } };
                        }
                        std::vector<std::string> names;
                        if (params.is_object() && params.contains(nflow::kMemberNames)
                            && params[nflow::kMemberNames].is_array()) {
                            for (const auto& n : params[nflow::kMemberNames]) {
                                if (n.is_string()) {
                                    names.push_back(n.get<std::string>());
                                }
                            }
                        }
                        json newNames = json::array();
                        int newInputs = 0;
                        std::vector<std::pair<WireRef, int>> wires; // (src, toPort)
                        for (int p = 0; p < nIn; ++p) {
                            WireRef s = (iit != inputSrc.end() && p < (int)iit->second.size())
                                ? iit->second[(size_t)p]
                                : WireRef {};
                            const std::string base
                                = (p < (int)names.size() && !names[(size_t)p].empty())
                                ? names[(size_t)p]
                                : ("signal" + std::to_string(p + 1));
                            const bool cx = !s.id.empty() && isComplexOut(s.id, s.port);
                            if (cx) {
                                newNames.push_back(base + "__re");
                                wires.push_back({ laneOf(s.id, s.port, 0), newInputs++ });
                                newNames.push_back(base + "__im");
                                wires.push_back({ laneOf(s.id, s.port, 1), newInputs++ });
                            } else {
                                newNames.push_back(base);
                                if (!s.id.empty()) {
                                    wires.push_back({ laneOf(s.id, s.port, 0), newInputs });
                                }
                                ++newInputs;
                            }
                        }
                        json nb = b;
                        nb[nflow::kInputs] = newInputs;
                        json np = params.is_object() ? params : json::object();
                        np[nflow::kMemberNames] = newNames;
                        nb[nflow::kParams] = np;
                        outBlocks.push_back(std::move(nb));
                        for (const auto& w : wires) {
                            addConn(w.first, id, w.second);
                        }
                        continue;
                    }
                }
                if (type == "busSelector") {
                    auto mit = selectorPortMap.find(id);
                    if (mit != selectorPortMap.end()) {
                        // Rewrite the selected paths: a complex member's last
                        // segment doubles into __re / __im; ports renumber.
                        const json& params = jval(b, nflow::kParams);
                        std::vector<std::string> paths;
                        if (params.is_object() && params.contains(nflow::kSelectedSignals)
                            && params[nflow::kSelectedSignals].is_array()) {
                            for (const auto& s : params[nflow::kSelectedSignals]) {
                                if (s.is_string()) {
                                    paths.push_back(s.get<std::string>());
                                }
                            }
                        }
                        json newPaths = json::array();
                        int newOutputs = 0;
                        for (size_t p = 0; p < paths.size(); ++p) {
                            const bool cx = p < mit->second.size() && mit->second[p].imPort >= 0;
                            if (cx) {
                                newPaths.push_back(paths[p] + "__re");
                                newPaths.push_back(paths[p] + "__im");
                                newOutputs += 2;
                            } else {
                                newPaths.push_back(paths[p]);
                                ++newOutputs;
                            }
                        }
                        json nb = b;
                        nb[nflow::kOutputs] = newOutputs;
                        json np = params.is_object() ? params : json::object();
                        np[nflow::kSelectedSignals] = newPaths;
                        nb[nflow::kParams] = np;
                        outBlocks.push_back(std::move(nb));
                        WireRef s = srcOf(id, 0);
                        if (!s.id.empty()) {
                            addConn(laneOf(s.id, s.port, 0), id, 0);
                        }
                        continue;
                    }
                }
                if (type == "realImagToComplex") {
                    continue;
                }
                if (type == "complexToRealImag") {
                    continue; // consumers resolve through laneOf of its source
                }
                if (type == "complexToMagnitudeAngle") {
                    WireRef s = srcOf(id, 0);
                    const WireRef re = s.id.empty() ? zeroRef() : laneOf(s.id, s.port, 0);
                    const WireRef im = s.id.empty() ? zeroRef() : laneOf(s.id, s.port, 1);
                    // magnitude = sqrt(re*re + im*im) on port 0
                    emitMult(id + "__m2r", re, re);
                    emitMult(id + "__m2i", im, im);
                    emitSum(id + "__m2", { id + "__m2r", 0 }, { id + "__m2i", 0 }, "++");
                    emitExpr(id, "sqrt(u)", { id + "__m2", 0 });
                    // angle = atan2(im, re) on port 1, only when consumed.
                    bool angleUsed = false;
                    for (const auto& c : connsJson) {
                        if (jstr(c, nflow::kFrom) == id && jint(c, nflow::kFromIndex, 0) == 1) {
                            angleUsed = true;
                            break;
                        }
                    }
                    if (angleUsed) {
                        addBlock(id + "__ang", "atan2", 2, 1, json::object());
                        addConn(im, id + "__ang", 0);
                        addConn(re, id + "__ang", 1);
                    }
                    continue;
                }
                if (type == "abs" && !cxOut) {
                    WireRef s = srcOf(id, 0);
                    if (!s.id.empty() && isComplexOut(s.id, s.port)) {
                        // |z| = sqrt(re^2 + im^2)
                        const WireRef re = laneOf(s.id, s.port, 0);
                        const WireRef im = laneOf(s.id, s.port, 1);
                        emitMult(id + "__m2r", re, re);
                        emitMult(id + "__m2i", im, im);
                        emitSum(id + "__m2", { id + "__m2r", 0 }, { id + "__m2i", 0 }, "++");
                        emitExpr(id, "sqrt(u)", { id + "__m2", 0 });
                        continue;
                    }
                }
                if (!cxOut) {
                    // Real block: keep it, remapping any complex input to its
                    // re lane (only complex-consuming sinks reach here).
                    json nb = b;
                    outBlocks.push_back(nb);
                    auto it = inputSrc.find(id);
                    if (it != inputSrc.end()) {
                        for (int p = 0; p < (int)it->second.size(); ++p) {
                            WireRef s = it->second[p];
                            if (s.id.empty()) {
                                continue;
                            }
                            addConn(laneOf(s.id, s.port, 0), id, p);
                        }
                    }
                    continue;
                }
                if (type == "magnitudeAngleToComplex") {
                    WireRef m = srcOf(id, 0);
                    WireRef a = srcOf(id, 1);
                    const WireRef mr = m.id.empty() ? zeroRef() : laneOf(m.id, m.port, 0);
                    const WireRef ar = a.id.empty() ? zeroRef() : laneOf(a.id, a.port, 0);
                    emitExpr(id + "__cos", "cos(u)", ar);
                    emitExpr(id + "__sin", "sin(u)", ar);
                    emitMult(id + "__re", mr, { id + "__cos", 0 });
                    emitMult(id + "__im", mr, { id + "__sin", 0 });
                    continue;
                }
                if (type == "conjugate") {
                    // re lane resolves through laneOf; emit the im negate.
                    WireRef s = srcOf(id, 0);
                    const WireRef im = s.id.empty() ? zeroRef() : laneOf(s.id, s.port, 1);
                    addBlock(id + "__im", "negate", 1, 1, json::object());
                    addConn(im, id + "__im", 0);
                    continue;
                }
                if (type == "mult") {
                    // (ar + i*ai) * (br + i*bi); more than two inputs are already
                    // chained into pairwise products at diagram level.
                    WireRef a = srcOf(id, 0);
                    WireRef bsrc = srcOf(id, 1);
                    const WireRef ar = a.id.empty() ? zeroRef() : laneOf(a.id, a.port, 0);
                    const WireRef ai = a.id.empty() ? zeroRef() : laneOf(a.id, a.port, 1);
                    const WireRef br = bsrc.id.empty() ? zeroRef() : laneOf(bsrc.id, bsrc.port, 0);
                    const WireRef bi = bsrc.id.empty() ? zeroRef() : laneOf(bsrc.id, bsrc.port, 1);
                    emitMult(id + "__rr", ar, br);
                    emitMult(id + "__ii", ai, bi);
                    emitMult(id + "__ir", ai, br);
                    emitMult(id + "__ri", ar, bi);
                    emitSum(id + "__re", { id + "__rr", 0 }, { id + "__ii", 0 }, "+-");
                    emitSum(id + "__im", { id + "__ir", 0 }, { id + "__ri", 0 }, "++");
                    continue;
                }
                if (type == "divide") {
                    WireRef a = srcOf(id, 0);
                    WireRef bsrc = srcOf(id, 1);
                    const WireRef ar = a.id.empty() ? zeroRef() : laneOf(a.id, a.port, 0);
                    const WireRef ai = a.id.empty() ? zeroRef() : laneOf(a.id, a.port, 1);
                    const WireRef br = bsrc.id.empty() ? zeroRef() : laneOf(bsrc.id, bsrc.port, 0);
                    const WireRef bi = bsrc.id.empty() ? zeroRef() : laneOf(bsrc.id, bsrc.port, 1);
                    emitMult(id + "__brr", br, br);
                    emitMult(id + "__bii", bi, bi);
                    emitSum(id + "__den", { id + "__brr", 0 }, { id + "__bii", 0 }, "++");
                    emitMult(id + "__rr", ar, br);
                    emitMult(id + "__ii", ai, bi);
                    emitSum(id + "__ren", { id + "__rr", 0 }, { id + "__ii", 0 }, "++");
                    emitMult(id + "__ir", ai, br);
                    emitMult(id + "__ri", ar, bi);
                    emitSum(id + "__imn", { id + "__ir", 0 }, { id + "__ri", 0 }, "+-");
                    addBlock(id + "__re", "divide", 2, 1, json::object());
                    addConn({ id + "__ren", 0 }, id + "__re", 0);
                    addConn({ id + "__den", 0 }, id + "__re", 1);
                    addBlock(id + "__im", "divide", 2, 1, json::object());
                    addConn({ id + "__imn", 0 }, id + "__im", 0);
                    addConn({ id + "__den", 0 }, id + "__im", 1);
                    continue;
                }
                if (type == nflow::kLabelSink) {
                    // Complex model output: <tag>__re / <tag>__im sinks.
                    WireRef s = srcOf(id, 0);
                    const WireRef re = s.id.empty() ? zeroRef() : laneOf(s.id, s.port, 0);
                    const WireRef im = s.id.empty() ? zeroRef() : laneOf(s.id, s.port, 1);
                    std::string tag;
                    const json& p = jval(b, nflow::kParams);
                    if (p.is_object() && p.contains(nflow::kGotoTag)
                        && p[nflow::kGotoTag].is_string()) {
                        tag = p[nflow::kGotoTag].get<std::string>();
                    }
                    json pre;
                    pre[nflow::kGotoTag] = tag + "__re";
                    addBlock(id + "__re", nflow::kLabelSink, 1, 1, std::move(pre));
                    addConn(re, id + "__re", 0);
                    json pim;
                    pim[nflow::kGotoTag] = tag + "__im";
                    addBlock(id + "__im", nflow::kLabelSink, 1, 1, std::move(pim));
                    addConn(im, id + "__im", 0);
                    continue;
                }
                // Duplicated-per-lane blocks (gain, sum, negate, switch and
                // the lane-preserving routing set).
                emitLaneCopy(b, id + "__re", 0);
                emitLaneCopy(b, id + "__im", 1);
            }

            if (needZero) {
                json p;
                p[nflow::kValue] = 0.0;
                addBlock("nflow_czero__c", "constant", 0, 1, std::move(p));
            }

            json lowered = diagram;
            lowered[nflow::kBlocks] = json::array();
            for (auto& b : outBlocks) {
                lowered[nflow::kBlocks].push_back(std::move(b));
            }
            lowered[nflow::kConnections] = json::array();
            for (auto& c : outConns) {
                lowered[nflow::kConnections].push_back(std::move(c));
            }
            return lowered;
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
json
lowerComplexDiagram(const json& diagram)
{
    ComplexLowerer lw(diagram);
    return lw.run();
}
//=============================================================================
} // namespace nflow
//=============================================================================
