//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Vector -> scalar diagram expansion for code generation.
//
// The simulation engine carries vector/matrix signals natively, but the C/Rust
// generators emit scalar code. Rather than teach every block template to emit
// arrays and loops, this pass rewrites a vector diagram into an equivalent
// all-scalar one:
//   - element-wise and stateful blocks become one scalar copy per element;
//   - routing blocks (mux/demux/selector/reshape/concatenate) collapse into
//     direct element wires (resolved recursively to the ultimate producer);
//   - vector constants / labels / list-valued params split per element.
// The existing (golden-tested) scalar generator then emits correct code.
//=============================================================================
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include "SimEngineBusTypes.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <map>
#include <set>
#include <unordered_map>
//=============================================================================
namespace nflow {
//=============================================================================
namespace {
    const std::set<std::string> kRouting = { "mux", "demux", "selector", "reshape", "concatenate",
        "busCreator", "busSelector", "busAssignment", "assignment" };
    //=============================================================================
    // Vector-math reductions carry no codegen template of their own; the
    // expansion below (see resolveImpl / emitReduction) lowers each into scalar
    // "sum" / "mult" primitive blocks, which the scalar generator emits for both
    // C and Rust. Keep this set in sync with the reduction handling in
    // resolveImpl; expandsToGeneratableCode() consults it so
    // __nflow_codegen_support__ reports these blocks as supported.
    const std::set<std::string> kExpansionLoweredReductions
        = { "sumElements", "productOfElements", "dotProduct", "crossProduct" };
    // L5+ lookup blocks lowered to luScalar helpers (C + Rust templates).
    const std::set<std::string> kExpansionLoweredLookups
        = { "prelookup", "interpolationPrelookup", "lookupDynamic" };
    // Sinks kept as a single block whose input ports are expanded per element
    // (as opposed to label sinks, which become distinct per-element copies).
    const std::set<std::string> kMultiPortSinks = { "scope", "xyScope", "xyzScope", "display" };
    std::string
    elemId(const std::string& id, int e)
    {
        return id + "__e" + std::to_string(e);
    }
    //=============================================================================
    // A scalar signal source: output port `port` of expanded block `id`.
    struct ScalarRef
    {
        std::string id;
        int port = 0;
    };
    //=============================================================================
    struct Expander
    {
        const json& diagram;
        VectorExpandResult result;

        std::unordered_map<std::string, json> byId;
        CodegenWidths widths;
        // inputSrc[toId][toPort] = (fromId, fromPort)
        std::unordered_map<std::string, std::unordered_map<int, std::pair<std::string, int>>>
            inputSrc;
        std::map<std::tuple<std::string, int, int>, ScalarRef> resolveMemo;
        bool needZero = false;

        // Named bus types declared by the model. Ids match the layout registry:
        // parsed named types first, inferred anonymous ones after.
        Nelson::NFlow::BusRegistry namedBusReg;
        // One entry per label sink fed by a named nonvirtual bus:
        // { name, bus, elements[] }, element tags in packed order.
        json busOutputsMeta = json::array();
        // Same for bus-typed label sources (struct-typed external inputs).
        json busInputsMeta = json::array();

        json outBlocks = json::array();
        json outConns = json::array();

        explicit Expander(const json& d) : diagram(d) { }

        int
        outW(const std::string& id, int p) const
        {
            auto it = widths.outWidths.find(id);
            if (it != widths.outWidths.end() && p >= 0 && p < (int)it->second.size()) {
                return std::max(1, it->second[p]);
            }
            return 1;
        }
        int
        inW(const std::string& id, int p) const
        {
            auto it = widths.inWidths.find(id);
            if (it != widths.inWidths.end() && p >= 0 && p < (int)it->second.size()) {
                return std::max(1, it->second[p]);
            }
            return 1;
        }
        std::pair<int, int>
        outDim(const std::string& id, int p) const
        {
            auto it = widths.outDims.find(id);
            if (it != widths.outDims.end() && p >= 0 && p < (int)it->second.size()) {
                return it->second[p];
            }
            return { 1, 1 };
        }
        std::pair<int, int>
        inDim(const std::string& id, int p) const
        {
            auto it = widths.inDims.find(id);
            if (it != widths.inDims.end() && p >= 0 && p < (int)it->second.size()) {
                return it->second[p];
            }
            return { 1, 1 };
        }
        std::vector<int>
        outDimN(const std::string& id, int p) const
        {
            auto it = widths.outDimsN.find(id);
            if (it != widths.outDimsN.end() && p >= 0 && p < (int)it->second.size()) {
                return it->second[p];
            }
            return { 1 };
        }
        std::vector<int>
        inDimN(const std::string& id, int p) const
        {
            auto it = widths.inDimsN.find(id);
            if (it != widths.inDimsN.end() && p >= 0 && p < (int)it->second.size()) {
                return it->second[p];
            }
            return { 1 };
        }
        int
        numInputs(const std::string& id) const
        {
            auto it = widths.inWidths.find(id);
            return (it != widths.inWidths.end()) ? (int)it->second.size() : 0;
        }
        int
        numOutputs(const std::string& id) const
        {
            auto it = widths.outWidths.find(id);
            return (it != widths.outWidths.end()) ? (int)it->second.size() : 0;
        }
        std::string
        typeOf(const std::string& id) const
        {
            auto it = byId.find(id);
            return (it != byId.end()) ? jstr(it->second, kType) : std::string {};
        }

        // The zero scalar constant, created lazily for unconnected routing inputs.
        ScalarRef
        zeroRef()
        {
            needZero = true;
            return { "nflow_zero__c", 0 };
        }

        // Resolve element `elem` of output port `outPort` of ORIGINAL block id
        // to the scalar signal that ultimately produces it in the expanded
        // diagram (following routing blocks recursively).
        ScalarRef
        resolve(const std::string& id, int outPort, int elem)
        {
            auto key = std::make_tuple(id, outPort, elem);
            auto mit = resolveMemo.find(key);
            if (mit != resolveMemo.end()) {
                return mit->second;
            }
            ScalarRef out = resolveImpl(id, outPort, elem);
            resolveMemo[key] = out;
            return out;
        }

        std::pair<std::string, int>
        srcOf(const std::string& id, int port) const
        {
            auto bit = inputSrc.find(id);
            if (bit != inputSrc.end()) {
                auto pit = bit->second.find(port);
                if (pit != bit->second.end()) {
                    return pit->second;
                }
            }
            return { std::string {}, 0 };
        }

        ScalarRef
        resolveImpl(const std::string& id, int outPort, int elem)
        {
            const std::string type = typeOf(id);
            if (kRouting.count(type)) {
                if (type == "mux" || type == "busCreator") {
                    // A bus packs its members in port order.
                    int acc = 0;
                    const int nIn = numInputs(id);
                    for (int p = 0; p < nIn; ++p) {
                        const int w = inW(id, p);
                        if (elem < acc + w) {
                            auto s = srcOf(id, p);
                            if (s.first.empty()) {
                                return zeroRef();
                            }
                            return resolve(s.first, s.second, elem - acc);
                        }
                        acc += w;
                    }
                    return zeroRef();
                }
                if (type == "busSelector") {
                    // Output port `outPort` starts at the resolved member
                    // offset inside the packed bus wired to input 0.
                    auto it = widths.busSelectorOffsets.find(id);
                    if (it == widths.busSelectorOffsets.end() || outPort < 0
                        || outPort >= (int)it->second.size() || it->second[outPort] < 0) {
                        return zeroRef();
                    }
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    return resolve(s.first, s.second, it->second[outPort] + elem);
                }
                if (type == "demux") {
                    int off = 0;
                    for (int q = 0; q < outPort; ++q) {
                        off += outW(id, q);
                    }
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    return resolve(s.first, s.second, off + elem);
                }
                if (type == "selector") {
                    // 1-based Indices; clamp into the input range.
                    nflow::VarMap vm; // params already resolved by flatten
                    auto it = byId.find(id);
                    std::vector<double> idx;
                    if (it != byId.end()) {
                        idx = parseList(jval(jval(it->second, kParams), kIndices), vm);
                    }
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    const int inWidth = inW(id, 0);
                    int k = (elem < (int)idx.size()) ? (int)std::llround(idx[elem]) : (elem + 1);
                    k = std::min(std::max(k, 1), std::max(1, inWidth));
                    return resolve(s.first, s.second, k - 1);
                }
                if (type == "assignment") {
                    // 1-based Indices route replacement elements (port 1) over
                    // the base signal (port 0). The first matching index wins.
                    nflow::VarMap vm; // params already resolved by flatten
                    auto it = byId.find(id);
                    std::vector<double> idx;
                    if (it != byId.end()) {
                        idx = parseList(jval(jval(it->second, kParams), kIndices), vm);
                    }
                    for (size_t k = 0; k < idx.size(); ++k) {
                        if ((int)std::llround(idx[k]) - 1 == elem) {
                            auto r = srcOf(id, 1);
                            if (r.first.empty()) {
                                return zeroRef();
                            }
                            return resolve(r.first, r.second, (int)k);
                        }
                    }
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    return resolve(s.first, s.second, elem);
                }
                if (type == "busAssignment") {
                    // Replacement port k+1 overwrites the packed region of
                    // assigned path k over the base bus (port 0). The runtime
                    // applies the paths in order, so the last path wins on
                    // overlap: scan the regions in reverse. A scalar replacement
                    // broadcasts over a wider member (clamped).
                    auto it = widths.busAssignmentRegions.find(id);
                    if (it != widths.busAssignmentRegions.end()) {
                        for (size_t k = it->second.size(); k-- > 0;) {
                            const int off = it->second[k].first;
                            const int w = it->second[k].second;
                            if (off < 0 || elem < off || elem >= off + w) {
                                continue;
                            }
                            auto r = srcOf(id, (int)k + 1);
                            if (r.first.empty()) {
                                return zeroRef();
                            }
                            const int rw = std::max(1, inW(id, (int)k + 1));
                            return resolve(r.first, r.second, std::min(elem - off, rw - 1));
                        }
                    }
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    return resolve(s.first, s.second, elem);
                }
                if (type == "reshape") {
                    auto s = srcOf(id, 0);
                    if (s.first.empty()) {
                        return zeroRef();
                    }
                    return resolve(s.first, s.second, elem);
                }
                if (type == "concatenate") {
                    // N-D column-major concatenation along ConcatenateDimension.
                    // prefix = product of the output dims before the concat dim,
                    // slice = prefix * output extent along it. Split the output
                    // index into (outer o, position along, offset kIn), find the
                    // input owning `along` and map back.
                    auto it = byId.find(id);
                    nflow::VarMap vm;
                    double dimv = 1.0;
                    if (it != byId.end()) {
                        dimv = resolveNumeric(jval(jval(it->second, kParams), kConcatDim), vm);
                    }
                    int dim = (int)std::llround(dimv);
                    if (dim < 1) {
                        dim = 1;
                    }
                    const std::vector<int> od = outDimN(id, 0);
                    auto extentOf = [](const std::vector<int>& d, int k) {
                        return (k >= 1 && k <= (int)d.size()) ? std::max(1, d[k - 1]) : 1;
                    };
                    int prefix = 1;
                    for (int k = 1; k < dim; ++k) {
                        prefix *= extentOf(od, k);
                    }
                    prefix = std::max(1, prefix);
                    const int slice = std::max(1, prefix * extentOf(od, dim));
                    const int o = elem / slice;
                    const int rem = elem % slice;
                    const int along = rem / prefix;
                    const int kIn = rem % prefix;
                    const int nIn = numInputs(id);
                    int base = 0;
                    for (int p = 0; p < nIn; ++p) {
                        const int wp = inW(id, p);
                        const int ep = (wp > 1) ? extentOf(inDimN(id, p), dim) : 1;
                        if (along < base + ep) {
                            auto s = srcOf(id, p);
                            if (s.first.empty()) {
                                return zeroRef();
                            }
                            int srcElem = o * (prefix * ep) + (along - base) * prefix + kIn;
                            if (wp <= 1) {
                                srcElem = 0;
                            } else if (srcElem >= wp) {
                                srcElem = wp - 1;
                            }
                            return resolve(s.first, s.second, srcElem);
                        }
                        base += ep;
                    }
                    return zeroRef();
                }
            }
            if (type == "matmul") {
                // Each output element is produced by an emitted scalar sum
                // block (matrix rule) or mult block (element-wise / scalar).
                const int w = outW(id, 0);
                if (w <= 1) {
                    return { id + "__mm0", 0 };
                }
                return { id + "__mme" + std::to_string(elem), 0 };
            }
            if (type == "sumElements" || type == "productOfElements" || type == "dotProduct") {
                return { id + "__red", 0 };
            }
            if (type == "crossProduct") {
                return { id + "__cr" + std::to_string(elem), 0 };
            }
            // Lookup L5+ lowered to luScalar helper blocks (see emitLookupLowered).
            if (type == "prelookup") {
                // Output element 0 -> index, element 1 -> fraction.
                return { id + (elem == 0 ? "__plK" : "__plF"), 0 };
            }
            if (type == "interpolationPrelookup") {
                return { id + "__ip", 0 };
            }
            if (type == "lookupDynamic") {
                return { id + "__ld", 0 };
            }
            if (type == "stateSpace" && outW(id, outPort) > 1) {
                // Kept as ONE multi-port block (coupled states): output
                // element e is generated output port e.
                return { id, elem };
            }
            // One scalar copy per element. A width-1 block keeps its original id.
            const int w = outW(id, outPort);
            if (w <= 1) {
                return { id, outPort };
            }
            return { elemId(id, elem), 0 };
        }

        // Copy a block's params, slicing any list-valued param of length == W
        // to its e-th element (vector Value / Initial / ...).
        json
        elemParams(const json& params, int e, int w, const std::string& type = "") const
        {
            json p = params.is_object() ? params : json::object();
            // fromWorkspace bakes samples column-major as values[k*width + i]
            // with a shared 'times' vector; element e needs the strided column
            // e (stride = width) and width 1, not the generic v[e] slice.
            if (type == "fromWorkspace" && w > 1) {
                int width = w;
                if (p.contains(kWidth) && p[kWidth].is_number()) {
                    width = std::max(1, p[kWidth].get<int>());
                }
                if (p.contains(kValues) && p[kValues].is_array()) {
                    const json& vals = p[kValues];
                    json col = json::array();
                    for (size_t k = 0; k * width + e < vals.size(); ++k) {
                        col.push_back(vals[k * width + e]);
                    }
                    p[kValues] = std::move(col);
                }
                p[kWidth] = 1;
                return p;
            }
            for (auto& [k, v] : p.items()) {
                // Lowering bookkeeping (__nfgGates chain, ...) is an array by
                // design. Slicing it would silently drop the guard.
                if (k.rfind("__nfg", 0) == 0) {
                    continue;
                }
                if (v.is_array() && (int)v.size() == w && w > 1) {
                    p[k] = v[e];
                }
            }
            return p;
        }

        void
        addBlock(const std::string& id, const std::string& type, int inputs, int outputs,
            const json& params)
        {
            json b;
            b[kId] = id;
            b[kType] = type;
            b[kInputs] = inputs;
            b[kOutputs] = outputs;
            b[kParams] = params;
            outBlocks.push_back(std::move(b));
        }

        void
        addConn(const ScalarRef& from, const std::string& toId, int toPort)
        {
            json c;
            c[kFrom] = from.id;
            c[kFromIndex] = from.port;
            c[kTo] = toId;
            c[kToIndex] = toPort;
            outConns.push_back(std::move(c));
        }

        // Expand a matmul block into scalar mult/sum blocks. resolve(matmul,
        // 0, elem) points at the emitted per-element block (see resolveImpl).
        void
        emitMatmul(const std::string& id, const json& params)
        {
            std::string rule = jstr(params, "MultiplicationRule", "matrix");
            const int nOutElem = std::max(1, outW(id, 0));
            const std::pair<int, int> outD = outDim(id, 0);
            const auto srcA = srcOf(id, 0);
            const auto srcB = srcOf(id, 1);
            auto emitProduct = [&](const std::string& cid, const ScalarRef& x, const ScalarRef& y) {
                json p;
                addBlock(cid, "mult", 2, 1, p);
                addConn(x, cid, 0);
                addConn(y, cid, 1);
            };
            if (rule == "elementwise" || rule == "scalar") {
                // Element-wise product (scalar rule broadcasts a width-1 side).
                const int wA = inW(id, 0), wB = inW(id, 1);
                for (int e = 0; e < nOutElem; ++e) {
                    const int ea = (wA <= 1) ? 0 : e;
                    const int eb = (wB <= 1) ? 0 : e;
                    ScalarRef x
                        = srcA.first.empty() ? zeroRef() : resolve(srcA.first, srcA.second, ea);
                    ScalarRef y
                        = srcB.first.empty() ? zeroRef() : resolve(srcB.first, srcB.second, eb);
                    emitProduct(id + (nOutElem <= 1 ? "__mm0" : "__mme" + std::to_string(e)), x, y);
                }
                return;
            }
            // Matrix product: out(i,j) = sum_k A(i,k) * B(k,j). Column-major.
            const int ra = std::max(1, outD.first);
            const int cb = std::max(1, outD.second);
            const int ca = std::max(1, inDim(id, 0).second); // == rows(B)
            for (int j = 0; j < cb; ++j) {
                for (int i = 0; i < ra; ++i) {
                    const int idx = j * ra + i;
                    const std::string base
                        = id + (nOutElem <= 1 ? "__mm0" : "__mme" + std::to_string(idx));
                    std::vector<std::string> terms;
                    for (int k = 0; k < ca; ++k) {
                        const int aElem = k * ra + i; // A(i,k) col-major (ra x ca)
                        const int bElem = j * ca + k; // B(k,j) col-major (ca x cb)
                        ScalarRef x = srcA.first.empty() ? zeroRef()
                                                         : resolve(srcA.first, srcA.second, aElem);
                        ScalarRef y = srcB.first.empty() ? zeroRef()
                                                         : resolve(srcB.first, srcB.second, bElem);
                        const std::string mid = base + "_k" + std::to_string(k);
                        emitProduct(mid, x, y);
                        terms.push_back(mid);
                    }
                    json sp;
                    addBlock(base, "sum", (int)terms.size(), 1, sp);
                    for (int t = 0; t < (int)terms.size(); ++t) {
                        addConn({ terms[t], 0 }, base, t);
                    }
                }
            }
        }

        // Expand a vector reduction into scalar sum/mult blocks. resolve() maps
        // the scalar output to "<id>__red" ("<id>__cr<c>" for crossProduct).
        void
        emitReduction(const std::string& id, const std::string& type)
        {
            if (type == "crossProduct") {
                const auto srcA = srcOf(id, 0);
                const auto srcB = srcOf(id, 1);
                auto a = [&](int e) {
                    return srcA.first.empty() ? zeroRef() : resolve(srcA.first, srcA.second, e);
                };
                auto bb = [&](int e) {
                    return srcB.first.empty() ? zeroRef() : resolve(srcB.first, srcB.second, e);
                };
                // component c = a[j]*b[k] - a[k]*b[j]
                static const int J[3] = { 1, 2, 0 };
                static const int K[3] = { 2, 0, 1 };
                for (int c = 0; c < 3; ++c) {
                    const std::string cs = std::to_string(c);
                    const std::string p1 = id + "__cr" + cs + "_p1";
                    const std::string p2 = id + "__cr" + cs + "_p2";
                    addBlock(p1, "mult", 2, 1, json::object());
                    addConn(a(J[c]), p1, 0);
                    addConn(bb(K[c]), p1, 1);
                    addBlock(p2, "mult", 2, 1, json::object());
                    addConn(a(K[c]), p2, 0);
                    addConn(bb(J[c]), p2, 1);
                    json sp;
                    sp[kSumInputs] = "+-";
                    addBlock(id + "__cr" + cs, "sum", 2, 1, sp);
                    addConn({ p1, 0 }, id + "__cr" + cs, 0);
                    addConn({ p2, 0 }, id + "__cr" + cs, 1);
                }
                return;
            }
            const int W = std::max(1, inW(id, 0));
            if (type == "dotProduct") {
                const auto srcA = srcOf(id, 0);
                const auto srcB = srcOf(id, 1);
                for (int e = 0; e < W; ++e) {
                    const std::string pid = id + "__dp" + std::to_string(e);
                    addBlock(pid, "mult", 2, 1, json::object());
                    addConn(srcA.first.empty() ? zeroRef() : resolve(srcA.first, srcA.second, e),
                        pid, 0);
                    addConn(srcB.first.empty() ? zeroRef() : resolve(srcB.first, srcB.second, e),
                        pid, 1);
                }
                json sp;
                addBlock(id + "__red", "sum", W, 1, sp);
                for (int e = 0; e < W; ++e) {
                    addConn({ id + "__dp" + std::to_string(e), 0 }, id + "__red", e);
                }
                return;
            }
            // sumElements / productOfElements over the W elements of input 0.
            const auto src = srcOf(id, 0);
            const std::string acc = (type == "sumElements") ? "sum" : "mult";
            addBlock(id + "__red", acc, W, 1, json::object());
            for (int e = 0; e < W; ++e) {
                addConn(src.first.empty() ? zeroRef() : resolve(src.first, src.second, e),
                    id + "__red", e);
            }
        }

        // Lower an L5+ lookup block into codegen-only luScalar helpers. The
        // static Breakpoints / Table are baked as luScalar params. resolveImpl
        // maps each output element to the matching luScalar.
        void
        emitLookupLowered(const std::string& id, const std::string& type)
        {
            nflow::VarMap vm;
            auto it = byId.find(id);
            const json params = (it != byId.end()) ? jval(it->second, kParams) : json::object();
            if (type == "prelookup") {
                std::vector<double> bp = parseList(jval(params, "BreakpointsForDimension1"), vm);
                json bpArr = json::array();
                for (double x : bp) {
                    bpArr.push_back(x);
                }
                auto s = srcOf(id, 0);
                ScalarRef u = s.first.empty() ? zeroRef() : resolve(s.first, s.second, 0);
                json pk;
                pk["mode"] = "prelookupK";
                pk["Breakpoints"] = bpArr;
                addBlock(id + "__plK", "luScalar", 1, 1, pk);
                addConn(u, id + "__plK", 0);
                json pf;
                pf["mode"] = "prelookupF";
                pf["Breakpoints"] = bpArr;
                addBlock(id + "__plF", "luScalar", 1, 1, pf);
                addConn(u, id + "__plF", 0);
                return;
            }
            if (type == "interpolationPrelookup") {
                std::vector<double> tbl = parseList(jval(params, "Table"), vm);
                json tArr = json::array();
                for (double x : tbl) {
                    tArr.push_back(x);
                }
                auto s = srcOf(id, 0);
                ScalarRef kk = s.first.empty() ? zeroRef() : resolve(s.first, s.second, 0);
                ScalarRef ff = s.first.empty() ? zeroRef() : resolve(s.first, s.second, 1);
                json p;
                p["mode"] = "interp";
                p["Table"] = tArr;
                addBlock(id + "__ip", "luScalar", 2, 1, p);
                addConn(kk, id + "__ip", 0);
                addConn(ff, id + "__ip", 1);
                return;
            }
            if (type == "lookupDynamic") {
                const int W = std::max(1, inW(id, 1));
                auto sx = srcOf(id, 0);
                auto sb = srcOf(id, 1);
                auto st = srcOf(id, 2);
                ScalarRef x = sx.first.empty() ? zeroRef() : resolve(sx.first, sx.second, 0);
                json p;
                p["mode"] = "dynamic";
                p["W"] = W;
                addBlock(id + "__ld", "luScalar", 1 + 2 * W, 1, p);
                addConn(x, id + "__ld", 0);
                for (int j = 0; j < W; ++j) {
                    addConn(sb.first.empty() ? zeroRef() : resolve(sb.first, sb.second, j),
                        id + "__ld", 1 + j);
                }
                for (int j = 0; j < W; ++j) {
                    addConn(st.first.empty() ? zeroRef() : resolve(st.first, st.second, j),
                        id + "__ld", 1 + W + j);
                }
                return;
            }
        }

        VectorExpandResult
        run()
        {
            widths = computeCodegenWidths(diagram);
            if (!widths.ok) {
                result.ok = false;
                result.error = widths.error;
                return result;
            }
            if (widths.allScalar && !widths.anyBus) {
                result.diagram = diagram; // nothing to do
                return result;
            }
            {
                // computeCodegenWidths already validated the declarations.
                std::string busErr;
                (void)Nelson::NFlow::parseBusRegistry(diagram, namedBusReg, busErr);
            }

            if (diagram.contains(kBlocks) && diagram[kBlocks].is_array()) {
                for (const auto& b : diagram[kBlocks]) {
                    byId[jstr(b, kId)] = b;
                }
            }
            if (diagram.contains(kConnections) && diagram[kConnections].is_array()) {
                for (const auto& c : diagram[kConnections]) {
                    inputSrc[jstr(c, kTo)][jint(c, kToIndex, 0)]
                        = { jstr(c, kFrom), jint(c, kFromIndex, 0) };
                }
            }

            for (const auto& b : diagram[kBlocks]) {
                const std::string id = jstr(b, kId);
                const std::string type = jstr(b, kType);
                if (kRouting.count(type)) {
                    continue; // routing vanishes (resolved through)
                }
                if (type == "width") {
                    // The scalar generators see an all-scalar diagram, so the
                    // input width is only known here: lower to a constant.
                    json p = json::object();
                    p["Value"] = inW(id, 0);
                    addBlock(id, "constant", 0, 1, p);
                    continue;
                }
                if (type == "matmul") {
                    emitMatmul(id, jval(b, kParams));
                    continue;
                }
                if (type == "sumElements" || type == "productOfElements" || type == "dotProduct"
                    || type == "crossProduct") {
                    emitReduction(id, type);
                    continue;
                }
                if (type == "prelookup" || type == "interpolationPrelookup"
                    || type == "lookupDynamic") {
                    emitLookupLowered(id, type);
                    continue;
                }
                const json params = jval(b, kParams);
                const int nIn = numInputs(id);
                const int nOut = numOutputs(id);

                if (type == "condGate" && nIn > 0 && inW(id, 0) > 1) {
                    // The runtime enables while ANY control element > 0; the
                    // scalar gate cannot reproduce that, so reject rather than
                    // silently gate on element 0.
                    result.ok = false;
                    result.error = "vector control signals are not supported for "
                                   "generated conditional subsystems";
                    result.errorBlockId = id;
                    return result;
                }

                if (type == nflow::kLabelSource) {
                    // One scalar label source per element, tags "<tag>_e<k>".
                    // The labelSink renaming matches, so a goto/from pair stays
                    // paired per element.
                    const int w = (nOut > 0) ? outW(id, 0) : 1;
                    int busId = -1;
                    auto bt = widths.outBusIds.find(id);
                    if (bt != widths.outBusIds.end() && !bt->second.empty()) {
                        busId = bt->second[0];
                    }
                    std::vector<std::pair<std::string, bool>> leaves;
                    if (busId >= 0) {
                        std::function<bool(int, int)> walk = [&](int tid, int depth) -> bool {
                            const Nelson::NFlow::BusType* t = namedBusReg.get(tid);
                            if (!t || depth > 32) {
                                return true;
                            }
                            for (const auto& m : t->members) {
                                if (m.sig.busTypeId >= 0) {
                                    if (!walk(m.sig.busTypeId, depth + 1)) {
                                        return false;
                                    }
                                    continue;
                                }
                                const int mw = std::max(1, m.sig.width());
                                const std::string tn = Nelson::NFlow::sigTypeName(m.sig.type);
                                for (int k = 0; k < mw; ++k) {
                                    leaves.emplace_back(tn, m.sig.isComplex);
                                }
                            }
                            return true;
                        };
                        walk(busId, 0);
                        for (const auto& lf : leaves) {
                            if (lf.second) {
                                result.ok = false;
                                result.error = "block '" + id
                                    + "': complex bus members are not supported for "
                                      "bus-typed label sources in generated code";
                                result.errorBlockId = id;
                                return result;
                            }
                        }
                    }
                    json elemTags = json::array();
                    const std::string baseTag = jstr(params, kGotoTag, id);
                    for (int e = 0; e < w; ++e) {
                        json p = elemParams(params, e, w, type);
                        std::string tag = baseTag;
                        if (w > 1) {
                            tag += "_e" + std::to_string(e);
                        }
                        p[kGotoTag] = tag;
                        elemTags.push_back(tag);
                        // The element sources are scalar leaves: drop the bus
                        // declaration and stamp the exact-64 leaf type so the
                        // expanded layout types each external input port.
                        p.erase(kBusType);
                        if (busId >= 0 && e < (int)leaves.size()
                            && (leaves[(size_t)e].first == "int64"
                                || leaves[(size_t)e].first == "uint64")) {
                            p[kOutDataType] = leaves[(size_t)e].first;
                        }
                        const std::string cid = (w > 1) ? elemId(id, e) : id;
                        addBlock(cid, type, 0, 1, p);
                    }
                    const Nelson::NFlow::BusType* t = namedBusReg.get(busId);
                    if (t && t->nonvirtual && !t->name.empty()) {
                        json m;
                        m[kName] = baseTag;
                        m[kBus] = t->name;
                        m[kElements] = std::move(elemTags);
                        busInputsMeta.push_back(std::move(m));
                    }
                    continue;
                }

                if (type == nflow::kLabelSink) {
                    const int w = (nIn > 0) ? inW(id, 0) : 1;
                    json elemTags = json::array();
                    for (int e = 0; e < w; ++e) {
                        json p = params.is_object() ? params : json::object();
                        std::string tag = jstr(params, kGotoTag, id);
                        if (w > 1) {
                            tag += "_e" + std::to_string(e);
                        }
                        p[kGotoTag] = tag;
                        elemTags.push_back(tag);
                        const std::string cid = (w > 1) ? elemId(id, e) : id;
                        // outputs:1 like an original labelSink so the layout
                        // types the sink's pass-through port (an exact-64
                        // element keeps its integer lane to the struct field).
                        addBlock(cid, type, 1, 1, p);
                        auto s = srcOf(id, 0);
                        if (!s.first.empty()) {
                            addConn(resolve(s.first, s.second, e), cid, 0);
                        }
                    }
                    // A named nonvirtual bus feeding the sink is recorded so the
                    // generators can re-group the per-element sinks into a struct
                    // output. Anonymous and virtual buses stay flattened.
                    const auto s0 = srcOf(id, 0);
                    if (!s0.first.empty()) {
                        int busId = -1;
                        auto bt = widths.outBusIds.find(s0.first);
                        if (bt != widths.outBusIds.end() && s0.second >= 0
                            && s0.second < (int)bt->second.size()) {
                            busId = bt->second[s0.second];
                        }
                        const Nelson::NFlow::BusType* t = namedBusReg.get(busId);
                        if (t && t->nonvirtual && !t->name.empty()) {
                            json m;
                            m[kName] = jstr(params, kGotoTag, id);
                            m[kBus] = t->name;
                            m[kElements] = std::move(elemTags);
                            busOutputsMeta.push_back(std::move(m));
                        }
                    }
                    continue;
                }

                if (type == "combinatorialLogic") {
                    // One block: the vector input becomes N scalar input ports,
                    // in MSB-first bit order of the runtime's index fold.
                    const int w = std::max(1, inW(id, 0));
                    addBlock(id, type, w, 1, params);
                    auto s = srcOf(id, 0);
                    for (int e = 0; e < w; ++e) {
                        if (!s.first.empty()) {
                            addConn(resolve(s.first, s.second, e), id, e);
                        }
                    }
                    continue;
                }

                if (type == "stateSpace"
                    && (((nIn > 0) ? inW(id, 0) : 1) > 1 || ((nOut > 0) ? outW(id, 0) : 1) > 1)) {
                    // MIMO: the coupled states cannot be cloned per element.
                    // The nu-wide input becomes nu scalar input ports and the
                    // ny-wide output becomes ny scalar output ports
                    // (out_<id>, out_<id>_p1, ...).
                    const int nu = (nIn > 0) ? std::max(1, inW(id, 0)) : 0;
                    const int ny = std::max(1, (nOut > 0) ? outW(id, 0) : 1);
                    addBlock(id, type, std::max(0, nu), ny, params);
                    auto s = srcOf(id, 0);
                    for (int e = 0; e < nu; ++e) {
                        if (!s.first.empty()) {
                            addConn(resolve(s.first, s.second, e), id, e);
                        }
                    }
                    continue;
                }

                if (kMultiPortSinks.count(type)) {
                    int scalarPort = 0;
                    int totalPorts = 0;
                    for (int p = 0; p < nIn; ++p) {
                        totalPorts += inW(id, p);
                    }
                    addBlock(id, type, std::max(1, totalPorts), 0, params);
                    for (int p = 0; p < nIn; ++p) {
                        const int w = inW(id, p);
                        auto s = srcOf(id, p);
                        for (int e = 0; e < w; ++e, ++scalarPort) {
                            if (!s.first.empty()) {
                                addConn(resolve(s.first, s.second, e), id, scalarPort);
                            }
                        }
                    }
                    continue;
                }

                // Generic expandable block: W scalar copies (W = output width,
                // or input width for a pure sink like terminator).
                int w = 1;
                if (nOut > 0) {
                    w = outW(id, 0);
                } else if (nIn > 0) {
                    w = inW(id, 0);
                }
                for (int e = 0; e < w; ++e) {
                    const std::string cid = (w > 1) ? elemId(id, e) : id;
                    addBlock(cid, type, nIn, std::max(0, nOut), elemParams(params, e, w, type));
                    // Element e, or element 0 for a broadcast input.
                    for (int p = 0; p < nIn; ++p) {
                        auto s = srcOf(id, p);
                        if (s.first.empty()) {
                            continue;
                        }
                        const int srcElem = (inW(id, p) <= 1) ? 0 : e;
                        addConn(resolve(s.first, s.second, srcElem), cid, p);
                    }
                }
            }

            if (needZero) {
                json p;
                p[kValue] = 0;
                addBlock("nflow_zero__c", "constant", 0, 1, p);
            }

            // Top-level fields are kept; blocks and connections are replaced.
            json out = diagram;
            out[kBlocks] = std::move(outBlocks);
            out[kConnections] = std::move(outConns);
            if (!busOutputsMeta.empty()) {
                out[kNonvirtualBusOutputs] = std::move(busOutputsMeta);
            }
            if (!busInputsMeta.empty()) {
                out[kNonvirtualBusInputs] = std::move(busInputsMeta);
            }
            result.diagram = std::move(out);
            return result;
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
VectorExpandResult
expandVectorDiagram(const json& diagram)
{
    return Expander(diagram).run();
}
//=============================================================================
bool
expandsToGeneratableCode(const std::string& type, const std::string& lang)
{
    // The lowering is language-agnostic, so support does not depend on `lang`.
    (void)lang;
    return kExpansionLoweredReductions.count(type) != 0
        || kExpansionLoweredLookups.count(type) != 0;
}
//=============================================================================
} // namespace nflow
//=============================================================================
