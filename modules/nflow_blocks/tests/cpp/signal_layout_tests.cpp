//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for the typed / vector signal layer:
//   - quantizeToType (saturate / wrap / rounding / boolean / single);
//   - buildSignalLayout dimension + type propagation;
//   - the routing and matrix blocks (mux/demux/reshape/concatenate/convert/
//     matmul) and MIMO state-space output widths;
//   - the documented compile-time rejections (vector into a scalar-only block,
//     non-double into a continuous-dynamics block, type/width mismatch, and the
//     code-generation vector guard);
//   - element-wise vectorization of the discrete filters (dtf/ddelay/dstateSpace);
//   - vector signals crossing subsystem boundaries (both directions);
//   - FMU array-valued output ports.
//=============================================================================
#include "test_helpers.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "SimEngineBusTypes.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <cstdio>
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Block;
using Nelson::NFlow::BlockId;
using Nelson::NFlow::BlockRegistry;
using Nelson::NFlow::buildSignalLayout;
using Nelson::NFlow::PortSig;
using Nelson::NFlow::quantizeToType;
using Nelson::NFlow::SignalConn;
using Nelson::NFlow::SignalLayout;
using Nelson::NFlow::SigRounding;
using Nelson::NFlow::SigType;
using Nelson::NFlow::sigTypeFromString;
using Nelson::NFlow::sigTypeName;
using Nelson::NFlow::sigTypeRange;
using Nelson::NFlow::ValMap;
//=============================================================================
namespace {
//=============================================================================
// Assembles a block list and a connection list, then runs buildSignalLayout.
struct LayoutBuilder
{
    std::vector<Block> blocks;
    std::vector<SignalConn> conns;
    std::unordered_map<std::string, std::string> labelSinks;
    std::unordered_map<std::string, BlockId> nameToId;
    ValMap variables;

    BlockId
    add(const std::string& id, const std::string& type, int inputs, int outputs,
        json params = json::object())
    {
        Block b;
        b.id = id;
        b.nid = static_cast<BlockId>(blocks.size());
        b.type = type;
        b.inputs = inputs;
        b.outputs = outputs;
        asJson(b.params) = std::move(params);
        nameToId[id] = b.nid;
        if (type == nflow::kLabelSink && asJson(b.params).contains(nflow::kGotoTag)
            && asJson(b.params)[nflow::kGotoTag].is_string()) {
            labelSinks[asJson(b.params)[nflow::kGotoTag].get<std::string>()] = id;
        }
        blocks.push_back(std::move(b));
        return static_cast<BlockId>(blocks.size() - 1);
    }

    BlockId
    constVec(const std::string& id, json valueList, const std::string& outType = "double")
    {
        json p;
        p[nflow::kValue] = std::move(valueList);
        p[nflow::kOutDataType] = outType;
        return add(id, "constant", 0, 1, std::move(p));
    }

    void
    wire(BlockId from, BlockId to, int toPort = 0, int fromPort = 0)
    {
        conns.push_back(SignalConn { from, fromPort, to, toPort });
    }

    SignalLayout
    build()
    {
        return buildSignalLayout(blocks, conns, labelSinks, nameToId, variables, nullptr);
    }

    SignalLayout
    buildWithBus(Nelson::NFlow::BusRegistry& busReg)
    {
        return buildSignalLayout(blocks, conns, labelSinks, nameToId, variables, nullptr, &busReg);
    }

    const PortSig&
    sig(const SignalLayout& l, BlockId nid, int port = 0) const
    {
        return l.sigs[l.portBase[nid] + port];
    }
};
//=============================================================================
} // namespace
//=============================================================================
TEST_CASE("quantizeToType saturates integer types to their range")
{
    CHECK(quantizeToType(200.0, SigType::Int8, true) == doctest::Approx(127.0));
    CHECK(quantizeToType(-200.0, SigType::Int8, true) == doctest::Approx(-128.0));
    CHECK(quantizeToType(300.0, SigType::UInt8, true) == doctest::Approx(255.0));
    CHECK(quantizeToType(-5.0, SigType::UInt8, true) == doctest::Approx(0.0));
    CHECK(quantizeToType(70000.0, SigType::Int16, true) == doctest::Approx(32767.0));
    // Infinities saturate rather than propagate.
    CHECK(quantizeToType(std::numeric_limits<double>::infinity(), SigType::Int32, true)
        == doctest::Approx(2147483647.0));
}
//=============================================================================
TEST_CASE("quantizeToType wraps modulo 2^n when saturation is off")
{
    // int8 span 256: 200 -> 200 - 256 = -56; 130 -> -126.
    CHECK(quantizeToType(200.0, SigType::Int8, false) == doctest::Approx(-56.0));
    CHECK(quantizeToType(130.0, SigType::Int8, false) == doctest::Approx(-126.0));
    CHECK(quantizeToType(300.0, SigType::UInt8, false) == doctest::Approx(44.0));
    CHECK(quantizeToType(-1.0, SigType::UInt8, false) == doctest::Approx(255.0));
    CHECK(quantizeToType(-128.0, SigType::Int8, false) == doctest::Approx(-128.0));
    CHECK(quantizeToType(127.0, SigType::Int8, false) == doctest::Approx(127.0));
}
//=============================================================================
TEST_CASE("quantizeToType honours the rounding mode")
{
    CHECK(quantizeToType(2.5, SigType::Int8, true, SigRounding::Nearest) == doctest::Approx(3.0));
    CHECK(quantizeToType(2.7, SigType::Int8, true, SigRounding::Floor) == doctest::Approx(2.0));
    CHECK(quantizeToType(2.1, SigType::Int8, true, SigRounding::Ceiling) == doctest::Approx(3.0));
    CHECK(quantizeToType(-2.7, SigType::Int8, true, SigRounding::Zero) == doctest::Approx(-2.0));
    // NaN into an integer type collapses to 0.
    CHECK(quantizeToType(std::numeric_limits<double>::quiet_NaN(), SigType::Int16, true)
        == doctest::Approx(0.0));
}
//=============================================================================
TEST_CASE("quantizeToType handles boolean / single / double semantics")
{
    CHECK(quantizeToType(5.0, SigType::Boolean) == doctest::Approx(1.0));
    CHECK(quantizeToType(-3.0, SigType::Boolean) == doctest::Approx(1.0));
    CHECK(quantizeToType(0.0, SigType::Boolean) == doctest::Approx(0.0));
    CHECK(quantizeToType(std::numeric_limits<double>::quiet_NaN(), SigType::Boolean)
        == doctest::Approx(0.0));
    // Single: exact float values survive; non-representable values lose precision
    // but stay close.
    CHECK(quantizeToType(0.5, SigType::Single) == doctest::Approx(0.5));
    CHECK(quantizeToType(1.0 / 3.0, SigType::Single)
        == doctest::Approx(static_cast<double>(static_cast<float>(1.0 / 3.0))));
    // Double is a pass-through.
    CHECK(quantizeToType(3.14159265358979, SigType::Double) == 3.14159265358979);
}
//=============================================================================
TEST_CASE("sigType name / range / parse round-trip")
{
    for (SigType t : { SigType::Double, SigType::Single, SigType::Int8, SigType::UInt8,
             SigType::Int16, SigType::UInt16, SigType::Int32, SigType::UInt32, SigType::Boolean,
             SigType::Int64, SigType::UInt64 }) {
        SigType parsed = SigType::Double;
        REQUIRE(sigTypeFromString(sigTypeName(t), parsed));
        CHECK(parsed == t);
    }
    SigType unused = SigType::Double;
    CHECK_FALSE(sigTypeFromString("int128", unused));
    double lo = 0.0, hi = 0.0;
    sigTypeRange(SigType::UInt16, lo, hi);
    CHECK(lo == doctest::Approx(0.0));
    CHECK(hi == doctest::Approx(65535.0));
}
//=============================================================================
TEST_CASE("buildSignalLayout: scalar signals stay width 1")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", 3.0);
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    b.wire(c, g);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, c).width() == 1);
    CHECK(b.sig(l, g).width() == 1);
    CHECK(b.sig(l, g).type == SigType::Double);
}
//=============================================================================
TEST_CASE("buildSignalLayout: a vector constant propagates its width downstream")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 }));
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    b.wire(c, g);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, c).rows() == 3);
    CHECK(b.sig(l, g).width() == 3);
    // Slots are contiguous: constant owns [0..3), gain owns [3..6).
    CHECK(b.sig(l, c).offset == 0);
    CHECK(b.sig(l, g).offset == 3);
    CHECK(l.totalSlots == 6);
}
//=============================================================================
TEST_CASE("buildSignalLayout: scalar broadcasts against a vector (element-wise max)")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId s = b.constVec("s", 10.0); // scalar
    BlockId v = b.constVec("v", json::array({ 1.0, 2.0, 3.0 })); // width 3
    BlockId sum = b.add("sum", "sum", 2, 1, json::object());
    b.wire(s, sum, 0);
    b.wire(v, sum, 1);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, sum).width() == 3);
}
//=============================================================================
TEST_CASE("buildSignalLayout: two different non-scalar widths are a compile error")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId a = b.constVec("a", json::array({ 1.0, 2.0 })); // width 2
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 })); // width 3
    BlockId sum = b.add("sum", "sum", 2, 1, json::object());
    b.wire(a, sum, 0);
    b.wire(c, sum, 1);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("width mismatch") != std::string::npos);
    // The offending block is named for the UI.
    CHECK(std::find(l.errorBlocks.begin(), l.errorBlocks.end(), "sum") != l.errorBlocks.end());
}
//=============================================================================
TEST_CASE("buildSignalLayout: a typed constant carries its declared type")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 }), "int8");
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, c).type == SigType::Int8);
    CHECK(b.sig(l, c).width() == 3);
}
//=============================================================================
TEST_CASE("buildSignalLayout: a comparison block forces a Boolean output")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId a = b.constVec("a", 1.0, "int16");
    BlockId c = b.constVec("c", 2.0); // double
    BlockId rel = b.add("rel", "relationalOperator", 2, 1, json::object());
    b.wire(a, rel, 0);
    b.wire(c, rel, 1);
    SignalLayout l = b.build();
    // Comparison blocks accept a mix of input types and emit Boolean.
    REQUIRE(l.ok);
    CHECK(b.sig(l, rel).type == SigType::Boolean);
}
//=============================================================================
TEST_CASE("buildSignalLayout: mixed input types on a same-type block are rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId a = b.constVec("a", 1.0); // double
    BlockId c = b.constVec("c", 2.0, "int8"); // int8
    BlockId sum = b.add("sum", "sum", 2, 1, json::object());
    b.wire(a, sum, 0);
    b.wire(c, sum, 1);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("type mismatch") != std::string::npos);
    CHECK(l.error.find("convert") != std::string::npos);
}
//=============================================================================
TEST_CASE("buildSignalLayout: a switch accepts mixed input types")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId a = b.constVec("a", 1.0); // double data
    BlockId ctrl = b.constVec("ctrl", 1.0); // control
    BlockId c = b.constVec("c", 2.0, "int8"); // int8 data
    BlockId sw = b.add("sw", "switch", 3, 1, json::object());
    b.wire(a, sw, 0);
    b.wire(ctrl, sw, 1);
    b.wire(c, sw, 2);
    SignalLayout l = b.build();
    CHECK(l.ok);
}
//=============================================================================
TEST_CASE("mux concatenates input widths; mixed types are rejected")
{
    ensureHandlersRegistered();
    {
        LayoutBuilder b;
        BlockId a = b.constVec("a", json::array({ 1.0, 2.0 })); // width 2
        BlockId c = b.constVec("c", json::array({ 3.0, 4.0, 5.0 })); // width 3
        BlockId mux = b.add("mux", "mux", 2, 1, json::object());
        b.wire(a, mux, 0);
        b.wire(c, mux, 1);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, mux).width() == 5);
    }
    {
        LayoutBuilder b;
        BlockId a = b.constVec("a", 1.0); // double
        BlockId c = b.constVec("c", 2.0, "int8"); // int8
        BlockId mux = b.add("mux", "mux", 2, 1, json::object());
        b.wire(a, mux, 0);
        b.wire(c, mux, 1);
        SignalLayout l = b.build();
        CHECK_FALSE(l.ok);
        CHECK(l.error.find("mix signal types") != std::string::npos);
    }
}
//=============================================================================
TEST_CASE("demux splits width evenly; a non-divisible width is rejected")
{
    ensureHandlersRegistered();
    {
        LayoutBuilder b;
        BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 })); // width 6
        BlockId dem = b.add("dem", "demux", 1, 3, json::object());
        b.wire(c, dem);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, dem, 0).width() == 2);
        CHECK(b.sig(l, dem, 1).width() == 2);
        CHECK(b.sig(l, dem, 2).width() == 2);
    }
    {
        LayoutBuilder b;
        BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0 })); // width 5
        BlockId dem = b.add("dem", "demux", 1, 2, json::object());
        b.wire(c, dem);
        SignalLayout l = b.build();
        CHECK_FALSE(l.ok);
        CHECK(l.error.find("not divisible") != std::string::npos);
    }
}
//=============================================================================
TEST_CASE("reshape reorients; an element-count change is rejected")
{
    ensureHandlersRegistered();
    {
        LayoutBuilder b;
        BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
        json p;
        p[nflow::kOutputDims] = json::array({ 2.0, 3.0 });
        BlockId rs = b.add("rs", "reshape", 1, 1, p);
        b.wire(c, rs);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, rs).rows() == 2);
        CHECK(b.sig(l, rs).cols() == 3);
        CHECK(b.sig(l, rs).numDims() == 2);
    }
    {
        LayoutBuilder b;
        BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
        json p;
        p[nflow::kOutputDims] = json::array({ 2.0, 2.0 }); // 4 != 6
        BlockId rs = b.add("rs", "reshape", 1, 1, p);
        b.wire(c, rs);
        SignalLayout l = b.build();
        CHECK_FALSE(l.ok);
        CHECK(l.error.find("element count") != std::string::npos);
    }
}
//=============================================================================
TEST_CASE("concatenate stacks along the requested dimension")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId a = b.constVec("a", json::array({ 1.0, 2.0 })); // [2x1]
    BlockId c = b.constVec("c", json::array({ 3.0, 4.0, 5.0 })); // [3x1]
    json p;
    p[nflow::kConcatDim] = 1.0; // vertical
    BlockId cat = b.add("cat", "concatenate", 2, 1, p);
    b.wire(a, cat, 0);
    b.wire(c, cat, 1);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, cat).rows() == 5);
    CHECK(b.sig(l, cat).width() == 5);
}
//=============================================================================
// N-D signals (> 2 dims): reshape produces them, element-wise blocks adopt
// them, concatenate stacks along a higher dimension, and the guards hold.
//=============================================================================
TEST_CASE("reshape produces an N-D shape that element-wise blocks adopt")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec(
        "c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 }));
    json p;
    p[nflow::kOutputDims] = json::array({ 2.0, 3.0, 2.0 });
    BlockId rs = b.add("rs", "reshape", 1, 1, p);
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    b.wire(c, rs);
    b.wire(rs, g);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK((b.sig(l, rs).dims == std::vector<int> { 2, 3, 2 }));
    CHECK(b.sig(l, rs).numDims() == 3);
    CHECK(b.sig(l, rs).width() == 12);
    // default rule: gain adopts the full N-D shape of its input
    CHECK((b.sig(l, g).dims == std::vector<int> { 2, 3, 2 }));
    CHECK(b.sig(l, g).width() == 12);
}
//=============================================================================
TEST_CASE("trailing singleton dimensions normalize away")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
    json p;
    p[nflow::kOutputDims] = json::array({ 2.0, 3.0, 1.0 }); // [2x3x1] == [2x3]
    BlockId rs = b.add("rs", "reshape", 1, 1, p);
    b.wire(c, rs);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK((b.sig(l, rs).dims == std::vector<int> { 2, 3 }));
    CHECK(b.sig(l, rs).numDims() == 2);
}
//=============================================================================
TEST_CASE("concatenate stacks along dimension 3")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ca = b.constVec("ca", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
    BlockId cb = b.constVec("cb", json::array({ 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 }));
    json pr;
    pr[nflow::kOutputDims] = json::array({ 2.0, 3.0 });
    BlockId ra = b.add("ra", "reshape", 1, 1, pr);
    BlockId rb = b.add("rb", "reshape", 1, 1, pr);
    json pc;
    pc[nflow::kConcatDim] = 3.0;
    BlockId cat = b.add("cat", "concatenate", 2, 1, pc);
    b.wire(ca, ra);
    b.wire(cb, rb);
    b.wire(ra, cat, 0);
    b.wire(rb, cat, 1);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK((b.sig(l, cat).dims == std::vector<int> { 2, 3, 2 }));
    CHECK(b.sig(l, cat).width() == 12);
}
//=============================================================================
TEST_CASE("mismatched N-D shapes into an element-wise block are rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ca = b.constVec(
        "ca", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 }));
    BlockId cb = b.constVec(
        "cb", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 }));
    json pa;
    pa[nflow::kOutputDims] = json::array({ 2.0, 3.0, 2.0 });
    BlockId ra = b.add("ra", "reshape", 1, 1, pa);
    json pb;
    pb[nflow::kOutputDims] = json::array({ 3.0, 2.0, 2.0 });
    BlockId rb = b.add("rb", "reshape", 1, 1, pb);
    BlockId s = b.add("s", "sum", 2, 1, json::object());
    b.wire(ca, ra);
    b.wire(cb, rb);
    b.wire(ra, s, 0);
    b.wire(rb, s, 1);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("mismatch") != std::string::npos);
}
//=============================================================================
TEST_CASE("matrix product rejects N-D inputs")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ca = b.constVec(
        "ca", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 }));
    BlockId cb = b.constVec("cb", json::array({ 1.0, 2.0 }));
    json pa;
    pa[nflow::kOutputDims] = json::array({ 2.0, 3.0, 2.0 });
    BlockId ra = b.add("ra", "reshape", 1, 1, pa);
    BlockId mm = b.add("mm", "matmul", 2, 1, json::object());
    b.wire(ca, ra);
    b.wire(cb, mm, 1);
    b.wire(ra, mm, 0);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("2-D") != std::string::npos);
}
//=============================================================================
TEST_CASE("a reshape beyond the dimension cap is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0 }));
    json p;
    json dims = json::array();
    for (int k = 0; k < Nelson::NFlow::kMaxSignalDims + 1; ++k) {
        dims.push_back(1.0);
    }
    dims[0] = 2.0;
    p[nflow::kOutputDims] = dims;
    BlockId rs = b.add("rs", "reshape", 1, 1, p);
    b.wire(c, rs);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("dimensions") != std::string::npos);
}
//=============================================================================
TEST_CASE("matmul resolves the matrix-product output shape")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ca = b.constVec("ca", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
    BlockId cb = b.constVec("cb", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
    json pa;
    pa[nflow::kOutputDims] = json::array({ 2.0, 3.0 });
    BlockId ra = b.add("ra", "reshape", 1, 1, pa);
    json pb;
    pb[nflow::kOutputDims] = json::array({ 3.0, 2.0 });
    BlockId rb = b.add("rb", "reshape", 1, 1, pb);
    BlockId mm = b.add("mm", "matmul", 2, 1, json::object());
    b.wire(ca, ra);
    b.wire(cb, rb);
    b.wire(ra, mm, 0);
    b.wire(rb, mm, 1);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, mm).rows() == 2); // [2x3] * [3x2] -> [2x2]
    CHECK(b.sig(l, mm).cols() == 2);
    CHECK(b.sig(l, mm).numDims() == 2);
}
//=============================================================================
TEST_CASE("matmul rejects an inner-dimension mismatch")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ca = b.constVec("ca", json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 }));
    BlockId cb = b.constVec("cb", json::array({ 1.0, 2.0, 3.0, 4.0 }));
    json pa;
    pa[nflow::kOutputDims] = json::array({ 2.0, 3.0 });
    BlockId ra = b.add("ra", "reshape", 1, 1, pa);
    json pb;
    pb[nflow::kOutputDims] = json::array({ 2.0, 2.0 });
    BlockId rb = b.add("rb", "reshape", 1, 1, pb);
    BlockId mm = b.add("mm", "matmul", 2, 1, json::object());
    b.wire(ca, ra);
    b.wire(cb, rb);
    b.wire(ra, mm, 0);
    b.wire(rb, mm, 1);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("dimension mismatch") != std::string::npos);
}
//=============================================================================
TEST_CASE("matmul element-wise rule requires equal input dimensions")
{
    ensureHandlersRegistered();
    {
        LayoutBuilder b;
        BlockId a = b.constVec("a", json::array({ 1.0, 2.0, 3.0 }));
        BlockId c = b.constVec("c", json::array({ 4.0, 5.0, 6.0 }));
        json p;
        p[nflow::kMulRule] = "elementwise";
        BlockId mm = b.add("mm", "matmul", 2, 1, p);
        b.wire(a, mm, 0);
        b.wire(c, mm, 1);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, mm).width() == 3);
    }
    {
        LayoutBuilder b;
        BlockId a = b.constVec("a", json::array({ 1.0, 2.0 }));
        BlockId c = b.constVec("c", json::array({ 4.0, 5.0, 6.0 }));
        json p;
        p[nflow::kMulRule] = "elementwise";
        BlockId mm = b.add("mm", "matmul", 2, 1, p);
        b.wire(a, mm, 0);
        b.wire(c, mm, 1);
        SignalLayout l = b.build();
        CHECK_FALSE(l.ok);
        CHECK(l.error.find("equal input dimensions") != std::string::npos);
    }
}
//=============================================================================
TEST_CASE("convert casts the type and preserves dimensions")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 })); // double, width 3
    json p;
    p[nflow::kOutDataType] = "int16";
    BlockId cv = b.add("cv", "convert", 1, 1, p);
    b.wire(c, cv);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, cv).type == SigType::Int16);
    CHECK(b.sig(l, cv).width() == 3);
}
//=============================================================================
TEST_CASE("state-space output width is the number of C rows (MIMO)")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId u = b.constVec("u", json::array({ 1.0, 2.0 })); // 2-input drive
    json p;
    p[nflow::kA] = json::array({ 0.0, 1.0, -1.0, -1.0 }); // 2x2 -> nx = 2
    p[nflow::kB] = json::array({ 0.0, 0.0, 1.0, 1.0 }); // 2x2
    p[nflow::kC] = json::array({ 1.0, 0.0, 0.0, 1.0 }); // 2x2 -> ny = 2
    p[nflow::kD] = json::array({ 0.0, 0.0, 0.0, 0.0 });
    BlockId ss = b.add("ss", "stateSpace", 1, 1, p);
    b.wire(u, ss);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, ss).width() == 2);
    CHECK(b.sig(l, ss).type == SigType::Double);
}
//=============================================================================
TEST_CASE("a vector signal into a scalar-only block is a clear compile error")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 }));
    BlockId xy = b.add("xy", "xyScope", 2, 1, json::object()); // x-y trace is scalar-only
    b.wire(c, xy, 0);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("does not support vector signals") != std::string::npos);
    CHECK(std::find(l.errorBlocks.begin(), l.errorBlocks.end(), "xy") != l.errorBlocks.end());
}
//=============================================================================
TEST_CASE("the discrete filters vectorize element-wise (n independent channels)")
{
    ensureHandlersRegistered();
    // Like tf, these accept a vector wire: each element is an independent
    // SISO channel.
    for (const char* type : { "dtf", "ddelay", "dstateSpace" }) {
        LayoutBuilder b;
        BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 }));
        BlockId d = b.add("d", type, 1, 1, json::object());
        b.wire(c, d);
        SignalLayout l = b.build();
        CHECK(l.ok);
        CHECK(b.sig(l, d).width() == 3);
    }
}
//=============================================================================
TEST_CASE("a non-double signal into a continuous-dynamics block is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", 5.0, "int8"); // scalar int8
    BlockId integ = b.add("i", "integrator", 1, 1, json::object());
    b.wire(c, integ);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("requires real double signals") != std::string::npos);
    CHECK(l.error.find("convert") != std::string::npos);
}
//=============================================================================
// Subsystem boundary crossing, in both directions.
//=============================================================================
TEST_CASE("a vector produced inside a subsystem crosses to its output port")
{
    ensureHandlersRegistered();
    json spec = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "c" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 },
                    { nflow::kParams, { { nflow::kValue, json::array({ 1.0, 2.0, 3.0 }) } } } },
                { { nflow::kId, "out" }, { nflow::kType, nflow::kLabelSink }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kGotoTag, "out" } } } },
            }) },
        { nflow::kConnections,
            json::array({ { { nflow::kFrom, "c" }, { nflow::kTo, "out" }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, 0 } } }) },
        { nflow::kExternalOutputs,
            json::array({ { { nflow::kId, "out" }, { nflow::kName, "out" } } }) },
    };
    LayoutBuilder b;
    BlockId sub = b.add("sub", "subsystem", 0, 1, json { { nflow::kSubsystem, spec } });
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, sub).width() == 3);
}
//=============================================================================
TEST_CASE("a vector wired into a subsystem crosses in and back out")
{
    ensureHandlersRegistered();
    json spec = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "in" }, { nflow::kType, nflow::kLabelSource },
                    { nflow::kInputs, 0 }, { nflow::kOutputs, 1 },
                    { nflow::kParams, json::object() } },
                { { nflow::kId, "g" }, { nflow::kType, "gain" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { "gain", 2.0 } } } },
                { { nflow::kId, "out" }, { nflow::kType, nflow::kLabelSink }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kGotoTag, "out" } } } },
            }) },
        { nflow::kConnections,
            json::array({
                { { nflow::kFrom, "in" }, { nflow::kTo, "g" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
                { { nflow::kFrom, "g" }, { nflow::kTo, "out" }, { nflow::kFromIndex, 0 },
                    { nflow::kToIndex, 0 } },
            }) },
        { nflow::kExternalInputs,
            json::array({ { { nflow::kId, "in" }, { nflow::kName, "in" } } }) },
        { nflow::kExternalOutputs,
            json::array({ { { nflow::kId, "out" }, { nflow::kName, "out" } } }) },
    };
    LayoutBuilder b;
    BlockId drive = b.constVec("drive", json::array({ 1.0, 2.0, 3.0 })); // width 3
    BlockId sub = b.add("sub", "subsystem", 1, 1, json { { nflow::kSubsystem, spec } });
    b.wire(drive, sub);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, sub).width() == 3);
}
//=============================================================================
TEST_CASE("FMU output-port widths come from the outputWidths parameter")
{
    ensureHandlersRegistered();
    {
        LayoutBuilder b;
        json p;
        p["outputWidths"] = json::array({ 3.0, 1.0 });
        BlockId fmu = b.add("fmu", nflow::kFmu, 0, 2, p);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, fmu, 0).width() == 3);
        CHECK(b.sig(l, fmu, 1).width() == 1);
    }
    {
        // A single-array FMU: the host JSON may collapse a 1-element list to a
        // scalar; the width still resolves.
        LayoutBuilder b;
        json p;
        p["outputWidths"] = 4.0;
        BlockId fmu = b.add("fmu", nflow::kFmu, 0, 1, p);
        SignalLayout l = b.build();
        REQUIRE(l.ok);
        CHECK(b.sig(l, fmu, 0).width() == 4);
    }
}
//=============================================================================
TEST_CASE("width analysis: an all-scalar diagram reports no vector port")
{
    ensureHandlersRegistered();
    json diagram = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "c" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { nflow::kValue, 1.0 } } } },
                { { nflow::kId, "g" }, { nflow::kType, "gain" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { "gain", 2.0 } } } },
            }) },
        { nflow::kConnections,
            json::array({ { { nflow::kFrom, "c" }, { nflow::kTo, "g" }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, 0 } } }) },
    };
    CHECK(nflow::firstVectorSignalBlockId(diagram).empty());
}
//=============================================================================
TEST_CASE("width analysis: a vector diagram names the first wide block")
{
    ensureHandlersRegistered();
    json diagram = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "c" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 },
                    { nflow::kParams, { { nflow::kValue, json::array({ 1.0, 2.0, 3.0 }) } } } },
                { { nflow::kId, "g" }, { nflow::kType, "gain" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 }, { nflow::kParams, { { "gain", 2.0 } } } },
            }) },
        { nflow::kConnections,
            json::array({ { { nflow::kFrom, "c" }, { nflow::kTo, "g" }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, 0 } } }) },
    };
    CHECK(nflow::firstVectorSignalBlockId(diagram) == "c");
}
//=============================================================================
// Exact 64-bit integer semantics: values a double-backed model cannot represent.
//=============================================================================
TEST_CASE("quantizeDoubleToI64 saturates and wraps at the 64-bit bounds")
{
    using Nelson::NFlow::quantizeDoubleToI64;
    constexpr long long i64max = std::numeric_limits<long long>::max();
    constexpr long long i64min = std::numeric_limits<long long>::min();
    // Saturation at the bounds a double cannot represent exactly.
    CHECK(quantizeDoubleToI64(1e19, SigType::Int64, true) == i64max);
    CHECK(quantizeDoubleToI64(-1e19, SigType::Int64, true) == i64min);
    CHECK(quantizeDoubleToI64(1e20, SigType::UInt64, true) == (long long)~0ULL);
    CHECK(quantizeDoubleToI64(-5.0, SigType::UInt64, true) == 0);
    // In-range doubles convert exactly.
    CHECK(quantizeDoubleToI64(4503599627370496.0, SigType::Int64, true) == 4503599627370496LL);
    CHECK(quantizeDoubleToI64(-2.5, SigType::Int64, true, SigRounding::Floor) == -3);
    CHECK(quantizeDoubleToI64(std::numeric_limits<double>::quiet_NaN(), SigType::Int64) == 0);
}
//=============================================================================
TEST_CASE("i64 arithmetic saturates exactly at INT64/UINT64 bounds")
{
    using namespace Nelson::NFlow;
    constexpr long long i64max = std::numeric_limits<long long>::max();
    constexpr long long i64min = std::numeric_limits<long long>::min();
    const long long u64max = (long long)~0ULL;
    // (INT64_MAX - 3) + 10 saturates; wraps to INT64_MIN + 6.
    CHECK(i64Add(i64max - 3, 10, true) == i64max);
    CHECK(i64Add(i64max - 3, 10, false) == i64min + 6);
    // (UINT64_MAX) + 10 saturates; wraps to 9 (modulo 2^64).
    CHECK(u64Add(u64max, 10, true) == u64max);
    CHECK(u64Add(u64max, 10, false) == 9);
    // Subtraction below zero (unsigned) and below INT64_MIN (signed).
    CHECK(u64Sub(5, 10, true) == 0);
    CHECK(i64Sub(i64min, 1, true) == i64min);
    CHECK(i64Sub(i64min, 1, false) == i64max);
    // Multiplication overflow, both signs.
    CHECK(i64Mul(i64max, 2, true) == i64max);
    CHECK(i64Mul(i64min, 2, true) == i64min);
    CHECK(u64Mul(u64max, 2, true) == u64max);
    // abs / negate at the asymmetric bound.
    CHECK(i64ApplyAbs(i64min, SigType::Int64, true) == i64max);
    CHECK(i64ApplyNeg(i64min, SigType::Int64, true) == i64max);
    CHECK(i64ApplyNeg(5, SigType::UInt64, true) == 0);
    // Exact division rounding.
    CHECK(i64Div(7, 2, true, SigRounding::Nearest) == 4);
    CHECK(i64Div(-7, 2, true, SigRounding::Nearest) == -4);
    CHECK(i64Div(7, 2, true, SigRounding::Floor) == 3);
    CHECK(i64Div(-7, 2, true, SigRounding::Floor) == -4);
    CHECK(i64Div(7, 2, true, SigRounding::Zero) == 3);
    CHECK(i64Div(i64min, -1, true, SigRounding::Nearest) == i64max);
    // Unsigned ordering above INT64_MAX (would be negative as signed).
    CHECK(i64Less(5, u64max, SigType::UInt64));
    CHECK_FALSE(i64Less(u64max, 5, SigType::UInt64));
    // Exact cross-casts.
    CHECK(requantizeI64(-3, SigType::Int64, SigType::UInt64, true) == 0);
    CHECK(requantizeI64(u64max, SigType::UInt64, SigType::Int64, true) == i64max);
}
//=============================================================================
TEST_CASE("an int64 constant demands the 64-bit lane and feeds exact math")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0 }), "int64");
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 3.0 } });
    b.wire(c, g);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, c).type == SigType::Int64);
    CHECK(b.sig(l, g).type == SigType::Int64);
    CHECK(l.hasExact64);
    CHECK_FALSE(l.hasComplex);
}
//=============================================================================
TEST_CASE("an exact-64 signal into a block without an exact path is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0 }), "uint64");
    BlockId u = b.add("u", "unitDelay", 1, 1, json::object());
    b.wire(c, u);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("uint64") != std::string::npos);
    CHECK(l.error.find("convert") != std::string::npos);
}
//=============================================================================
// Complex signals: builders force complexity on, extractors force it off, and
// blocks without an imaginary-lane path reject complex inputs.
//=============================================================================
TEST_CASE("realImagToComplex forces a complex output that propagates through math")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId re = b.constVec("re", json::array({ 1.0, 2.0 }));
    BlockId im = b.constVec("im", json::array({ 3.0, 4.0 }));
    BlockId cplx = b.add("cplx", "realImagToComplex", 2, 1, json::object());
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    BlockId mag = b.add("mag", "abs", 1, 1, json::object());
    b.wire(re, cplx, 0);
    b.wire(im, cplx, 1);
    b.wire(cplx, g);
    b.wire(g, mag);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK_FALSE(b.sig(l, re).isComplex);
    CHECK(b.sig(l, cplx).isComplex);
    CHECK(b.sig(l, g).isComplex); // OR-propagation
    CHECK_FALSE(b.sig(l, mag).isComplex); // abs -> real magnitude
    CHECK(l.hasComplex);
    CHECK(b.sig(l, cplx).width() == 2);
}
//=============================================================================
TEST_CASE("complexToRealImag emits two real ports of the input shape")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId re = b.constVec("re", json::array({ 1.0, 2.0, 3.0 }));
    BlockId cplx = b.add("cplx", "realImagToComplex", 2, 1, json::object());
    BlockId split = b.add("split", "complexToRealImag", 1, 2, json::object());
    b.wire(re, cplx, 0);
    b.wire(cplx, split);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK_FALSE(b.sig(l, split, 0).isComplex);
    CHECK_FALSE(b.sig(l, split, 1).isComplex);
    CHECK(b.sig(l, split, 0).width() == 3);
    CHECK(b.sig(l, split, 1).width() == 3);
}
//=============================================================================
TEST_CASE("a complex signal into a block without a complex path is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId re = b.constVec("re", json::array({ 1.0 }));
    BlockId cplx = b.add("cplx", "realImagToComplex", 2, 1, json::object());
    BlockId u = b.add("u", "unitDelay", 1, 1, json::object());
    b.wire(re, cplx, 0);
    b.wire(cplx, u);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("complex") != std::string::npos);
    CHECK(l.error.find("complexToRealImag") != std::string::npos);
}
//=============================================================================
TEST_CASE("a complex signal into a continuous-dynamics block is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId re = b.constVec("re", json::array({ 1.0 }));
    BlockId cplx = b.add("cplx", "realImagToComplex", 2, 1, json::object());
    BlockId ig = b.add("ig", "integrator", 1, 1, json::object());
    b.wire(re, cplx, 0);
    b.wire(cplx, ig);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("complex") != std::string::npos);
}
//=============================================================================
TEST_CASE("bus registry: packed layout and nested member paths")
{
    using Nelson::NFlow::BusMember;
    using Nelson::NFlow::BusRegistry;
    using Nelson::NFlow::BusType;
    BusRegistry reg;
    BusType inner;
    inner.name = "Inner";
    {
        BusMember a;
        a.name = "a";
        a.sig.setVector(2);
        inner.members.push_back(a);
        BusMember b;
        b.name = "b";
        b.sig.type = SigType::Int64;
        inner.members.push_back(b);
    }
    const int innerId = reg.add(inner);
    BusType outer;
    outer.name = "Outer";
    outer.nonvirtual = true;
    {
        BusMember p;
        p.name = "pos";
        p.sig.setVector(3);
        outer.members.push_back(p);
        BusMember s;
        s.name = "sub";
        s.sig.busTypeId = innerId;
        outer.members.push_back(s);
        BusMember q;
        q.name = "ok";
        q.sig.type = SigType::Boolean;
        outer.members.push_back(q);
    }
    const int outerId = reg.add(outer);
    CHECK(reg.packedWidth(innerId) == 3); // [2] + scalar
    CHECK(reg.packedWidth(outerId) == 7); // [3] + 3 + scalar
    CHECK(reg.memberOffset(outerId, "pos") == 0);
    CHECK(reg.memberOffset(outerId, "sub") == 3);
    CHECK(reg.memberOffset(outerId, "sub.b") == 5);
    CHECK(reg.memberOffset(outerId, "ok") == 6);
    CHECK(reg.memberOffset(outerId, "nope") == -1);
    CHECK(reg.memberOffset(outerId, "pos.x") == -1); // descends into a leaf
    const PortSig* ps = reg.resolvePath(outerId, "sub.b");
    REQUIRE(ps != nullptr);
    CHECK(ps->type == SigType::Int64);
    CHECK(reg.resolvePath(outerId, "sub.c") == nullptr);
}
//=============================================================================
TEST_CASE("bus registry: structural equality ignores ids, not structure")
{
    using Nelson::NFlow::BusMember;
    using Nelson::NFlow::BusRegistry;
    using Nelson::NFlow::BusType;
    BusRegistry reg;
    auto makeType = [](const char* name, int width) {
        BusType t;
        t.name = name;
        BusMember m;
        m.name = "v";
        m.sig.setVector(width);
        t.members.push_back(m);
        return t;
    };
    const int a = reg.add(makeType("A", 4));
    const int b = reg.add(makeType("B", 4));
    const int c = reg.add(makeType("C", 5));
    CHECK(reg.structurallyEqual(a, b));
    CHECK_FALSE(reg.structurallyEqual(a, c));
}
//=============================================================================
TEST_CASE("busTypes parser: nested by name, rejects unknown and duplicates")
{
    using Nelson::NFlow::BusRegistry;
    using Nelson::NFlow::parseBusRegistry;
    {
        json d = { { nflow::kBusTypes,
            json::array(
                { { { nflow::kName, "Outer" },
                      { nflow::kMembers,
                          json::array({ { { nflow::kName, "pos" }, { nflow::kType, "double" },
                                            { nflow::kDims, json::array({ 3.0 }) } },
                              { { nflow::kName, "sub" }, { "bus", "Inner" } } }) } },
                    { { nflow::kName, "Inner" }, { "nonvirtual", true },
                        { nflow::kMembers,
                            json::array({ { { nflow::kName, "b" },
                                { nflow::kType, "int64" } } }) } } }) } };
        BusRegistry reg;
        std::string err;
        REQUIRE(parseBusRegistry(d, reg, err));
        const int outer = reg.idOf("Outer");
        REQUIRE(outer >= 0);
        CHECK(reg.packedWidth(outer) == 4);
        CHECK(reg.memberOffset(outer, "sub.b") == 3);
        CHECK(reg.types[(size_t)reg.idOf("Inner")].nonvirtual);
    }
    {
        json d = { { nflow::kBusTypes,
            json::array({ { { nflow::kName, "X" },
                { nflow::kMembers,
                    json::array({ { { nflow::kName, "m" }, { "bus", "Missing" } } }) } } }) } };
        BusRegistry reg;
        std::string err;
        CHECK_FALSE(parseBusRegistry(d, reg, err));
        CHECK(err.find("Missing") != std::string::npos);
    }
    {
        json d = { { nflow::kBusTypes,
            json::array({ { { nflow::kName, "X" }, { nflow::kMembers, json::array() } },
                { { nflow::kName, "X" }, { nflow::kMembers, json::array() } } }) } };
        BusRegistry reg;
        std::string err;
        CHECK_FALSE(parseBusRegistry(d, reg, err));
        CHECK(err.find("duplicate") != std::string::npos);
    }
}
//=============================================================================
TEST_CASE("busCreator infers a packed bus and busSelector extracts members")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId v = b.constVec("v", json::array({ 1.0, 2.0 }));
    BlockId k = b.constVec("k", json::array({ 7.0 }), "int64");
    json pc;
    pc[nflow::kMemberNames] = json::array({ "pos", "count" });
    BlockId bc = b.add("bc", "busCreator", 2, 1, pc);
    json psel;
    psel[nflow::kSelectedSignals] = json::array({ "count", "pos" });
    BlockId sel = b.add("sel", "busSelector", 1, 2, psel);
    b.wire(v, bc, 0);
    b.wire(k, bc, 1);
    b.wire(bc, sel);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    REQUIRE(l.ok);
    CHECK(b.sig(l, bc).busTypeId >= 0);
    CHECK(b.sig(l, bc).width() == 3); // [2] + scalar, packed
    CHECK(l.hasExact64); // the int64 member demands the 64-bit lane
    CHECK(b.sig(l, sel, 0).type == SigType::Int64); // "count"
    CHECK(b.sig(l, sel, 0).width() == 1);
    CHECK(b.sig(l, sel, 1).width() == 2); // "pos"
    CHECK(b.sig(l, sel, 1).busTypeId == -1);
}
//=============================================================================
TEST_CASE("busSelector can return the selected members as one bus")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId v = b.constVec("v", json::array({ 1.0, 2.0 }));
    BlockId k = b.constVec("k", json::array({ 7.0 }), "int64");
    json pc;
    pc[nflow::kMemberNames] = json::array({ "pos", "count" });
    BlockId bc = b.add("bc", "busCreator", 2, 1, pc);
    json psel;
    psel[nflow::kSelectedSignals] = json::array({ "count", "pos" });
    psel[nflow::kOutputAsBus] = true;
    BlockId sel = b.add("sel", "busSelector", 1, 1, psel);
    b.wire(v, bc, 0);
    b.wire(k, bc, 1);
    b.wire(bc, sel);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    REQUIRE(l.ok);
    CHECK(b.sig(l, sel).busTypeId >= 0);
    CHECK(b.sig(l, sel).width() == 3);
    const Nelson::NFlow::BusType* outBus = reg.get(b.sig(l, sel).busTypeId);
    REQUIRE(outBus != nullptr);
    REQUIRE(outBus->members.size() == 2);
    CHECK(outBus->members[0].name == "count");
    CHECK(outBus->members[0].sig.type == SigType::Int64);
    CHECK(outBus->members[1].name == "pos");
    CHECK(outBus->members[1].sig.width() == 2);
}
//=============================================================================
TEST_CASE("multiportSwitch preserves homogeneous data input type")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ctrl = b.constVec("ctrl", json::array({ 2.0 }));
    BlockId a = b.constVec("a", json::array({ 1.0 }), "int16");
    BlockId c = b.constVec("c", json::array({ 2.0 }), "int16");
    BlockId sw = b.add("sw", "multiportSwitch", 3, 1, json::object());
    b.wire(ctrl, sw, 0);
    b.wire(a, sw, 1);
    b.wire(c, sw, 2);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK(b.sig(l, sw).type == SigType::Int16);
}
//=============================================================================
TEST_CASE("multiportSwitch rejects mixed data input types")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId ctrl = b.constVec("ctrl", json::array({ 1.0 }));
    BlockId a = b.constVec("a", json::array({ 1.0 }), "int16");
    BlockId c = b.constVec("c", json::array({ 2.0 }), "single");
    BlockId sw = b.add("sw", "multiportSwitch", 3, 1, json::object());
    b.wire(ctrl, sw, 0);
    b.wire(a, sw, 1);
    b.wire(c, sw, 2);
    SignalLayout l = b.build();
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("multiportSwitch") != std::string::npos);
    CHECK(l.error.find("convert") != std::string::npos);
}
//=============================================================================
TEST_CASE("busSelector rejects an unknown member path naming the members")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId v = b.constVec("v", json::array({ 1.0 }));
    BlockId bc = b.add("bc", "busCreator", 1, 1, json::object());
    json psel;
    psel[nflow::kSelectedSignals] = json::array({ "nope" });
    BlockId sel = b.add("sel", "busSelector", 1, 1, psel);
    b.wire(v, bc, 0);
    b.wire(bc, sel);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("nope") != std::string::npos);
    CHECK(l.error.find("signal1") != std::string::npos);
}
//=============================================================================
TEST_CASE("a bus into a non-bus-aware block is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId v = b.constVec("v", json::array({ 1.0 }));
    BlockId bc = b.add("bc", "busCreator", 1, 1, json::object());
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    b.wire(v, bc, 0);
    b.wire(bc, g);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("busSelector") != std::string::npos);
}
//=============================================================================
TEST_CASE("a busSelector fed by a plain signal is rejected")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId v = b.constVec("v", json::array({ 1.0 }));
    json psel;
    psel[nflow::kSelectedSignals] = json::array({ "x" });
    BlockId sel = b.add("sel", "busSelector", 1, 1, psel);
    b.wire(v, sel);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    CHECK_FALSE(l.ok);
    CHECK(l.error.find("not a bus") != std::string::npos);
}
//=============================================================================
TEST_CASE("nested buses pack recursively and resolve dotted paths")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId x = b.constVec("x", json::array({ 1.0, 2.0 }));
    json pInner;
    pInner[nflow::kMemberNames] = json::array({ "a" });
    BlockId inner = b.add("inner", "busCreator", 1, 1, pInner);
    BlockId y = b.constVec("y", json::array({ 9.0 }));
    json pOuter;
    pOuter[nflow::kMemberNames] = json::array({ "sub", "flag" });
    BlockId outer = b.add("outer", "busCreator", 2, 1, pOuter);
    json psel;
    psel[nflow::kSelectedSignals] = json::array({ "sub.a" });
    BlockId sel = b.add("sel", "busSelector", 1, 1, psel);
    b.wire(x, inner, 0);
    b.wire(inner, outer, 0);
    b.wire(y, outer, 1);
    b.wire(outer, sel);
    Nelson::NFlow::BusRegistry reg;
    SignalLayout l = b.buildWithBus(reg);
    REQUIRE(l.ok);
    CHECK(b.sig(l, outer).width() == 3); // nested [2] + scalar
    CHECK(b.sig(l, sel, 0).width() == 2); // sub.a
}
//=============================================================================
TEST_CASE("a plain diagram demands no sibling storage lane")
{
    ensureHandlersRegistered();
    LayoutBuilder b;
    BlockId c = b.constVec("c", json::array({ 1.0, 2.0, 3.0 }), "int16");
    BlockId g = b.add("g", "gain", 1, 1, json { { "gain", 2.0 } });
    b.wire(c, g);
    SignalLayout l = b.build();
    REQUIRE(l.ok);
    CHECK_FALSE(l.hasComplex);
    CHECK_FALSE(l.hasExact64);
}
//=============================================================================
TEST_CASE("lane predicates key on the port descriptor")
{
    PortSig ps;
    CHECK_FALSE(Nelson::NFlow::portUsesImag(ps));
    CHECK_FALSE(Nelson::NFlow::portUsesI64(ps));
    ps.isComplex = true;
    CHECK(Nelson::NFlow::portUsesImag(ps));
    CHECK_FALSE(Nelson::NFlow::portUsesI64(ps)); // complexity != 64-bit lane
}
//=============================================================================
TEST_CASE("width analysis: an N-D port resolves its full shape")
{
    ensureHandlersRegistered();
    json diagram = {
        { nflow::kBlocks,
            json::array({
                { { nflow::kId, "c" }, { nflow::kType, "constant" }, { nflow::kInputs, 0 },
                    { nflow::kOutputs, 1 },
                    { nflow::kParams,
                        { { nflow::kValue,
                            json::array({ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                                12.0 }) } } } },
                { { nflow::kId, "rs" }, { nflow::kType, "reshape" }, { nflow::kInputs, 1 },
                    { nflow::kOutputs, 1 },
                    { nflow::kParams,
                        { { nflow::kOutputDims, json::array({ 2.0, 3.0, 2.0 }) } } } },
            }) },
        { nflow::kConnections,
            json::array({ { { nflow::kFrom, "c" }, { nflow::kTo, "rs" }, { nflow::kFromIndex, 0 },
                { nflow::kToIndex, 0 } } }) },
    };
    auto w = nflow::computeCodegenWidths(diagram);
    REQUIRE(w.ok);
    CHECK_FALSE(w.allScalar);
    REQUIRE(w.outDimsN.count("rs") == 1);
    CHECK((w.outDimsN["rs"][0] == std::vector<int> { 2, 3, 2 }));
    REQUIRE(w.outTypes.count("rs") == 1);
    CHECK(w.outTypes["rs"][0] == "double");
}
//=============================================================================
