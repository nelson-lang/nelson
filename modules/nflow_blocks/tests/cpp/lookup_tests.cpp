//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "test_helpers.hpp"
//=============================================================================
using namespace nflow_tests;
//=============================================================================
using Nelson::NFlow::Phase;
// Build a scalar lookup1D block with the given params and input, run the
// algebraic phase and return output port 0.
static double
lookup1d(json params, double u)
{
    Harness h;
    auto b = h.addBlock("b", "lookup1D", 1, 1, std::move(params));
    h.connect(h.addInput(u), b);
    h.run(0, Phase::ALGEBRAIC);
    return h.output(b, 0);
}
//=============================================================================
TEST_CASE("lookup1D interpolates a 1-D table")
{
    const json bp = { 0.0, 1.0, 2.0, 3.0, 4.0 };
    const json tbl = { 0.0, 1.0, 4.0, 9.0, 16.0 };
    auto P = [&](const char* interp, const char* extrap) {
        return json { { "BreakpointsForDimension1", bp }, { "Table", tbl },
            { "InterpMethod", interp }, { "ExtrapMethod", extrap } };
    };
    checkClose(lookup1d(P("Linear point-slope", "Linear"), 2.0), 4.0);
    checkClose(lookup1d(P("Linear point-slope", "Linear"), 1.5), 2.5);
    checkClose(lookup1d(P("Linear Lagrange", "Linear"), 1.5), 2.5);
    // Flat holds the lower value; Nearest rounds the fraction.
    checkClose(lookup1d(P("Flat", "Clip"), 1.7), 1.0);
    checkClose(lookup1d(P("Nearest", "Clip"), 1.4), 1.0);
    checkClose(lookup1d(P("Nearest", "Clip"), 1.6), 4.0);
    checkClose(lookup1d(P("Linear point-slope", "Clip"), -1.0), 0.0);
    checkClose(lookup1d(P("Linear point-slope", "Linear"), -1.0), -1.0);
    checkClose(lookup1d(P("Linear point-slope", "Clip"), 5.0), 16.0);
    checkClose(lookup1d(P("Linear point-slope", "Linear"), 5.0), 23.0);
}
//=============================================================================
static double
lookup2d(json params, double u1, double u2)
{
    Harness h;
    auto b = h.addBlock("b", "lookup2D", 2, 1, std::move(params));
    h.connect(h.addInput(u1), b, 0);
    h.connect(h.addInput(u2), b, 1);
    h.run(0, Phase::ALGEBRAIC);
    return h.output(b, 0);
}
//=============================================================================
TEST_CASE("lookup2D interpolates a 2-D table")
{
    // Column-major 2x2 table: T(0,0)=0, T(1,0)=10, T(0,1)=100, T(1,1)=110.
    const json bp1 = { 0.0, 1.0 };
    const json bp2 = { 0.0, 1.0 };
    const json tbl = { 0.0, 10.0, 100.0, 110.0 };
    auto P = [&](const char* interp) {
        return json { { "BreakpointsForDimension1", bp1 }, { "BreakpointsForDimension2", bp2 },
            { "Table", tbl }, { "InterpMethod", interp }, { "ExtrapMethod", "Linear" } };
    };
    checkClose(lookup2d(P("Linear point-slope"), 0.0, 0.0), 0.0);
    checkClose(lookup2d(P("Linear point-slope"), 1.0, 1.0), 110.0);
    checkClose(lookup2d(P("Linear point-slope"), 0.5, 0.0), 5.0);
    checkClose(lookup2d(P("Linear point-slope"), 0.0, 0.5), 50.0);
    // Center (bilinear average of the four corners).
    checkClose(lookup2d(P("Linear point-slope"), 0.5, 0.5), 55.0);
    // Flat holds the lower corner.
    checkClose(lookup2d(P("Flat"), 0.7, 0.7), 0.0);
    // Nearest rounds each axis independently.
    checkClose(lookup2d(P("Nearest"), 0.6, 0.4), 10.0);
}
//=============================================================================
static double
lookupnd(json params, const std::vector<double>& u)
{
    Harness h;
    auto b = h.addBlock("b", "lookupND", (int)u.size(), 1, std::move(params));
    for (int k = 0; k < (int)u.size(); ++k) {
        h.connect(h.addInput(u[(size_t)k]), b, k);
    }
    h.run(0, Phase::ALGEBRAIC);
    return h.output(b, 0);
}
//=============================================================================
TEST_CASE("lookupND interpolates an n-D table")
{
    // 3-D 2x2x2 table, column-major: T(i,j,k) = i + 10j + 100k.
    const json bp = { 0.0, 1.0 };
    const json tbl = { 0.0, 1.0, 10.0, 11.0, 100.0, 101.0, 110.0, 111.0 };
    auto P = [&](const char* interp) {
        return json { { "NumberOfTableDimensions", 3 }, { "BreakpointsForDimension1", bp },
            { "BreakpointsForDimension2", bp }, { "BreakpointsForDimension3", bp },
            { "Table", tbl }, { "InterpMethod", interp }, { "ExtrapMethod", "Linear" } };
    };
    checkClose(lookupnd(P("Linear point-slope"), { 0.0, 0.0, 0.0 }), 0.0);
    checkClose(lookupnd(P("Linear point-slope"), { 1.0, 1.0, 1.0 }), 111.0);
    // Trilinear centre = mean of the eight corners.
    checkClose(lookupnd(P("Linear point-slope"), { 0.5, 0.5, 0.5 }), 55.5);
    checkClose(lookupnd(P("Linear point-slope"), { 0.5, 0.0, 0.0 }), 0.5);
    checkClose(lookupnd(P("Linear point-slope"), { 0.0, 0.5, 0.0 }), 5.0);
    checkClose(lookupnd(P("Linear point-slope"), { 0.0, 0.0, 0.5 }), 50.0);
    // A 1-D lookupND matches a plain interpolation.
    const json bp4 = { 0.0, 1.0, 2.0 };
    const json tbl4 = { 0.0, 1.0, 4.0 };
    json p1 = { { "NumberOfTableDimensions", 1 }, { "BreakpointsForDimension1", bp4 },
        { "Table", tbl4 }, { "InterpMethod", "Linear point-slope" }, { "ExtrapMethod", "Linear" } };
    checkClose(lookupnd(p1, { 1.5 }), 2.5);
}
//=============================================================================
static double
directlookup(json params, const std::vector<double>& idx)
{
    Harness h;
    auto b = h.addBlock("b", "directLookup", (int)idx.size(), 1, std::move(params));
    for (int k = 0; k < (int)idx.size(); ++k) {
        h.connect(h.addInput(idx[(size_t)k]), b, k);
    }
    h.run(0, Phase::ALGEBRAIC);
    return h.output(b, 0);
}
//=============================================================================
TEST_CASE("directLookup selects a table element by integer index")
{
    // 2x3 column-major table: T(i,j) = i + 10j, dims [2,3] -> 6 elements.
    const json tbl = { 0.0, 1.0, 10.0, 11.0, 20.0, 21.0 };
    json P
        = { { "NumberOfTableDimensions", 2 }, { "TableDimensions", { 2, 3 } }, { "Table", tbl } };
    checkClose(directlookup(P, { 0.0, 0.0 }), 0.0);
    checkClose(directlookup(P, { 1.0, 0.0 }), 1.0);
    checkClose(directlookup(P, { 0.0, 2.0 }), 20.0);
    checkClose(directlookup(P, { 1.0, 2.0 }), 21.0);
    // Non-integer inputs round to nearest; out-of-range clamps.
    checkClose(directlookup(P, { 0.6, 1.4 }), 11.0);
    checkClose(directlookup(P, { 9.0, 9.0 }), 21.0);
    checkClose(directlookup(P, { -3.0, -3.0 }), 0.0);
}
//=============================================================================
