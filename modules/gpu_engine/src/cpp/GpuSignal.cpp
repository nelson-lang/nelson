//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cstring>
#include "GpuSignal.hpp"
#include "GpuKernels.hpp"
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static uint32_t
floatBits(float value)
{
    uint32_t bits;
    std::memcpy(&bits, &value, sizeof(bits));
    return bits;
}
//=============================================================================
static WGPUComputePipeline
pipeline(const std::string& key, const std::string& source)
{
    WGPUComputePipeline p = GpuContext::getInstance()->pipelineFor(key, source.c_str(), "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    return p;
}
//=============================================================================
static bool
isRealFloat(GpuArrayObject* a)
{
    return a != nullptr && !a->isComplex() && !a->isRawIntStorage()
        && GpuIntegerResultType(a, a).empty();
}
//=============================================================================
static const char* POLYVAL_WGSL = R"WGSL(
struct Params { n : u32, np : u32 };
@group(0) @binding(0) var<storage, read> p : array<f32>;
@group(0) @binding(1) var<storage, read> x : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let xv = x[i];
    var acc = p[0];
    for (var k = 1u; k < params.np; k = k + 1u) {
        acc = acc * xv + p[k];
    }
    c[i] = acc;
}
)WGSL";
//=============================================================================
bool
GpuPolyval(GpuArrayObject* p, GpuArrayObject* x, ArrayOf& result)
{
    if (!isRealFloat(p) || !isRealFloat(x)
        || !(p->dimensions().isVector() || p->dimensions().isScalar())) {
        return false;
    }
    GpuArrayObject* output = AllocateGpuArray(x->dimensions(), "single", false);
    result = ArrayOf::handle(output);
    uint32_t n = (uint32_t)x->elementCount();
    uint32_t np = (uint32_t)p->elementCount();
    if (n == 0) {
        return true;
    }
    if (np == 0) {
        result = GpuFilled(x->dimensions(), 0.0f, "single");
        return true;
    }
    uint32_t params[2] = { n, np };
    GpuDispatchKernel(pipeline("gpuArray:polyval", POLYVAL_WGSL),
        { p->buffer(), x->buffer(), output->buffer() }, params, 2, (n + 255u) / 256u, 1);
    return true;
}
//=============================================================================
static const char* CONV_WGSL = R"WGSL(
struct Params { n : u32, nu : u32, nv : u32, start : u32 };
@group(0) @binding(0) var<storage, read> u : array<f32>;
@group(0) @binding(1) var<storage, read> v : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let m = i + params.start;
    var first = 0u;
    if (m + 1u > params.nv) { first = m + 1u - params.nv; }
    let last = min(m, params.nu - 1u);
    var acc : f32 = 0.0;
    for (var j = first; j <= last; j = j + 1u) {
        acc = acc + u[j] * v[m - j];
    }
    c[i] = acc;
}
)WGSL";
//=============================================================================
bool
GpuConv(GpuArrayObject* u, GpuArrayObject* v, const std::string& shape, ArrayOf& result)
{
    if (!isRealFloat(u) || !isRealFloat(v)) {
        return false;
    }
    Dimensions du = u->dimensions();
    Dimensions dv = v->dimensions();
    bool uVector = du.isVector() || du.isScalar();
    bool vVector = dv.isVector() || dv.isScalar();
    if (!uVector || !vVector) {
        return false;
    }
    int64_t nu = (int64_t)u->elementCount();
    int64_t nv = (int64_t)v->elementCount();
    // one thread loops over the shorter vector: very long overlaps stay on the host
    if (std::min(nu, nv) > 65536) {
        return false;
    }
    int64_t length = 0;
    int64_t start = 0;
    bool column = false;
    auto isRow = [](const Dimensions& d) { return d.rows() == 1 && d.columns() > 1; };
    auto isColumn = [](const Dimensions& d) { return d.columns() == 1 && d.rows() > 1; };
    if (shape == "full") {
        length = (nu == 0 || nv == 0) ? 0 : nu + nv - 1;
        column = !isRow(du) && !isRow(dv) && (isColumn(du) || isColumn(dv));
    } else if (shape == "same") {
        length = nu;
        start = nv / 2;
        column = isColumn(du);
    } else if (shape == "valid") {
        length = std::max<int64_t>(nu - nv + 1, 0);
        start = nv - 1;
        column = isColumn(du);
    } else {
        return false;
    }
    Dimensions outDims = column ? Dimensions(length, 1) : Dimensions(1, length);
    GpuArrayObject* output = AllocateGpuArray(outDims, "single", false);
    result = ArrayOf::handle(output);
    if (length == 0 || nu == 0 || nv == 0) {
        return true;
    }
    uint32_t params[4] = { (uint32_t)length, (uint32_t)nu, (uint32_t)nv, (uint32_t)start };
    GpuDispatchKernel(pipeline("gpuArray:conv", CONV_WGSL),
        { u->buffer(), v->buffer(), output->buffer() }, params, 4, (uint32_t)((length + 255) / 256),
        1);
    return true;
}
//=============================================================================
// one output per thread: sum of A(i - p, j - q) * B(p, q) over the overlap, in full coordinates
static const char* CONV2_WGSL = R"WGSL(
struct Params { outRows : u32, outCols : u32, ma : u32, na : u32, mb : u32, nb : u32, rowOff : u32, colOff : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> b : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let index = gid.x;
    if (index >= params.outRows * params.outCols) { return; }
    let i = index % params.outRows + params.rowOff;
    let j = index / params.outRows + params.colOff;
    let pFirst = select(0u, i - (params.ma - 1u), i + 1u > params.ma);
    let pLast = min(params.mb - 1u, i);
    let qFirst = select(0u, j - (params.na - 1u), j + 1u > params.na);
    let qLast = min(params.nb - 1u, j);
    var s : f32 = 0.0;
    for (var q = qFirst; q <= qLast; q = q + 1u) {
        for (var p = pFirst; p <= pLast; p = p + 1u) {
            s = s + a[(i - p) + (j - q) * params.ma] * b[p + q * params.mb];
        }
    }
    c[index] = s;
}
)WGSL";
//=============================================================================
bool
GpuConv2(GpuArrayObject* a, GpuArrayObject* b, const std::string& shape, ArrayOf& result)
{
    if (!isRealFloat(a) || !isRealFloat(b) || a->dimensions().length() > 2
        || b->dimensions().length() > 2 || a->elementCount() == 0 || b->elementCount() == 0
        || b->elementCount() > 65536) {
        return false;
    }
    int64_t ma = (int64_t)a->dimensions().rows();
    int64_t na = (int64_t)a->dimensions().columns();
    int64_t mb = (int64_t)b->dimensions().rows();
    int64_t nb = (int64_t)b->dimensions().columns();
    int64_t rows = 0, cols = 0, rowOff = 0, colOff = 0;
    if (shape == "full") {
        rows = ma + mb - 1;
        cols = na + nb - 1;
    } else if (shape == "same") {
        rows = ma;
        cols = na;
        rowOff = mb / 2;
        colOff = nb / 2;
    } else if (shape == "valid") {
        rows = std::max<int64_t>(ma - mb + 1, 0);
        cols = std::max<int64_t>(na - nb + 1, 0);
        rowOff = mb - 1;
        colOff = nb - 1;
    } else {
        return false;
    }
    GpuArrayObject* output = AllocateGpuArray(Dimensions(rows, cols), "single", false);
    result = ArrayOf::handle(output);
    uint64_t total = (uint64_t)rows * (uint64_t)cols;
    if (total == 0) {
        return true;
    }
    uint32_t params[8] = { (uint32_t)rows, (uint32_t)cols, (uint32_t)ma, (uint32_t)na, (uint32_t)mb,
        (uint32_t)nb, (uint32_t)rowOff, (uint32_t)colOff };
    GpuDispatchKernel(pipeline("gpuArray:conv2", CONV2_WGSL),
        { a->buffer(), b->buffer(), output->buffer() }, params, 8, (uint32_t)((total + 255) / 256),
        1);
    return true;
}
//=============================================================================
// one query per thread on a grid of 2 or 3 axes (array dimensions): the grid vectors one after
// the other in `grids`, a binary search per axis, then linear (all corners) or nearest
static const char* INTERP_GRID_WGSL = R"WGSL(
struct Params { nq : u32, rank : u32, n0 : u32, n1 : u32, n2 : u32, nearest : u32, useFill : u32, fill : u32 };
@group(0) @binding(0) var<storage, read> grids : array<f32>;
@group(0) @binding(1) var<storage, read> v : array<f32>;
@group(0) @binding(2) var<storage, read> q0 : array<f32>;
@group(0) @binding(3) var<storage, read> q1 : array<f32>;
@group(0) @binding(4) var<storage, read> q2 : array<f32>;
@group(0) @binding(5) var<storage, read_write> out : array<f32>;
@group(0) @binding(6) var<uniform> params : Params;
fn isNan(t : f32) -> bool { return (bitcast<u32>(t) & 0x7fffffffu) > 0x7f800000u; }
fn axisLength(axis : u32) -> u32 {
    if (axis == 0u) { return params.n0; }
    if (axis == 1u) { return params.n1; }
    return params.n2;
}
fn axisStart(axis : u32) -> u32 {
    if (axis == 0u) { return 0u; }
    if (axis == 1u) { return params.n0; }
    return params.n0 + params.n1;
}
fn query(axis : u32, k : u32) -> f32 {
    if (axis == 0u) { return q0[k]; }
    if (axis == 1u) { return q1[k]; }
    return q2[k];
}
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let k = gid.x;
    if (k >= params.nq) { return; }
    var nanBits : u32 = 0x7fc00000u;
    var cellIndex = array<u32, 3>(0u, 0u, 0u);
    var frac = array<f32, 3>(0.0, 0.0, 0.0);
    for (var axis = 0u; axis < params.rank; axis = axis + 1u) {
        let t = query(axis, k);
        let n = axisLength(axis);
        let s = axisStart(axis);
        if (isNan(t)) { out[k] = bitcast<f32>(nanBits); return; }
        if (t < grids[s] || t > grids[s + n - 1u]) {
            out[k] = select(bitcast<f32>(nanBits), bitcast<f32>(params.fill), params.useFill == 1u);
            return;
        }
        var lo = 0u;
        var hi = n - 1u;
        loop {
            if (hi - lo <= 1u) { break; }
            let mid = (lo + hi) / 2u;
            if (grids[s + mid] <= t) { lo = mid; } else { hi = mid; }
        }
        cellIndex[axis] = lo;
        frac[axis] = (t - grids[s + lo]) / (grids[s + lo + 1u] - grids[s + lo]);
    }
    let stride1 = params.n0;
    let stride2 = params.n0 * params.n1;
    if (params.nearest == 1u) {
        var position = 0u;
        for (var axis = 0u; axis < params.rank; axis = axis + 1u) {
            let index = select(cellIndex[axis], cellIndex[axis] + 1u, frac[axis] >= 0.5);
            position = position + index * select(select(stride2, stride1, axis == 1u), 1u, axis == 0u);
        }
        out[k] = v[position];
        return;
    }
    var total : f32 = 0.0;
    let corners = 1u << params.rank;
    for (var c = 0u; c < corners; c = c + 1u) {
        var weight : f32 = 1.0;
        var position = 0u;
        for (var axis = 0u; axis < params.rank; axis = axis + 1u) {
            let upper = ((c >> axis) & 1u) == 1u;
            let index = cellIndex[axis] + select(0u, 1u, upper);
            weight = weight * select(1.0 - frac[axis], frac[axis], upper);
            position = position + index * select(select(stride2, stride1, axis == 1u), 1u, axis == 0u);
        }
        if (weight != 0.0) { total = total + weight * v[position]; }
    }
    out[k] = total;
}
)WGSL";
//=============================================================================
// strictly ascending grid vector of the expected length, as host values
static bool
ascendingGrid(GpuArrayObject* g, indexType expected, std::vector<single>& values)
{
    if (!isRealFloat(g) || g->elementCount() != expected || expected < 2
        || !(g->dimensions().isVector())) {
        return false;
    }
    ArrayOf host = GatherGpuArray(g);
    const single* p = (const single*)host.dataPointer();
    for (indexType k = 0; k + 1 < expected; ++k) {
        if (!(p[k] < p[k + 1])) {
            return false;
        }
    }
    values.insert(values.end(), p, p + expected);
    return true;
}
//=============================================================================
bool
GpuInterpGrid(const std::vector<GpuArrayObject*>& grids, GpuArrayObject* v,
    const std::vector<GpuArrayObject*>& queries, bool nearest, bool useFill, float fill,
    ArrayOf& result)
{
    size_t rank = grids.size();
    if (rank < 2 || rank > 3 || queries.size() != rank || !isRealFloat(v)
        || (indexType)v->dimensions().length() > (indexType)rank) {
        return false;
    }
    Dimensions queryDims = queries[0]->dimensions();
    for (GpuArrayObject* q : queries) {
        if (!isRealFloat(q) || !q->dimensions().equals(queryDims)) {
            return false;
        }
    }
    std::vector<single> packed;
    uint32_t lengths[3] = { 1u, 1u, 1u };
    for (size_t axis = 0; axis < rank; ++axis) {
        indexType n = v->dimensions().dimensionLength((indexType)axis);
        if (!ascendingGrid(grids[axis], n, packed)) {
            return false;
        }
        lengths[axis] = (uint32_t)n;
    }
    single* gridData = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, packed.size());
    std::copy(packed.begin(), packed.end(), gridData);
    ArrayOf gridOwner = CreateGpuArray(ArrayOf(NLS_SINGLE, Dimensions(packed.size(), 1), gridData));
    GpuArrayObject* output = AllocateGpuArray(queryDims, "single", false);
    result = ArrayOf::handle(output);
    uint32_t nq = (uint32_t)queryDims.elementCount();
    if (nq == 0) {
        return true;
    }
    uint32_t fillBits;
    std::memcpy(&fillBits, &fill, sizeof(fillBits));
    uint32_t params[8] = { nq, (uint32_t)rank, lengths[0], lengths[1], lengths[2],
        nearest ? 1u : 0u, useFill ? 1u : 0u, fillBits };
    GpuArrayObject* last = rank == 3 ? queries[2] : queries[0];
    GpuDispatchKernel(pipeline("gpuArray:interpgrid", INTERP_GRID_WGSL),
        { AsGpuArrayObject(gridOwner)->buffer(), v->buffer(), queries[0]->buffer(),
            queries[1]->buffer(), last->buffer(), output->buffer() },
        params, 8, (nq + 255u) / 256u, 1);
    return true;
}
//=============================================================================
// one lane per thread; coefficients are [b (nb), a (na)] already divided by a(1)
static const char* FILTER_WGSL = R"WGSL(
struct Params { laneCount : u32, redLen : u32, before : u32, nb : u32, na : u32 };
@group(0) @binding(0) var<storage, read> coef : array<f32>;
@group(0) @binding(1) var<storage, read> x : array<f32>;
@group(0) @binding(2) var<storage, read_write> y : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let lane = gid.x;
    if (lane >= params.laneCount) { return; }
    let start = lane % params.before + (lane / params.before) * params.before * params.redLen;
    let order = max(params.nb, params.na);
    // transposed direct form state
    var z : array<f32, 64>;
    for (var k = 0u; k < 64u; k = k + 1u) { z[k] = 0.0; }
    for (var t = 0u; t < params.redLen; t = t + 1u) {
        let e = start + t * params.before;
        let xv = x[e];
        let b0 = coef[0];
        let yv = b0 * xv + z[0];
        for (var k = 1u; k < order; k = k + 1u) {
            var bk : f32 = 0.0;
            var ak : f32 = 0.0;
            if (k < params.nb) { bk = coef[k]; }
            if (k < params.na) { ak = coef[params.nb + k]; }
            var next : f32 = 0.0;
            if (k < order - 1u) { next = z[k]; }
            z[k - 1u] = bk * xv - ak * yv + next;
        }
        y[e] = yv;
    }
}
)WGSL";
//=============================================================================
// output element = sum over the taps of b(k) * x(n - k) along the lane
static const char* FIR_WGSL = R"WGSL(
struct Params { n : u32, redLen : u32, before : u32, nb : u32 };
@group(0) @binding(0) var<storage, read> coef : array<f32>;
@group(0) @binding(1) var<storage, read> x : array<f32>;
@group(0) @binding(2) var<storage, read_write> y : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let t = (i / params.before) % params.redLen;
    var acc : f32 = 0.0;
    let taps = min(params.nb, t + 1u);
    for (var k = 0u; k < taps; k = k + 1u) {
        acc = acc + coef[k] * x[i - k * params.before];
    }
    y[i] = acc;
}
)WGSL";
//=============================================================================
bool
GpuFilter(const std::vector<float>& b, const std::vector<float>& a, GpuArrayObject* x, int dim,
    ArrayOf& result)
{
    if (!isRealFloat(x) || b.empty() || a.empty() || a[0] == 0.0f) {
        return false;
    }
    size_t order = std::max(b.size(), a.size());
    if (order > 64 || (a.size() > 1 && order > 64)) {
        return false;
    }
    GpuLaneGeometry lane = GpuComputeLaneGeometry(x->dimensions(), dim);
    bool fir = a.size() == 1;
    // a recursive filter runs one lane per thread: very long lanes stay on the host
    if (!fir && lane.redLen > (1u << 20)) {
        return false;
    }
    std::vector<float> coefficients;
    for (float value : b) {
        coefficients.push_back(value / a[0]);
    }
    for (float value : a) {
        coefficients.push_back(value / a[0]);
    }
    single* host = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, coefficients.size());
    std::copy(coefficients.begin(), coefficients.end(), host);
    ArrayOf coefOwner
        = CreateGpuArray(ArrayOf(NLS_SINGLE, Dimensions(1, coefficients.size()), host));
    GpuArrayObject* coef = AsGpuArrayObject(coefOwner);
    GpuArrayObject* output = AllocateGpuArray(x->dimensions(), "single", false);
    result = ArrayOf::handle(output);
    uint32_t n = (uint32_t)x->elementCount();
    if (n == 0) {
        return true;
    }
    if (lane.identity) {
        lane.laneCount = n;
        lane.redLen = 1;
        lane.before = 1;
    }
    if (fir) {
        uint32_t params[4] = { n, lane.redLen, lane.before, (uint32_t)b.size() };
        GpuDispatchKernel(pipeline("gpuArray:fir", FIR_WGSL),
            { coef->buffer(), x->buffer(), output->buffer() }, params, 4, (n + 255u) / 256u, 1);
        return true;
    }
    uint32_t params[5]
        = { lane.laneCount, lane.redLen, lane.before, (uint32_t)b.size(), (uint32_t)a.size() };
    GpuDispatchKernel(pipeline("gpuArray:iir", FILTER_WGSL),
        { coef->buffer(), x->buffer(), output->buffer() }, params, 5,
        (lane.laneCount + 255u) / 256u, 1);
    return true;
}
//=============================================================================
// x ascending; mode 0: NaN outside, 1: fill value outside, 2: extrapolate
static const char* INTERP1_WGSL = R"WGSL(
struct Params { nq : u32, n : u32, method : u32, mode : u32, fill : u32 };
@group(0) @binding(0) var<storage, read> x : array<f32>;
@group(0) @binding(1) var<storage, read> v : array<f32>;
@group(0) @binding(2) var<storage, read> q : array<f32>;
@group(0) @binding(3) var<storage, read_write> c : array<f32>;
@group(0) @binding(4) var<uniform> params : Params;
fn isNan(t : f32) -> bool { return (bitcast<u32>(t) & 0x7fffffffu) > 0x7f800000u; }
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.nq) { return; }
    let t = q[i];
    let last = params.n - 1u;
    var nanBits : u32 = 0x7fc00000u;
    if (isNan(t)) { c[i] = bitcast<f32>(nanBits); return; }
    let outside = t < x[0] || t > x[last];
    if (outside && params.mode != 2u) {
        if (params.mode == 1u) { c[i] = bitcast<f32>(params.fill); } else { c[i] = bitcast<f32>(nanBits); }
        return;
    }
    // segment k with x[k] <= t <= x[k + 1]
    var lo = 0u;
    var hi = last;
    if (t <= x[0]) { hi = 1u; }
    else if (t >= x[last]) { lo = last - 1u; }
    else {
        loop {
            if (hi - lo <= 1u) { break; }
            let mid = (lo + hi) / 2u;
            if (x[mid] <= t) { lo = mid; } else { hi = mid; }
        }
    }
    let k = min(lo, last - 1u);
    let x0 = x[k];
    let x1 = x[k + 1u];
    var r : f32;
    switch (params.method) {
        case 1u: {
            if (t < x0) { r = v[k]; }
            else if (t > x1) { r = v[k + 1u]; }
            else if (t - x0 < x1 - t) { r = v[k]; } else { r = v[k + 1u]; }
        }
        case 2u: {
            if (t >= x1) { r = v[k + 1u]; } else { r = v[k]; }
        }
        case 3u: {
            if (t <= x0) { r = v[k]; } else { r = v[k + 1u]; }
        }
        default: {
            r = v[k] + (v[k + 1u] - v[k]) * ((t - x0) / (x1 - x0));
        }
    }
    c[i] = r;
}
)WGSL";
//=============================================================================
bool
GpuInterp1(GpuArrayObject* x, GpuArrayObject* v, GpuArrayObject* q, const std::string& method,
    int extrapMode, float fillValue, ArrayOf& result)
{
    static const std::vector<std::string> methods = { "linear", "nearest", "previous", "next" };
    auto found = std::find(methods.begin(), methods.end(), method);
    if (found == methods.end() || !isRealFloat(x) || !isRealFloat(v) || !isRealFloat(q)) {
        return false;
    }
    uint32_t methodIndex = (uint32_t)(found - methods.begin());
    indexType n = x->elementCount();
    if (n < 2 || v->elementCount() != n || !(v->dimensions().isVector())
        || (extrapMode == 2 && methodIndex >= 2)) {
        return false;
    }
    // the sample points must be strictly monotonic; descending points are reversed
    ArrayOf hostX = GatherGpuArray(x);
    const single* px = (const single*)hostX.dataPointer();
    bool ascending = true;
    bool descending = true;
    for (indexType k = 0; k + 1 < n; ++k) {
        ascending = ascending && px[k] < px[k + 1];
        descending = descending && px[k] > px[k + 1];
    }
    if (!ascending && !descending) {
        return false;
    }
    ArrayOf flippedX, flippedV;
    if (descending) {
        flippedX = GpuFlip(x, -1);
        flippedV = GpuFlip(v, -1);
        x = AsGpuArrayObject(flippedX);
        v = AsGpuArrayObject(flippedV);
    }
    GpuArrayObject* output = AllocateGpuArray(q->dimensions(), "single", false);
    result = ArrayOf::handle(output);
    uint32_t nq = (uint32_t)q->elementCount();
    if (nq == 0) {
        return true;
    }
    uint32_t params[5]
        = { nq, (uint32_t)n, methodIndex, (uint32_t)extrapMode, floatBits(fillValue) };
    GpuDispatchKernel(pipeline("gpuArray:interp1", INTERP1_WGSL),
        { x->buffer(), v->buffer(), q->buffer(), output->buffer() }, params, 5, (nq + 255u) / 256u,
        1);
    return true;
}
//=============================================================================
static Dimensions
reducedDims(const Dimensions& dims, int dimension)
{
    indexType ndims = std::max<indexType>(dims.length(), 2);
    std::vector<indexType> lengths;
    for (indexType k = 0; k < ndims; ++k) {
        lengths.push_back((int)k + 1 == dimension ? 1 : dims.dimensionLength(k));
    }
    Dimensions out(lengths);
    out.simplify();
    return out;
}
//=============================================================================
bool
GpuTrapz(GpuArrayObject* y, float spacing, int dim, bool cumulative, ArrayOf& result)
{
    if (!isRealFloat(y)) {
        return false;
    }
    GpuLaneGeometry lane = GpuComputeLaneGeometry(y->dimensions(), dim);
    if (lane.identity || y->elementCount() == 0) {
        Dimensions out = cumulative
            ? y->dimensions()
            : (lane.identity ? y->dimensions() : reducedDims(y->dimensions(), lane.dimension));
        result = GpuFilled(out, 0.0f, "single");
        return true;
    }
    GpuLaneSpec spec;
    spec.state = { { "acc", "f32" } };
    spec.mergeLoad = "    let v = other_acc;\n";
    spec.init = "    acc = v;\n";
    spec.update = "    acc = acc + v;\n";
    spec.outputs = { "c" };
    spec.extraParams = { "spacing" };
    std::vector<uint32_t> extra = { floatBits(spacing) };
    if (cumulative) {
        // trapezoid ending at element k (0 for the first one)
        spec.load = "    var v : f32 = 0.0;\n"
                    "    if (k > 0u) { v = 0.5 * (a[e] + a[e - params.before]); }\n";
        spec.store = "    c[e] = acc * bitcast<f32>(params.spacing);\n";
        GpuArrayObject* output = AllocateGpuArray(y->dimensions(), "single", false);
        result = ArrayOf::handle(output);
        GpuRunLaneScan("gpuArray:cumtrapz", spec, lane, y->buffer(), { output->buffer() }, extra);
        return true;
    }
    // end points weigh one half, a single point gives 0
    spec.load = "    var w : f32 = 1.0;\n"
                "    if (k == 0u || k + 1u == params.redLen) { w = 0.5; }\n"
                "    if (params.redLen == 1u) { w = 0.0; }\n"
                "    let v = a[e] * w;\n";
    spec.store = "    c[o] = acc * bitcast<f32>(params.spacing);\n";
    GpuArrayObject* output
        = AllocateGpuArray(reducedDims(y->dimensions(), lane.dimension), "single", false);
    result = ArrayOf::handle(output);
    GpuRunLaneReduce("gpuArray:trapz", spec, lane, y->buffer(), { output->buffer() }, extra);
    return true;
}
//=============================================================================
// statistic of the window [t - before, t + after] (cut at the lane ends) for every element
static const char* MOVING_WGSL = R"WGSL(
struct Params { n : u32, redLen : u32, before : u32, kb : u32, kf : u32, op : u32,
    omitNan : u32, normN : u32 };
@group(0) @binding(0) var<storage, read> x : array<f32>;
@group(0) @binding(1) var<storage, read_write> c : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
fn isNan(t : f32) -> bool { return (bitcast<u32>(t) & 0x7fffffffu) > 0x7f800000u; }
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let t = (i / params.before) % params.redLen;
    let base = i - t * params.before;
    let lo = t - min(t, params.kb);
    let hi = min(params.redLen - 1u, t + params.kf);
    var nanBits : u32 = 0x7fc00000u;
    let nanValue = bitcast<f32>(nanBits);
    var count : f32 = 0.0;
    var acc : f32 = 0.0;
    var sawNan = false;
    if (params.op == 2u) { acc = 1.0; }
    var best : f32 = nanValue;
    for (var j = lo; j <= hi; j = j + 1u) {
        let v = x[base + j * params.before];
        if (isNan(v)) { sawNan = true; continue; }
        count = count + 1.0;
        switch (params.op) {
            case 2u: { acc = acc * v; }
            case 3u: { if (isNan(best) || v > best) { best = v; } }
            case 4u: { if (isNan(best) || v < best) { best = v; } }
            default: { acc = acc + v; }
        }
    }
    if (params.op == 3u || params.op == 4u) { c[i] = best; return; }
    if (sawNan && params.omitNan == 0u) { c[i] = nanValue; return; }
    switch (params.op) {
        case 1u: { c[i] = select(acc / count, nanValue, count == 0.0); }
        case 5u, 6u: {
            if (count == 0.0) { c[i] = nanValue; return; }
            let mean = acc / count;
            var ss : f32 = 0.0;
            for (var j = lo; j <= hi; j = j + 1u) {
                let v = x[base + j * params.before];
                if (!isNan(v)) { ss = ss + (v - mean) * (v - mean); }
            }
            var variance : f32 = 0.0;
            if (count > 1.0) { variance = ss / select(count - 1.0, count, params.normN == 1u); }
            c[i] = select(variance, sqrt(variance), params.op == 6u);
        }
        default: { c[i] = acc; }
    }
}
)WGSL";
//=============================================================================
bool
GpuMoving(GpuArrayObject* x, const std::string& op, uint32_t before, uint32_t after, int dim,
    bool omitNan, bool normalizeByN, ArrayOf& result)
{
    static const std::vector<std::string> ops
        = { "sum", "mean", "prod", "max", "min", "var", "std" };
    auto found = std::find(ops.begin(), ops.end(), op);
    if (found == ops.end() || !isRealFloat(x) || before + after > 4096u) {
        return false;
    }
    GpuLaneGeometry lane = GpuComputeLaneGeometry(x->dimensions(), dim);
    uint32_t n = (uint32_t)x->elementCount();
    if (lane.identity) {
        lane.redLen = 1;
        lane.before = 1;
    }
    GpuArrayObject* output = AllocateGpuArray(x->dimensions(), "single", false);
    result = ArrayOf::handle(output);
    if (n == 0) {
        return true;
    }
    uint32_t params[8] = { n, lane.redLen, lane.before, before, after,
        (uint32_t)(found - ops.begin()), omitNan ? 1u : 0u, normalizeByN ? 1u : 0u };
    GpuDispatchKernel(pipeline("gpuArray:moving", MOVING_WGSL), { x->buffer(), output->buffer() },
        params, 8, (n + 255u) / 256u, 1);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
