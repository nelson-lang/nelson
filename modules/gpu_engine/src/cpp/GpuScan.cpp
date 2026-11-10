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
#include <string>
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
static void
checkUsable(GpuArrayObject* a)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!GpuContext::getInstance()->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
}
//=============================================================================
// scan `a` into a new array of the same size, freeing it if the dispatch fails
static ArrayOf
runScan(GpuArrayObject* a, const GpuLaneGeometry& lane, const std::string& opKey,
    const GpuLaneSpec& spec, const std::string& resultType, bool complex)
{
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), resultType, complex);
    try {
        GpuRunLaneScan(opKey, spec, lane, a->buffer(), { output->buffer() });
    } catch (...) {
        delete output;
        throw;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// stepwise saturating scan on the host, for lanes too long for one GPU thread
static ArrayOf
hostSaturatingScan(
    GpuArrayObject* a, const GpuLaneGeometry& lane, bool isProduct, double lo, double hi)
{
    ArrayOf host = GatherGpuArray(a);
    NelsonType hostClass = host.dataClass();
    host.promoteType(NLS_DOUBLE);
    double* values = (double*)host.readWriteDataPointer();
    uint64_t before = lane.before;
    uint64_t redLen = lane.redLen;
    for (uint64_t o = 0; o < lane.laneCount; ++o) {
        uint64_t start = (o % before) + (o / before) * before * redLen;
        double acc = values[start];
        for (uint64_t k = 1; k < redLen; ++k) {
            uint64_t e = start + k * before;
            acc = isProduct ? acc * values[e] : acc + values[e];
            acc = std::min(std::max(acc, lo), hi);
            values[e] = acc;
        }
    }
    host.promoteType(hostClass);
    return CreateGpuArray(host);
}
//=============================================================================
static GpuLaneSpec
realScanSpec(const std::string& combineExpr)
{
    GpuLaneSpec spec;
    spec.state = { { "acc", "f32" } };
    spec.load = "    let v = a[e];\n";
    spec.mergeLoad = "    let v = other_acc;\n";
    spec.init = "    acc = v;\n";
    spec.update = "    acc = " + combineExpr + ";\n";
    spec.store = "    c[e] = acc;\n";
    spec.outputs = { "c" };
    return spec;
}
//=============================================================================
static ArrayOf
gpuScan(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& combineExpr,
    bool saturating = false, bool isProduct = false)
{
    checkUsable(a);
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    // scanning a singleton dimension is the identity
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    GpuLaneSpec spec = realScanSpec(combineExpr);
    // a saturating integer step does not regroup, so the lane is scanned in order
    spec.associative = !saturating;
    if (saturating && GpuLaneTooLongForSerialScan(lane)) {
        NelsonType hostClass = NLS_SINGLE;
        std::string lo, hi;
        GpuIntegerUnderlyingType(a->underlyingType(), hostClass, lo, hi);
        return hostSaturatingScan(a, lane, isProduct, std::stod(lo), std::stod(hi));
    }
    return runScan(a, lane, opKey, spec, a->underlyingType(), false);
}
//=============================================================================
static ArrayOf
gpuComplexScan(GpuArrayObject* a, int dim, const std::string& opKey,
    const std::string& combineReExpr, const std::string& combineImExpr)
{
    checkUsable(a);
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    GpuLaneSpec spec;
    spec.state = { { "accRe", "f32" }, { "accIm", "f32" } };
    spec.load = "    let vRe = a[2u * e];\n    let vIm = a[2u * e + 1u];\n";
    spec.mergeLoad = "    let vRe = other_accRe;\n    let vIm = other_accIm;\n";
    spec.init = "    accRe = vRe;\n    accIm = vIm;\n";
    spec.update = "    let nr = " + combineReExpr + ";\n    let ni = " + combineImExpr
        + ";\n    accRe = nr;\n    accIm = ni;\n";
    spec.store = "    c[2u * e] = accRe;\n    c[2u * e + 1u] = accIm;\n";
    spec.outputs = { "c" };
    return runScan(a, lane, opKey, spec, "single", true);
}
//=============================================================================
static ArrayOf
gpuInt32Scan(GpuArrayObject* a, int dim, const std::string& op)
{
    checkUsable(a);
    std::string intType = a->underlyingType();
    bool isUnsigned = (intType == "uint32");
    const std::string T = isUnsigned ? "u32" : "i32";

    std::string combine;
    if (op == "sum") {
        if (isUnsigned) {
            combine = "    let s = acc + v;\n"
                      "    if (s < acc) { acc = 4294967295u; } else { acc = s; }\n";
        } else {
            combine = "    let s = acc + v;\n"
                      "    if (acc > 0 && v > 0 && s < 0) { acc = 2147483647; }\n"
                      "    else if (acc < 0 && v < 0 && s >= 0) { acc = (-2147483647 - 1); }\n"
                      "    else { acc = s; }\n";
        }
    } else if (op == "max") {
        combine = "    if (v > acc) { acc = v; }\n";
    } else if (op == "min") {
        combine = "    if (v < acc) { acc = v; }\n";
    } else { // prod
        if (isUnsigned) {
            combine = "    let p = acc * v;\n"
                      "    if (acc != 0u && p / acc != v) { acc = 4294967295u; }\n"
                      "    else { acc = p; }\n";
        } else {
            combine = "    if (acc == (-2147483647 - 1) && v == -1) { acc = 2147483647; }\n"
                      "    else if (v == (-2147483647 - 1) && acc == -1) { acc = 2147483647; }\n"
                      "    else {\n"
                      "        let p = acc * v;\n"
                      "        var ov = false;\n"
                      "        if (acc != 0) { if (p / acc != v) { ov = true; } }\n"
                      "        if (ov) {\n"
                      "            if ((acc > 0 && v > 0) || (acc < 0 && v < 0)) { acc = "
                      "2147483647; }\n"
                      "            else { acc = (-2147483647 - 1); }\n"
                      "        } else { acc = p; }\n"
                      "    }\n";
        }
    }

    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    bool saturating = (op == "sum" || op == "prod");
    if (saturating && GpuLaneTooLongForSerialScan(lane)) {
        double lo = isUnsigned ? 0.0 : -2147483648.0;
        double hi = isUnsigned ? 4294967295.0 : 2147483647.0;
        return hostSaturatingScan(a, lane, op == "prod", lo, hi);
    }
    GpuLaneSpec spec;
    spec.state = { { "acc", T } };
    spec.load = "    let v = bitcast<" + T + ">(a[e]);\n";
    spec.mergeLoad = "    let v = other_acc;\n";
    spec.init = "    acc = v;\n";
    spec.update = combine;
    spec.store = "    c[e] = bitcast<f32>(acc);\n";
    spec.outputs = { "c" };
    spec.associative = !saturating;
    return runScan(a, lane, "gpuArray:i32scan:" + op + ":" + intType, spec, intType, false);
}
//=============================================================================
ArrayOf
GpuCumsum(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexScan(a, dim, "gpuArray:ccumsum", "accRe + vRe", "accIm + vIm");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "sum");
    }
    // int8/16 cumsum saturates at each step and keeps the class (values are
    // exact in f32, so the saturating combine reproduces the host result).
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return gpuScan(
            a, dim, "gpuArray:cumsum:" + intType, GpuSaturateExpr("acc + v", intType), true);
    }
    return gpuScan(a, dim, "gpuArray:cumsum", "acc + v");
}
//=============================================================================
ArrayOf
GpuCumprod(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexScan(
            a, dim, "gpuArray:ccumprod", "accRe * vRe - accIm * vIm", "accRe * vIm + accIm * vRe");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "prod");
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return gpuScan(
            a, dim, "gpuArray:cumprod:" + intType, GpuSaturateExpr("acc * v", intType), true, true);
    }
    return gpuScan(a, dim, "gpuArray:cumprod", "acc * v");
}
//=============================================================================
ArrayOf
GpuCummax(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "max");
    }
    std::string intType = GpuIntegerResultType(a, a);
    std::string key = intType.empty() ? "gpuArray:cummax" : "gpuArray:cummax:" + intType;
    return gpuScan(a, dim, key + ":nan", GpuWgslExtreme("acc", "v", true, false));
}
//=============================================================================
ArrayOf
GpuCummin(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "min");
    }
    std::string intType = GpuIntegerResultType(a, a);
    std::string key = intType.empty() ? "gpuArray:cummin" : "gpuArray:cummin:" + intType;
    return gpuScan(a, dim, key + ":nan", GpuWgslExtreme("acc", "v", false, false));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
