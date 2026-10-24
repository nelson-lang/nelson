//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shape / rearrangement kernels. flip/circshift/repmat/diff move data only, so
// they work for every underlying type (nComp handles the interleaved complex
// layout, raw int32 bits move unchanged) and keep the class.
//=============================================================================
#include <cstring>
#include <string>
#include <vector>
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
static ArrayOf
runGather(GpuArrayObject* a, const Dimensions& outputDims, uint32_t nComp, const std::string& opKey,
    const std::string& indexBody, const uint32_t* extraParams, uint32_t extraCount);
//=============================================================================
ArrayOf
GpuFlip(GpuArrayObject* a, int dim)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    uint32_t nComp = a->isComplex() ? 2u : 1u;
    // per output element: reverse the position along the flipped dimension
    std::string body = "        let iB = i % params.p1;\n" // before
                       "        let rest = i / params.p1;\n"
                       "        let k = rest % params.p2;\n" // redLen
                       "        let iA = rest / params.p2;\n"
                       "        let start = iB + iA * (params.p1 * params.p2);\n"
                       "        let src = start + (params.p2 - 1u - k) * params.p1;\n";
    uint32_t extra[2] = { lane.before, lane.redLen };
    return runGather(a, dims, nComp, "gpuArray:flip", body, extra, 2);
}
//=============================================================================
ArrayOf
GpuCircshift(GpuArrayObject* a, int shift, int dim)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    uint32_t nComp = a->isComplex() ? 2u : 1u;
    // positive modulo of the shift so the source position wraps around
    int r = shift % (int)lane.redLen;
    uint32_t shiftMod = (uint32_t)((r % (int)lane.redLen + (int)lane.redLen) % (int)lane.redLen);
    std::string body = "        let iB = i % params.p1;\n"
                       "        let rest = i / params.p1;\n"
                       "        let k = rest % params.p2;\n"
                       "        let iA = rest / params.p2;\n"
                       "        let start = iB + iA * (params.p1 * params.p2);\n"
                       "        let srcK = (k + params.p2 - params.p3) % params.p2;\n"
                       "        let src = start + srcK * params.p1;\n";
    uint32_t extra[3] = { lane.before, lane.redLen, shiftMod };
    return runGather(a, dims, nComp, "gpuArray:circshift", body, extra, 3);
}
//=============================================================================
ArrayOf
GpuRepmat(GpuArrayObject* a, indexType rowCopies, indexType colCopies)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!a->dimensions().is2D()) {
        Error(L"Nelson:repmat:not2D", L"repmat: only 2-D gpuArray is supported.");
    }
    uint32_t r = (uint32_t)a->dimensions().rows();
    uint32_t c = (uint32_t)a->dimensions().columns();
    Dimensions outputDims((indexType)r * rowCopies, (indexType)c * colCopies);
    uint32_t nComp = a->isComplex() ? 2u : 1u;
    // out(i,j) = a(i mod r, j mod c), column-major
    std::string body = "        let outRows = params.p1 * params.p3;\n" // r * rowCopies
                       "        let oi = i % outRows;\n"
                       "        let oj = i / outRows;\n"
                       "        let src = (oi % params.p1) + (oj % params.p2) * params.p1;\n";
    uint32_t extra[3] = { r, c, (uint32_t)rowCopies };
    return runGather(a, outputDims, nComp, "gpuArray:repmat", body, extra, 3);
}
//=============================================================================
ArrayOf
GpuDiff(GpuArrayObject* a, int order, int dim)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    if (order <= 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    GpuContext* context = GpuContext::getInstance();
    GpuArrayObject* current = a;
    ArrayOf owned; // keeps the intermediate alive
    for (int pass = 0; pass < order; ++pass) {
        Dimensions dims = current->dimensions();
        GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
        if (lane.identity || lane.redLen == 0) {
            // difference of a singleton dimension is empty
            std::vector<indexType> outVec;
            for (indexType k = 0; k < (indexType)dims.length(); ++k) {
                outVec.push_back(dims.dimensionLength(k));
            }
            if (!outVec.empty()) {
                outVec[(size_t)(lane.dimension - 1)] = 0;
            }
            return ArrayOf::handle(
                AllocateGpuArray(Dimensions(outVec), a->underlyingType(), a->isComplex()));
        }
        uint32_t nComp = current->isComplex() ? 2u : 1u;
        uint32_t newRedLen = lane.redLen - 1u;
        std::vector<indexType> outVec;
        for (indexType k = 0; k < (indexType)dims.length(); ++k) {
            outVec.push_back(dims.dimensionLength(k));
        }
        outVec[(size_t)(lane.dimension - 1)] = (indexType)newRedLen;
        Dimensions outputDims(outVec);
        outputDims.simplify();

        std::string source
            = std::string("struct Params { n : u32, before : u32, redLen : u32, nComp : u32 };\n"
                          "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                          "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                          "@group(0) @binding(2) var<uniform> params : Params;\n"
                          "@compute @workgroup_size(256)\n"
                          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                          "    let i = gid.x;\n    if (i < params.n) {\n"
                          "        let newRed = params.redLen - 1u;\n"
                          "        let iB = i % params.before;\n"
                          "        let rest = i / params.before;\n"
                          "        let k = rest % newRed;\n"
                          "        let iA = rest / newRed;\n"
                          "        let inStart = iB + iA * (params.before * params.redLen);\n"
                          "        let hi = inStart + (k + 1u) * params.before;\n"
                          "        let lo = inStart + k * params.before;\n"
                          "        for (var t : u32 = 0u; t < params.nComp; t = t + 1u) {\n"
                          "            c[i * params.nComp + t] = a[hi * params.nComp + t] - a[lo * "
                          "params.nComp + t];\n        }\n    }\n}\n");
        WGPUComputePipeline pipeline
            = context->pipelineFor("gpuArray:diff", source.c_str(), "main");
        if (pipeline == nullptr) {
            Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
        }
        GpuArrayObject* output
            = AllocateGpuArray(outputDims, a->underlyingType(), current->isComplex());
        uint32_t outCount = (uint32_t)outputDims.elementCount();
        if (outCount > 0) {
            uint32_t params[4] = { outCount, lane.before, lane.redLen, nComp };
            uint32_t workgroupCountX = (outCount + 255u) / 256u;
            GpuDispatchKernel(
                pipeline, { current->buffer(), output->buffer() }, params, 4, workgroupCountX, 1);
        }
        owned = ArrayOf::handle(output);
        current = output;
    }
    return owned;
}
//=============================================================================
ArrayOf
GpuKron(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex() || b->isComplex() || a->isRawIntStorage() || b->isRawIntStorage()) {
        Error(L"Nelson:kron:unsupportedType", L"kron is only supported for real single gpuArray.");
    }
    if (!a->dimensions().is2D() || !b->dimensions().is2D()) {
        Error(L"Nelson:kron:not2D", L"kron: only 2-D gpuArray is supported.");
    }
    GpuContext* context = GpuContext::getInstance();
    uint32_t ra = (uint32_t)a->dimensions().rows();
    uint32_t ca = (uint32_t)a->dimensions().columns();
    uint32_t rb = (uint32_t)b->dimensions().rows();
    uint32_t cb = (uint32_t)b->dimensions().columns();
    Dimensions outputDims((indexType)ra * rb, (indexType)ca * cb);
    std::string source
        = std::string("struct Params { n : u32, ra : u32, rb : u32, cb : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
                      "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(3) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n    if (i < params.n) {\n"
                      "        let outRows = params.ra * params.rb;\n"
                      "        let I = i % outRows;\n"
                      "        let J = i / outRows;\n"
                      "        let aIdx = (I / params.rb) + (J / params.cb) * params.ra;\n"
                      "        let bIdx = (I % params.rb) + (J % params.cb) * params.rb;\n"
                      "        c[i] = a[aIdx] * b[bIdx];\n    }\n}\n");
    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:kron", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(outputDims);
    uint32_t outCount = (uint32_t)outputDims.elementCount();
    if (outCount > 0) {
        uint32_t params[4] = { outCount, ra, rb, cb };
        uint32_t workgroupCountX = (outCount + 255u) / 256u;
        GpuDispatchKernel(pipeline, { a->buffer(), b->buffer(), output->buffer() }, params, 4,
            workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// triu (upper = true) / tril (upper = false): keep a(i,j) when (col - row) is on
// the requested side of the k-th diagonal, otherwise write 0. Works on the
// device for real, complex (nComp) and small-integer classes; the zero bit
// pattern (0.0f) is also integer 0, so it is correct for every storage.
ArrayOf
GpuTriangular(GpuArrayObject* a, int k, bool upper)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!a->dimensions().is2D()) {
        Error(L"Nelson:gpuArray:not2D", L"Only 2-D gpuArray is supported.");
    }
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    uint32_t rows = (uint32_t)a->dimensions().rows();
    uint32_t nComp = a->isComplex() ? 2u : 1u;
    const char* keep = upper ? "(col - kk) >= row" : "(col - kk) <= row";
    std::string source
        = std::string("struct Params { n : u32, nComp : u32, rows : u32, k : i32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let e = gid.x;\n    if (e < params.n) {\n"
                      "        let row = i32(e % params.rows);\n"
                      "        let col = i32(e / params.rows);\n"
                      "        let kk = params.k;\n"
                      "        let keep = ")
        + keep
        + ";\n"
          "        for (var t : u32 = 0u; t < params.nComp; t = t + 1u) {\n"
          "            if (keep) {\n"
          "                c[e * params.nComp + t] = a[e * params.nComp + t];\n"
          "            } else {\n"
          "                c[e * params.nComp + t] = 0.0;\n"
          "            }\n        }\n    }\n}\n";
    std::string opKey = upper ? "gpuArray:triu" : "gpuArray:tril";
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), a->underlyingType(), a->isComplex());
    uint32_t elementCount = (uint32_t)a->dimensions().elementCount();
    if (elementCount > 0) {
        uint32_t params[4];
        params[0] = elementCount;
        params[1] = nComp;
        params[2] = rows;
        std::memcpy(&params[3], &k, sizeof(int32_t)); // pass signed k through the u32 slot
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 4, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuTrace(GpuArrayObject* a)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex() || a->isRawIntStorage()) {
        Error(L"Nelson:trace:unsupportedType",
            L"trace is only supported for real single/integer gpuArray.");
    }
    if (!a->dimensions().is2D()) {
        Error(L"Nelson:trace:not2D", L"trace: only 2-D gpuArray is supported.");
    }
    GpuContext* context = GpuContext::getInstance();
    uint32_t rows = (uint32_t)a->dimensions().rows();
    uint32_t cols = (uint32_t)a->dimensions().columns();
    uint32_t diag = rows < cols ? rows : cols;
    // single thread sums the diagonal (the output is a scalar)
    std::string source
        = std::string("struct Params { rows : u32, diag : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(1)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    if (gid.x == 0u) {\n        var acc : f32 = 0.0;\n"
                      "        for (var k : u32 = 0u; k < params.diag; k = k + 1u) {\n"
                      "            acc = acc + a[k + k * params.rows];\n        }\n"
                      "        c[0] = acc;\n    }\n}\n");
    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:trace", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    // trace of an integer keeps the class (its exact value fits in f32)
    GpuArrayObject* output = AllocateGpuArray(Dimensions(1, 1), a->underlyingType());
    uint32_t params[2] = { rows, diag };
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 2, 1, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
// static helper functions
//=============================================================================
// Shared gather driver: one thread per output element runs `indexBody`, which
// must define `src` (the source element index); each component is copied.
// Params exposes n (element count), nComp, then p1/p2/p3 from extraParams.
static ArrayOf
runGather(GpuArrayObject* a, const Dimensions& outputDims, uint32_t nComp, const std::string& opKey,
    const std::string& indexBody, const uint32_t* extraParams, uint32_t extraCount)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    std::string source
        = std::string("struct Params { n : u32, nComp : u32, p1 : u32, p2 : u32, p3 : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n    if (i < params.n) {\n")
        + indexBody
        + "        for (var t : u32 = 0u; t < params.nComp; t = t + 1u) {\n"
          "            c[i * params.nComp + t] = a[src * params.nComp + t];\n        }\n    }\n}\n";
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(outputDims, a->underlyingType(), a->isComplex());
    uint32_t elementCount = (uint32_t)outputDims.elementCount();
    if (elementCount > 0) {
        uint32_t params[5] = { elementCount, nComp, 0u, 0u, 0u };
        for (uint32_t k = 0; k < extraCount && k < 3; ++k) {
            params[2 + k] = extraParams[k];
        }
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 5, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
