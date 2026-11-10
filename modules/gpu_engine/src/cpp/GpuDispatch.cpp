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
#include <cstring>
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GpuLaneGeometry
GpuComputeLaneGeometry(const Dimensions& dims, int dim)
{
    GpuLaneGeometry geometry;
    indexType ndims = dims.length();
    if (ndims < 2) {
        ndims = 2;
    }
    int useDim = dim;
    if (useDim <= 0) {
        useDim = (int)ndims;
        // first dimension whose length is not 1 (an empty dimension counts)
        for (indexType k = 0; k < ndims; ++k) {
            if (dims.dimensionLength(k) != 1) {
                useDim = (int)(k + 1);
                break;
            }
        }
    }
    geometry.dimension = useDim;
    if (useDim > (int)ndims) {
        geometry.identity = true;
        return geometry;
    }
    indexType d = (indexType)(useDim - 1);
    indexType before = 1;
    for (indexType k = 0; k < d; ++k) {
        before *= dims.dimensionLength(k);
    }
    indexType redLen = dims.dimensionLength(d);
    indexType after = 1;
    for (indexType k = d + 1; k < ndims; ++k) {
        after *= dims.dimensionLength(k);
    }
    geometry.before = (uint32_t)before;
    geometry.redLen = (uint32_t)redLen;
    geometry.laneCount = (uint32_t)(before * after);
    return geometry;
}
//=============================================================================
WGPUBuffer
GpuCreateIndexBuffer(const std::vector<uint32_t>& indices)
{
    GpuContext* context = GpuContext::getInstance();
    size_t byteSize = indices.size() * sizeof(uint32_t);
    if (byteSize == 0) {
        byteSize = sizeof(uint32_t);
    }
    WGPUBufferDescriptor bufferDesc = WGPU_BUFFER_DESCRIPTOR_INIT;
    bufferDesc.label.data = "gpuArray-index";
    bufferDesc.label.length = strlen("gpuArray-index");
    bufferDesc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst;
    bufferDesc.size = byteSize;
    WGPUBuffer buffer = wgpuDeviceCreateBuffer(context->device(), &bufferDesc);
    if (buffer == nullptr) {
        Error(L"Nelson:GPU:indexBufferAllocationFailed", L"Cannot allocate GPU index buffer.");
    }
    if (!indices.empty()) {
        wgpuQueueWriteBuffer(
            context->queue(), buffer, 0, indices.data(), indices.size() * sizeof(uint32_t));
    }
    return buffer;
}
//=============================================================================
void
GpuDispatchKernel(WGPUComputePipeline pipeline, const std::vector<WGPUBuffer>& storageBuffers,
    const uint32_t* params, size_t paramCount, uint32_t workgroupCountX, uint32_t workgroupCountY,
    uint32_t workgroupCountZ)
{
    GpuDispatchKernelSequence(pipeline, storageBuffers,
        { std::vector<uint32_t>(params, params + paramCount) }, workgroupCountX, workgroupCountY,
        workgroupCountZ);
}
//=============================================================================
void
GpuDispatchKernelSequence(WGPUComputePipeline pipeline,
    const std::vector<WGPUBuffer>& storageBuffers,
    const std::vector<std::vector<uint32_t>>& paramSets, uint32_t workgroupCountX,
    uint32_t workgroupCountY, uint32_t workgroupCountZ)
{
    std::vector<GpuDispatchStep> steps;
    steps.reserve(paramSets.size());
    for (const std::vector<uint32_t>& params : paramSets) {
        steps.push_back({ pipeline, params, workgroupCountX, workgroupCountY, workgroupCountZ });
    }
    GpuDispatchSteps(storageBuffers, steps);
}
//=============================================================================
void
GpuDispatchSteps(
    const std::vector<WGPUBuffer>& storageBuffers, const std::vector<GpuDispatchStep>& steps)
{
    GpuContext* context = GpuContext::getInstance();
    context->throwIfFailed();
    WGPUDevice device = context->device();
    if (steps.empty()) {
        return;
    }
    // one uniform buffer, one 256-byte aligned slot per dispatch
    uint64_t slotSize = 256u;
    for (const GpuDispatchStep& step : steps) {
        uint64_t bytes = ((step.params.size() * sizeof(uint32_t)) + 15u) & ~(uint64_t)15u;
        slotSize = std::max<uint64_t>(slotSize, (bytes + 255u) & ~(uint64_t)255u);
    }
    std::vector<uint8_t> uniformData(slotSize * steps.size(), 0);
    for (size_t s = 0; s < steps.size(); ++s) {
        memcpy(uniformData.data() + s * slotSize, steps[s].params.data(),
            steps[s].params.size() * sizeof(uint32_t));
    }
    WGPUBufferDescriptor uniformDesc = WGPU_BUFFER_DESCRIPTOR_INIT;
    uniformDesc.usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst;
    uniformDesc.size = uniformData.size();
    WGPUBuffer uniformBuffer = wgpuDeviceCreateBuffer(device, &uniformDesc);
    if (uniformBuffer == nullptr) {
        Error(L"Nelson:GPU:allocationFailed", L"Cannot allocate GPU buffer.");
    }
    wgpuQueueWriteBuffer(
        context->queue(), uniformBuffer, 0, uniformData.data(), uniformData.size());

    uint32_t limit = context->maxWorkgroupsPerDimension();
    uint32_t bindingCount = (uint32_t)storageBuffers.size() + 1;
    std::vector<WGPUBindGroupEntry> entries(bindingCount);
    for (uint32_t k = 0; k < (uint32_t)storageBuffers.size(); ++k) {
        entries[k] = WGPU_BIND_GROUP_ENTRY_INIT;
        entries[k].binding = k;
        entries[k].buffer = storageBuffers[k];
        entries[k].size = WGPU_WHOLE_SIZE;
    }
    entries[bindingCount - 1] = WGPU_BIND_GROUP_ENTRY_INIT;
    entries[bindingCount - 1].binding = bindingCount - 1;
    entries[bindingCount - 1].buffer = uniformBuffer;

    WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(device, &encoderDesc);
    WGPUComputePassDescriptor passDesc = WGPU_COMPUTE_PASS_DESCRIPTOR_INIT;
    WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(encoder, &passDesc);
    // dispatches of one pass run in order, each one seeing the previous writes
    std::vector<WGPUBindGroup> bindGroups;
    std::vector<WGPUBindGroupLayout> layouts;
    WGPUComputePipeline current = nullptr;
    WGPUBindGroupLayout layout = nullptr;
    for (size_t s = 0; s < steps.size(); ++s) {
        const GpuDispatchStep& step = steps[s];
        uint32_t x = step.workgroupCountX;
        uint32_t y = step.workgroupCountY;
        uint32_t z = step.workgroupCountZ;
        // 1-D kernels rebuild their index from X and Y (see GpuContext::pipelineFor)
        if (y == 1 && z == 1 && x > limit) {
            y = (x + limit - 1u) / limit;
            x = (x + y - 1u) / y;
        }
        if (x > limit || y > limit || z > limit) {
            wgpuComputePassEncoderEnd(pass);
            wgpuComputePassEncoderRelease(pass);
            wgpuCommandEncoderRelease(encoder);
            for (WGPUBindGroup g : bindGroups) {
                wgpuBindGroupRelease(g);
            }
            for (WGPUBindGroupLayout l : layouts) {
                wgpuBindGroupLayoutRelease(l);
            }
            wgpuBufferRelease(uniformBuffer);
            Error(L"Nelson:GPU:dispatchTooLarge", L"GPU work size exceeds the device limits.");
        }
        if (step.pipeline != current) {
            current = step.pipeline;
            layout = wgpuComputePipelineGetBindGroupLayout(current, 0);
            layouts.push_back(layout);
            wgpuComputePassEncoderSetPipeline(pass, current);
        }
        uint64_t bytes = ((step.params.size() * sizeof(uint32_t)) + 15u) & ~(uint64_t)15u;
        entries[bindingCount - 1].offset = s * slotSize;
        entries[bindingCount - 1].size = bytes;
        WGPUBindGroupDescriptor bindGroupDesc = WGPU_BIND_GROUP_DESCRIPTOR_INIT;
        bindGroupDesc.layout = layout;
        bindGroupDesc.entryCount = bindingCount;
        bindGroupDesc.entries = entries.data();
        WGPUBindGroup bindGroup = wgpuDeviceCreateBindGroup(device, &bindGroupDesc);
        bindGroups.push_back(bindGroup);
        wgpuComputePassEncoderSetBindGroup(pass, 0, bindGroup, 0, nullptr);
        wgpuComputePassEncoderDispatchWorkgroups(pass, x, y, z);
    }
    wgpuComputePassEncoderEnd(pass);
    wgpuComputePassEncoderRelease(pass);

    WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
    wgpuQueueSubmit(context->queue(), 1, &commands);

    wgpuCommandBufferRelease(commands);
    wgpuCommandEncoderRelease(encoder);
    for (WGPUBindGroup bindGroup : bindGroups) {
        wgpuBindGroupRelease(bindGroup);
    }
    for (WGPUBindGroupLayout l : layouts) {
        wgpuBindGroupLayoutRelease(l);
    }
    wgpuBufferRelease(uniformBuffer);
}
//=============================================================================
// expanded operands support this many dimensions once contiguous ones are merged
static const size_t GPU_BINARY_MAX_RANK = 8;
//=============================================================================
GpuBinaryGeometry
GpuComputeBinaryGeometry(const Dimensions& a, const Dimensions& b)
{
    GpuBinaryGeometry geometry;
    bool aScalar = a.isScalar();
    bool bScalar = b.isScalar();
    if (aScalar || bScalar || a.equals(b)) {
        geometry.outputDims = (aScalar && !bScalar) ? b : a;
        uint32_t n = (uint32_t)geometry.outputDims.elementCount();
        geometry.params = { n, (aScalar && !bScalar) ? 0u : 1u, (bScalar && !aScalar) ? 0u : 1u };
        return geometry;
    }
    // a dimension of length 1 is repeated to match the other operand
    size_t rank = std::max((size_t)a.length(), (size_t)b.length());
    std::vector<indexType> outLength(rank);
    std::vector<uint64_t> aStep(rank), bStep(rank);
    uint64_t aStride = 1;
    uint64_t bStride = 1;
    for (size_t k = 0; k < rank; ++k) {
        indexType la = (k < a.length()) ? a.dimensionLength(k) : 1;
        indexType lb = (k < b.length()) ? b.dimensionLength(k) : 1;
        if (la != lb && la != 1 && lb != 1) {
            Error(L"Nelson:gpuArray:sizeMismatch",
                L"Arrays have incompatible sizes for this operation.");
        }
        outLength[k] = (la == 1) ? lb : la;
        aStep[k] = (la == 1) ? 0 : aStride;
        bStep[k] = (lb == 1) ? 0 : bStride;
        aStride *= la;
        bStride *= lb;
    }
    geometry.outputDims = Dimensions(outLength);
    geometry.expand = true;
    // merge neighbouring dimensions that both operands walk the same way
    std::vector<uint64_t> lengths, aSteps, bSteps;
    for (size_t k = 0; k < rank; ++k) {
        if (outLength[k] == 1) {
            continue;
        }
        if (!lengths.empty()) {
            uint64_t prevLength = lengths.back();
            if (aStep[k] == aSteps.back() * prevLength && bStep[k] == bSteps.back() * prevLength) {
                lengths.back() *= outLength[k];
                continue;
            }
        }
        lengths.push_back(outLength[k]);
        aSteps.push_back(aStep[k]);
        bSteps.push_back(bStep[k]);
    }
    if (lengths.size() > GPU_BINARY_MAX_RANK) {
        Error(L"Nelson:gpuArray:expansionRankUnsupported",
            L"Implicit expansion over more than 8 dimensions is not supported for gpuArray.");
    }
    uint32_t n = (uint32_t)geometry.outputDims.elementCount();
    geometry.params.assign(4 + 3 * GPU_BINARY_MAX_RANK, 0u);
    geometry.params[0] = n;
    geometry.params[1] = (uint32_t)lengths.size();
    for (size_t k = 0; k < lengths.size(); ++k) {
        geometry.params[4 + k] = (uint32_t)lengths[k];
        geometry.params[4 + GPU_BINARY_MAX_RANK + k] = (uint32_t)aSteps[k];
        geometry.params[4 + 2 * GPU_BINARY_MAX_RANK + k] = (uint32_t)bSteps[k];
    }
    return geometry;
}
//=============================================================================
std::string
GpuBinaryParamsWgsl(bool expand)
{
    if (!expand) {
        return "struct Params { n : u32, aStride : u32, bStride : u32 };\n"
               "fn operandIndex(i : u32) -> vec2<u32> {\n"
               "    return vec2<u32>(i * params.aStride, i * params.bStride);\n"
               "}\n";
    }
    return "struct Params { n : u32, rank : u32, pad0 : u32, pad1 : u32,\n"
           "    lengths : array<vec4<u32>, 2>, aSteps : array<vec4<u32>, 2>,\n"
           "    bSteps : array<vec4<u32>, 2> };\n"
           "fn operandIndex(i : u32) -> vec2<u32> {\n"
           "    var rest = i;\n"
           "    var ia = 0u;\n"
           "    var ib = 0u;\n"
           "    for (var d = 0u; d < params.rank; d = d + 1u) {\n"
           "        let len = params.lengths[d / 4u][d % 4u];\n"
           "        let c = rest % len;\n"
           "        rest = rest / len;\n"
           "        ia = ia + c * params.aSteps[d / 4u][d % 4u];\n"
           "        ib = ib + c * params.bSteps[d / 4u][d % 4u];\n"
           "    }\n"
           "    return vec2<u32>(ia, ib);\n"
           "}\n";
}
//=============================================================================
void
GpuRunBinaryKernel(const std::string& opKey, const std::string& source,
    const GpuBinaryGeometry& geometry, const std::vector<WGPUBuffer>& buffers)
{
    uint32_t n = geometry.params[0];
    if (n == 0) {
        return;
    }
    WGPUComputePipeline pipeline = GpuContext::getInstance()->pipelineFor(
        geometry.expand ? opKey + ":expand" : opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuDispatchKernel(
        pipeline, buffers, geometry.params.data(), geometry.params.size(), (n + 255u) / 256u, 1);
}
//=============================================================================
std::string
GpuBuildBinaryWgsl(const std::string& expr, bool expand)
{
    return GpuBinaryParamsWgsl(expand)
        + "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
          "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
          "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
          "@group(0) @binding(3) var<uniform> params : Params;\n"
          "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i < params.n) {\n"
          "        let ix = operandIndex(i);\n"
          "        let ai = a[ix.x];\n"
          "        let bi = b[ix.y];\n"
          "        c[i] = "
        + expr + ";\n    }\n}\n";
}
//=============================================================================
std::string
GpuBuildUnaryWgsl(const std::string& expr)
{
    return std::string("struct Params { n : u32 };\n"
                       "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                       "@group(0) @binding(2) var<uniform> params : Params;\n"
                       "@compute @workgroup_size(256)\n"
                       "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                       "    let i = gid.x;\n"
                       "    if (i < params.n) {\n"
                       "        let ai = a[i];\n"
                       "        c[i] = ")
        + expr + ";\n    }\n}\n";
}
//=============================================================================
// a whole workgroup per lane when lanes are few or too long for one thread
static bool
useCooperativeLane(const GpuLaneGeometry& lane, bool scan)
{
    if (scan) {
        return lane.redLen >= 1024u && (lane.laneCount < 1024u || lane.redLen > 65536u);
    }
    if (lane.redLen < 256u) {
        return false;
    }
    // strided lanes: one thread per lane already reads neighbouring addresses
    if (lane.before > 1u) {
        return lane.laneCount < 512u || lane.redLen > 65536u;
    }
    return lane.laneCount < 8192u || lane.redLen > 16384u;
}
//=============================================================================
static std::string
laneHeader(const GpuLaneSpec& spec)
{
    std::string s = spec.helpers + "struct Params { outCount : u32, redLen : u32, before : u32";
    for (const std::string& name : spec.extraParams) {
        s += ", " + name + " : u32";
    }
    s += " };\n@group(0) @binding(0) var<storage, read> a : array<f32>;\n";
    size_t binding = 1;
    for (const std::string& name : spec.outputs) {
        s += "@group(0) @binding(" + std::to_string(binding++) + ") var<storage, read_write> "
            + name + " : array<f32>;\n";
    }
    s += "@group(0) @binding(" + std::to_string(binding) + ") var<uniform> params : Params;\n";
    return s;
}
//=============================================================================
static std::string
laneStart()
{
    return "    let iB = o % params.before;\n"
           "    let iA = o / params.before;\n"
           "    let start = iB + iA * (params.before * params.redLen);\n"
           "    let redLenF = f32(params.redLen);\n";
}
//=============================================================================
static std::string
laneDecls(const GpuLaneSpec& spec)
{
    std::string s;
    for (const GpuLaneVar& v : spec.state) {
        s += "    var " + v.name + " : " + v.type + ";\n";
    }
    return s;
}
//=============================================================================
static std::string
sharedArrays(const GpuLaneSpec& spec, const std::string& prefix)
{
    std::string s;
    for (const GpuLaneVar& v : spec.state) {
        s += "var<workgroup> " + prefix + v.name + " : array<" + v.type + ", 256>;\n";
    }
    return s;
}
//=============================================================================
static std::string
copyState(const GpuLaneSpec& spec, const std::string& to, const std::string& from)
{
    std::string s;
    for (const GpuLaneVar& v : spec.state) {
        s += "    " + to + v.name + from + v.name + ";\n";
    }
    return s;
}
//=============================================================================
static std::string
cooperativeEntry()
{
    return "@compute @workgroup_size(256)\n"
           "fn main(@builtin(workgroup_id) wid : vec3<u32>, @builtin(num_workgroups) nwg : "
           "vec3<u32>,\n"
           "        @builtin(local_invocation_index) t : u32) {\n"
           "    let o = wid.x + wid.y * nwg.x;\n"
           "    if (o >= params.outCount) { return; }\n";
}
//=============================================================================
static std::string
buildLaneReduceWgsl(const GpuLaneSpec& spec, bool cooperative)
{
    const std::string at = "    let e = start + k * params.before;\n";
    std::string s = laneHeader(spec);
    if (!cooperative) {
        // one thread per lane
        s += "@compute @workgroup_size(256)\n"
             "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
             "    let o = gid.x;\n"
             "    if (o >= params.outCount) { return; }\n"
            + laneStart() + laneDecls(spec) + "    {\n    let k = 0u;\n" + at + spec.load
            + spec.init + "    }\n"
            + "    for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n" + at + spec.load
            + spec.update + "    }\n" + spec.store + "}\n";
        return s;
    }
    // 256 threads stride over the lane, then a tree merge in workgroup memory
    std::string other;
    for (const GpuLaneVar& v : spec.state) {
        other += "    let other_" + v.name + " = sh_" + v.name + "[t + stride];\n";
    }
    s += sharedArrays(spec, "sh_") + cooperativeEntry() + laneStart() + laneDecls(spec)
        + "    {\n    let k = t;\n" + at + spec.load + spec.init + "    }\n"
        + "    for (var k : u32 = t + 256u; k < params.redLen; k = k + 256u) {\n" + at + spec.load
        + spec.update + "    }\n" + copyState(spec, "sh_", "[t] = ") + "    workgroupBarrier();\n"
        + "    for (var stride : u32 = 128u; stride > 0u; stride = stride >> 1u) {\n"
        + "    if (t < stride) {\n" + other + spec.mergeLoad + spec.update
        + copyState(spec, "sh_", "[t] = ") + "    }\n" + "    workgroupBarrier();\n    }\n"
        + "    if (t == 0u) {\n" + spec.store + "    }\n}\n";
    return s;
}
//=============================================================================
// first pass of a split lane: workgroup w reduces chunk w % chunks of lane w / chunks and
// stores its state, one f32 slot per state variable, in `partial`
static std::string
buildLaneChunkWgsl(const GpuLaneSpec& spec)
{
    GpuLaneSpec chunked = spec;
    chunked.outputs = { "partial" };
    chunked.extraParams.push_back("chunks");
    chunked.extraParams.push_back("chunkLen");
    const std::string at = "    let e = start + k * params.before;\n";
    std::string other;
    std::string save;
    size_t slots = spec.state.size();
    for (size_t s = 0; s < slots; ++s) {
        const GpuLaneVar& v = spec.state[s];
        other += "    let other_" + v.name + " = sh_" + v.name + "[t + stride];\n";
        save += "    partial[w * " + std::to_string(slots) + "u + " + std::to_string(s)
            + "u] = bitcast<f32>(" + v.name + ");\n";
    }
    return laneHeader(chunked) + sharedArrays(spec, "sh_")
        + "@compute @workgroup_size(256)\n"
          "fn main(@builtin(workgroup_id) wid : vec3<u32>, @builtin(num_workgroups) nwg : "
          "vec3<u32>,\n"
          "        @builtin(local_invocation_index) t : u32) {\n"
          "    let w = wid.x + wid.y * nwg.x;\n"
          "    if (w >= params.outCount) { return; }\n"
          "    let o = w / params.chunks;\n"
          "    let chunk = w % params.chunks;\n"
        + laneStart() + laneDecls(spec)
        + "    let kBegin = chunk * params.chunkLen;\n"
          "    var kEnd = kBegin + params.chunkLen;\n"
          "    if (chunk + 1u == params.chunks) { kEnd = params.redLen; }\n"
          "    {\n    let k = kBegin + t;\n"
        + at + spec.load + spec.init + "    }\n"
        + "    for (var k : u32 = kBegin + t + 256u; k < kEnd; k = k + 256u) {\n" + at + spec.load
        + spec.update + "    }\n" + copyState(spec, "sh_", "[t] = ") + "    workgroupBarrier();\n"
        + "    for (var stride : u32 = 128u; stride > 0u; stride = stride >> 1u) {\n"
        + "    if (t < stride) {\n" + other + spec.mergeLoad + spec.update
        + copyState(spec, "sh_", "[t] = ") + "    }\n" + "    workgroupBarrier();\n    }\n"
        + "    if (t == 0u) {\n" + save + "    }\n}\n";
}
//=============================================================================
// second pass: each lane merges its chunk states; the store sees the full lane length
static GpuLaneSpec
mergeChunksSpec(const GpuLaneSpec& spec)
{
    GpuLaneSpec merged = spec;
    size_t slots = spec.state.size();
    merged.load.clear();
    for (size_t s = 0; s < slots; ++s) {
        const GpuLaneVar& v = spec.state[s];
        merged.load += "    let other_" + v.name + " = bitcast<" + v.type + ">(a[e * "
            + std::to_string(slots) + "u + " + std::to_string(s) + "u]);\n";
    }
    merged.load += spec.mergeLoad;
    merged.extraParams.push_back("fullLen");
    std::string store = spec.store;
    for (const std::pair<std::string, std::string>& r :
        { std::make_pair(std::string("redLenF"), std::string("f32(params.fullLen)")),
            std::make_pair(std::string("params.redLen"), std::string("params.fullLen")) }) {
        size_t pos = 0;
        while ((pos = store.find(r.first, pos)) != std::string::npos) {
            store.replace(pos, r.first.size(), r.second);
            pos += r.second.size();
        }
    }
    merged.store = store;
    return merged;
}
//=============================================================================
// copyState writes `sh_x[t] = x`; this variant reads a state back
static std::string
loadState(const GpuLaneSpec& spec, const std::string& from)
{
    std::string s;
    for (const GpuLaneVar& v : spec.state) {
        s += "    " + v.name + " = " + from + v.name + "[t];\n";
    }
    return s;
}
//=============================================================================
static std::string
buildLaneScanWgsl(const GpuLaneSpec& spec, bool cooperative)
{
    const std::string at = "    let e = start + k * params.before;\n";
    std::string s = laneHeader(spec);
    if (!cooperative) {
        s += "@compute @workgroup_size(256)\n"
             "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
             "    let o = gid.x;\n"
             "    if (o >= params.outCount) { return; }\n"
            + laneStart() + laneDecls(spec) + "    {\n    let k = 0u;\n" + at + spec.load
            + spec.init + spec.store + "    }\n"
            + "    for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n" + at + spec.load
            + spec.update + spec.store + "    }\n}\n";
        return s;
    }
    // each thread scans one chunk; thread 0 scans the chunk totals; chunks are rescanned
    std::string other;
    for (const GpuLaneVar& v : spec.state) {
        other += "    let other_" + v.name + " = sh_" + v.name + "[i];\n";
    }
    std::string prefixStore;
    for (const GpuLaneVar& v : spec.state) {
        prefixStore += "    pre_" + v.name + "[i] = " + v.name + ";\n";
    }
    s += sharedArrays(spec, "sh_") + sharedArrays(spec, "pre_")
        + "var<workgroup> sh_laneHas : array<u32, 256>;\n"
          "var<workgroup> pre_laneHas : array<u32, 256>;\n"
        + cooperativeEntry() + laneStart() + laneDecls(spec)
        + "    let chunk = (params.redLen + 255u) / 256u;\n"
          "    let k0 = min(t * chunk, params.redLen);\n"
          "    let k1 = min(k0 + chunk, params.redLen);\n"
          "    var laneHas : u32 = 0u;\n"
          "    if (k0 < k1) {\n"
          "    {\n    let k = k0;\n"
        + at + spec.load + spec.init + "    }\n"
        + "    for (var k : u32 = k0 + 1u; k < k1; k = k + 1u) {\n" + at + spec.load + spec.update
        + "    }\n    laneHas = 1u;\n    }\n" + copyState(spec, "sh_", "[t] = ")
        + "    sh_laneHas[t] = laneHas;\n    workgroupBarrier();\n"
          "    if (t == 0u) {\n"
          "    var runHas : u32 = 0u;\n"
          "    for (var i : u32 = 0u; i < 256u; i = i + 1u) {\n"
        + prefixStore
        + "    pre_laneHas[i] = runHas;\n"
          "    if (sh_laneHas[i] != 0u) {\n"
        + other + spec.mergeLoad + "    if (runHas != 0u) {\n" + spec.update + "    } else {\n"
        + spec.init + "    }\n    runHas = 1u;\n    }\n    }\n    }\n"
        + "    workgroupBarrier();\n"
          "    if (k0 < k1) {\n"
          "    if (pre_laneHas[t] != 0u) {\n"
        + loadState(spec, "pre_") + "    for (var k : u32 = k0; k < k1; k = k + 1u) {\n" + at
        + spec.load + spec.update + spec.store + "    }\n"
        + "    } else {\n    {\n    let k = k0;\n" + at + spec.load + spec.init + spec.store
        + "    }\n" + "    for (var k : u32 = k0 + 1u; k < k1; k = k + 1u) {\n" + at + spec.load
        + spec.update + spec.store + "    }\n    }\n    }\n}\n";
    return s;
}
//=============================================================================
static void
runLaneKernel(const std::string& opKey, const std::string& source, bool cooperative,
    const GpuLaneGeometry& lane, WGPUBuffer input, const std::vector<WGPUBuffer>& outputs,
    const std::vector<uint32_t>& extraParams)
{
    GpuContext* context = GpuContext::getInstance();
    WGPUComputePipeline pipeline
        = context->pipelineFor(opKey + (cooperative ? ":wg" : ":lane"), source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    std::vector<uint32_t> params = { lane.laneCount, lane.redLen, lane.before };
    params.insert(params.end(), extraParams.begin(), extraParams.end());
    std::vector<WGPUBuffer> buffers = { input };
    buffers.insert(buffers.end(), outputs.begin(), outputs.end());
    uint32_t workgroupCount = cooperative ? lane.laneCount : (lane.laneCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, buffers, params.data(), params.size(), workgroupCount, 1);
}
//=============================================================================
void
GpuRunLaneReduce(const std::string& opKey, const GpuLaneSpec& spec, const GpuLaneGeometry& lane,
    WGPUBuffer input, const std::vector<WGPUBuffer>& outputs,
    const std::vector<uint32_t>& extraParams)
{
    // a few very long lanes: split each one over several workgroups, then merge
    if (lane.laneCount < 64u && lane.redLen >= (1u << 16)) {
        uint32_t chunks = std::max<uint32_t>(2u, 512u / std::max<uint32_t>(lane.laneCount, 1u));
        chunks = std::min<uint32_t>(chunks, lane.redLen / 4096u);
        if (chunks >= 2u) {
            uint32_t chunkLen = lane.redLen / chunks;
            uint32_t slots = (uint32_t)spec.state.size();
            GpuArrayObject* partial = AllocateGpuArray(
                Dimensions((indexType)lane.laneCount * chunks * slots, 1), "single", false);
            ArrayOf partialOwner = ArrayOf::handle(partial);
            GpuContext* context = GpuContext::getInstance();
            std::string source = buildLaneChunkWgsl(spec);
            WGPUComputePipeline pipeline
                = context->pipelineFor(opKey + ":chunks", source.c_str(), "main");
            if (pipeline == nullptr) {
                Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
            }
            std::vector<uint32_t> params = { lane.laneCount * chunks, lane.redLen, lane.before };
            params.insert(params.end(), extraParams.begin(), extraParams.end());
            params.push_back(chunks);
            params.push_back(chunkLen);
            GpuDispatchKernel(pipeline, { input, partial->buffer() }, params.data(),
                params.size(), lane.laneCount * chunks, 1);
            GpuLaneGeometry merged;
            merged.laneCount = lane.laneCount;
            merged.redLen = chunks;
            merged.before = 1;
            std::vector<uint32_t> mergedExtra = extraParams;
            mergedExtra.push_back(lane.redLen);
            bool cooperative = chunks >= 256u;
            runLaneKernel(opKey + ":merge", buildLaneReduceWgsl(mergeChunksSpec(spec), cooperative),
                cooperative, merged, partial->buffer(), outputs, mergedExtra);
            return;
        }
    }
    bool cooperative = useCooperativeLane(lane, false);
    runLaneKernel(opKey, buildLaneReduceWgsl(spec, cooperative), cooperative, lane, input, outputs,
        extraParams);
}
//=============================================================================
bool
GpuLaneTooLongForSerialScan(const GpuLaneGeometry& lane)
{
    return lane.redLen > (1u << 20);
}
//=============================================================================
void
GpuRunLaneScan(const std::string& opKey, const GpuLaneSpec& spec, const GpuLaneGeometry& lane,
    WGPUBuffer input, const std::vector<WGPUBuffer>& outputs,
    const std::vector<uint32_t>& extraParams)
{
    bool cooperative = spec.associative && useCooperativeLane(lane, true);
    runLaneKernel(opKey, buildLaneScanWgsl(spec, cooperative), cooperative, lane, input, outputs,
        extraParams);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
