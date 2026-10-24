//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
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
        for (indexType k = 0; k < ndims; ++k) {
            if (dims.dimensionLength(k) > 1) {
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
    const uint32_t* params, size_t paramCount, uint32_t workgroupCountX, uint32_t workgroupCountY)
{
    GpuContext* context = GpuContext::getInstance();
    WGPUDevice device = context->device();

    // small uniform buffer holding the kernel parameters (16-byte aligned)
    uint64_t uniformSize = ((paramCount * sizeof(uint32_t)) + 15u) & ~(uint64_t)15u;
    WGPUBufferDescriptor uniformDesc = WGPU_BUFFER_DESCRIPTOR_INIT;
    uniformDesc.usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst;
    uniformDesc.size = uniformSize;
    WGPUBuffer uniformBuffer = wgpuDeviceCreateBuffer(device, &uniformDesc);
    if (uniformBuffer == nullptr) {
        Error(L"Nelson:GPU:allocationFailed", L"Cannot allocate GPU buffer.");
    }
    wgpuQueueWriteBuffer(context->queue(), uniformBuffer, 0, params, paramCount * sizeof(uint32_t));

    WGPUBindGroupLayout bindGroupLayout = wgpuComputePipelineGetBindGroupLayout(pipeline, 0);

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
    entries[bindingCount - 1].size = WGPU_WHOLE_SIZE;

    WGPUBindGroupDescriptor bindGroupDesc = WGPU_BIND_GROUP_DESCRIPTOR_INIT;
    bindGroupDesc.layout = bindGroupLayout;
    bindGroupDesc.entryCount = bindingCount;
    bindGroupDesc.entries = entries.data();
    WGPUBindGroup bindGroup = wgpuDeviceCreateBindGroup(device, &bindGroupDesc);

    WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(device, &encoderDesc);
    WGPUComputePassDescriptor passDesc = WGPU_COMPUTE_PASS_DESCRIPTOR_INIT;
    WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(encoder, &passDesc);
    wgpuComputePassEncoderSetPipeline(pass, pipeline);
    wgpuComputePassEncoderSetBindGroup(pass, 0, bindGroup, 0, nullptr);
    wgpuComputePassEncoderDispatchWorkgroups(pass, workgroupCountX, workgroupCountY, 1);
    wgpuComputePassEncoderEnd(pass);
    wgpuComputePassEncoderRelease(pass);

    WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
    wgpuQueueSubmit(context->queue(), 1, &commands);

    wgpuCommandBufferRelease(commands);
    wgpuCommandEncoderRelease(encoder);
    wgpuBindGroupRelease(bindGroup);
    wgpuBindGroupLayoutRelease(bindGroupLayout);
    wgpuBufferRelease(uniformBuffer);
}
//=============================================================================
std::string
GpuBuildBinaryWgsl(const std::string& expr)
{
    return std::string("struct Params { n : u32, aStride : u32, bStride : u32 };\n"
                       "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
                       "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
                       "@group(0) @binding(3) var<uniform> params : Params;\n"
                       "@compute @workgroup_size(256)\n"
                       "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                       "    let i = gid.x;\n"
                       "    if (i < params.n) {\n"
                       "        let ai = a[i * params.aStride];\n"
                       "        let bi = b[i * params.bStride];\n"
                       "        c[i] = ")
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
std::string
GpuBuildReduceWgsl(
    const std::string& initExpr, const std::string& combineExpr, const std::string& finishExpr)
{
    // One thread per output element. N-D geometry via the before/redLen split
    // in column-major layout: start = iB + iA * (before * redLen), step = before.
    return std::string("struct Params { outCount : u32, redLen : u32, before : u32 };\n"
                       "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                       "@group(0) @binding(2) var<uniform> params : Params;\n"
                       "@compute @workgroup_size(256)\n"
                       "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                       "    let o = gid.x;\n"
                       "    if (o < params.outCount) {\n"
                       "        let iB = o % params.before;\n"
                       "        let iA = o / params.before;\n"
                       "        let start = iB + iA * (params.before * params.redLen);\n"
                       "        var acc : f32 = ")
        + initExpr
        + ";\n"
          "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
          "            let v = a[start + k * params.before];\n"
          "            acc = "
        + combineExpr
        + ";\n        }\n"
          "        let redLenF = f32(params.redLen);\n"
          "        c[o] = "
        + finishExpr + ";\n    }\n}\n";
}
//=============================================================================
std::string
GpuBuildArgReduceWgsl(const std::string& compareExpr)
{
    return std::string("struct Params { outCount : u32, redLen : u32, before : u32 };\n"
                       "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read_write> vals : array<f32>;\n"
                       "@group(0) @binding(2) var<storage, read_write> idx : array<f32>;\n"
                       "@group(0) @binding(3) var<uniform> params : Params;\n"
                       "@compute @workgroup_size(256)\n"
                       "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                       "    let o = gid.x;\n"
                       "    if (o < params.outCount) {\n"
                       "        let iB = o % params.before;\n"
                       "        let iA = o / params.before;\n"
                       "        let start = iB + iA * (params.before * params.redLen);\n"
                       "        var acc : f32 = a[start];\n"
                       "        var best : u32 = 0u;\n"
                       "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
                       "            let v = a[start + k * params.before];\n"
                       "            if (")
        + compareExpr
        + ") {\n"
          "                acc = v;\n"
          "                best = k;\n"
          "            }\n        }\n"
          "        vals[o] = acc;\n"
          "        idx[o] = f32(best + 1u);\n"
          "    }\n}\n";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
