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
#include "GpuTransfer.hpp"
#include "GpuContext.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static WGPUBuffer
createDeviceBuffer(uint64_t byteSize, WGPUBufferUsage usage, const char* label);
//=============================================================================
GpuArrayObject*
AllocateGpuArray(const Dimensions& dims, const std::string& underlyingType, bool complex)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    // complex arrays store 2 f32 (re, im) per element
    uint64_t floatSlots = (uint64_t)dims.elementCount() * (complex ? 2u : 1u);
    uint64_t byteSize = floatSlots * sizeof(float);
    if (byteSize == 0) {
        byteSize = sizeof(float);
    }
    if (byteSize > context->maxBufferSize()) {
        Error(L"Nelson:gpuArray:exceedsMaxBufferSize",
            L"Requested gpuArray exceeds the GPU maximum buffer size.");
    }
    WGPUBuffer buffer = createDeviceBuffer(byteSize,
        WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst | WGPUBufferUsage_CopySrc, "gpuArray");
    if (buffer == nullptr) {
        Error(L"Nelson:GPU:allocationFailed", L"Cannot allocate GPU buffer.");
    }
    return new GpuArrayObject(buffer, dims, underlyingType, complex);
}
//=============================================================================
GpuArrayObject*
UploadGpuArray(const ArrayOf& hostArray)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (hostArray.isSparse()) {
        Error(L"Nelson:gpuArray:sparseUnsupported", L"gpuArray: sparse inputs are not supported.");
    }
    NelsonType inputClass = hostArray.dataClass();
    bool complex = hostArray.isComplex();
    if (inputClass != NLS_SINGLE && inputClass != NLS_DOUBLE && inputClass != NLS_SCOMPLEX
        && inputClass != NLS_DCOMPLEX && inputClass != NLS_LOGICAL
        && !IS_INTEGER_TYPE(inputClass)) {
        Error(L"Nelson:gpuArray:invalidInputType", L"gpuArray: numeric or logical input expected.");
    }
    // int32/uint32 keep their exact 32-bit values: the buffer stores the raw
    // integer bits (kernels bitcast back), so no single downcast happens.
    if (inputClass == NLS_INT32 || inputClass == NLS_UINT32) {
        std::string intType = (inputClass == NLS_INT32) ? "int32" : "uint32";
        Dimensions dims = hostArray.dimensions();
        GpuArrayObject* gpuObject = AllocateGpuArray(dims, intType, false);
        size_t byteSize = (size_t)dims.elementCount() * sizeof(uint32_t);
        if (byteSize > 0) {
            wgpuQueueWriteBuffer(
                context->queue(), gpuObject->buffer(), 0, hostArray.dataPointer(), byteSize);
        }
        return gpuObject;
    }
    // WebGPU/WGSL has no f64 compute type: everything is uploaded as f32
    // (documented double -> single downcast). Complex uploads as interleaved
    // (re, im) f32 pairs (NLS_SCOMPLEX), matching the device buffer layout.
    ArrayOf promoted(hostArray);
    promoted.promoteType(complex ? NLS_SCOMPLEX : NLS_SINGLE);

    // gather() round-trips the host class through the underlyingType tag.
    // logical -> "logical"; the small integer classes keep their own tag (their
    // values fit exactly in f32). int32 and wider stay "single": f32 cannot
    // represent them exactly.
    std::string underlyingType = "single";
    switch (inputClass) {
    case NLS_LOGICAL:
        underlyingType = "logical";
        break;
    case NLS_INT8:
        underlyingType = "int8";
        break;
    case NLS_UINT8:
        underlyingType = "uint8";
        break;
    case NLS_INT16:
        underlyingType = "int16";
        break;
    case NLS_UINT16:
        underlyingType = "uint16";
        break;
    default:
        break;
    }

    Dimensions dims = promoted.dimensions();
    GpuArrayObject* gpuObject = AllocateGpuArray(dims, underlyingType, complex);
    size_t byteSize = (size_t)gpuObject->floatCount() * sizeof(float);
    if (byteSize > 0) {
        wgpuQueueWriteBuffer(
            context->queue(), gpuObject->buffer(), 0, promoted.dataPointer(), byteSize);
    }
    return gpuObject;
}
//=============================================================================
ArrayOf
CreateGpuArray(const ArrayOf& hostArray)
{
    return ArrayOf::handle(UploadGpuArray(hostArray));
}
//=============================================================================
GpuArrayObject*
CloneGpuArray(GpuArrayObject* source)
{
    GpuContext* context = GpuContext::getInstance();
    if (source == nullptr || !source->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    GpuArrayObject* output
        = AllocateGpuArray(source->dimensions(), source->underlyingType(), source->isComplex());
    indexType floatCount = source->floatCount();
    if (floatCount > 0) {
        size_t byteSize = (size_t)floatCount * sizeof(float);
        WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
        WGPUCommandEncoder encoder
            = wgpuDeviceCreateCommandEncoder(context->device(), &encoderDesc);
        wgpuCommandEncoderCopyBufferToBuffer(
            encoder, source->buffer(), 0, output->buffer(), 0, byteSize);
        WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
        WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
        wgpuQueueSubmit(context->queue(), 1, &commands);
        wgpuCommandBufferRelease(commands);
        wgpuCommandEncoderRelease(encoder);
    }
    return output;
}
//=============================================================================
struct MapRequestResult
{
    bool done = false;
    bool success = false;
};
//=============================================================================
static void
onBufferMapped(WGPUMapAsyncStatus status, WGPUStringView message, void* userdata1, void* userdata2)
{
    auto* result = static_cast<MapRequestResult*>(userdata1);
    result->done = true;
    result->success = (status == WGPUMapAsyncStatus_Success);
}
//=============================================================================
ArrayOf
GatherGpuArray(GpuArrayObject* gpuObject)
{
    GpuContext* context = GpuContext::getInstance();
    if (gpuObject == nullptr || !gpuObject->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    Dimensions dims = gpuObject->dimensions();
    indexType elementCount = dims.elementCount();
    bool isLogical = gpuObject->isLogical();
    bool isComplex = gpuObject->isComplex();

    // Small integer arrays are stored as f32 on device; gather() reads the
    // floats as single then casts back to the host integer class (host cast
    // rounds and saturates exactly like the device saturation).
    NelsonType integerClass = NLS_SINGLE;
    std::string intLo, intHi;
    bool isInteger
        = GpuIntegerUnderlyingType(gpuObject->underlyingType(), integerClass, intLo, intHi);

    // int32/uint32 store the raw 32-bit integer bits: gather() copies them back
    // byte for byte into the matching host integer class (no reinterpretation).
    GpuStorage storage = gpuObject->storageKind();
    bool isRawInt = (storage != GpuStorage::Float32);
    NelsonType rawIntClass = (storage == GpuStorage::Uint32) ? NLS_UINT32 : NLS_INT32;

    // The device buffer is f32 (interleaved re/im when complex). Rebuild the
    // matching host array: NLS_INT32/UINT32 (raw bits), NLS_LOGICAL (0/1),
    // NLS_SCOMPLEX, or NLS_SINGLE.
    single* destination = nullptr;
    logical* logicalDestination = nullptr;
    ArrayOf result;
    if (isRawInt) {
        destination = (single*)ArrayOf::allocateArrayOf(rawIntClass, elementCount);
        result = ArrayOf(rawIntClass, dims, destination);
    } else if (isLogical) {
        logicalDestination = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, elementCount);
        result = ArrayOf(NLS_LOGICAL, dims, logicalDestination);
    } else if (isComplex) {
        destination = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, elementCount);
        result = ArrayOf(NLS_SCOMPLEX, dims, destination);
    } else {
        destination = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        result = ArrayOf(NLS_SINGLE, dims, destination);
    }
    if (elementCount == 0) {
        if (isInteger) {
            result.promoteType(integerClass);
        }
        return result;
    }
    size_t byteSize = (size_t)gpuObject->floatCount() * sizeof(float);

    WGPUBuffer stagingBuffer = createDeviceBuffer(
        byteSize, WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst, "gpuArray-staging");
    if (stagingBuffer == nullptr) {
        Error(L"Nelson:GPU:stagingBufferAllocationFailed", L"Cannot allocate GPU staging buffer.");
    }

    WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(context->device(), &encoderDesc);
    wgpuCommandEncoderCopyBufferToBuffer(
        encoder, gpuObject->buffer(), 0, stagingBuffer, 0, byteSize);
    WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
    wgpuQueueSubmit(context->queue(), 1, &commands);
    wgpuCommandBufferRelease(commands);
    wgpuCommandEncoderRelease(encoder);

    MapRequestResult mapResult;
    WGPUBufferMapCallbackInfo mapCallbackInfo = WGPU_BUFFER_MAP_CALLBACK_INFO_INIT;
    mapCallbackInfo.mode = WGPUCallbackMode_WaitAnyOnly;
    mapCallbackInfo.callback = onBufferMapped;
    mapCallbackInfo.userdata1 = &mapResult;

    bool waited = context->waitForFuture(
        wgpuBufferMapAsync(stagingBuffer, WGPUMapMode_Read, 0, byteSize, mapCallbackInfo));
    if (!waited || !mapResult.success) {
        wgpuBufferRelease(stagingBuffer);
        Error(L"Nelson:GPU:readbackFailed", L"Cannot read back GPU buffer.");
    }
    const void* mapped = wgpuBufferGetConstMappedRange(stagingBuffer, 0, byteSize);
    if (mapped == nullptr) {
        wgpuBufferUnmap(stagingBuffer);
        wgpuBufferRelease(stagingBuffer);
        Error(L"Nelson:GPU:readbackFailed", L"Cannot read back GPU buffer.");
    }
    if (isLogical) {
        const float* mappedFloats = static_cast<const float*>(mapped);
        for (indexType k = 0; k < elementCount; ++k) {
            logicalDestination[k] = (mappedFloats[k] != 0.0f) ? 1 : 0;
        }
    } else {
        memcpy(destination, mapped, byteSize);
    }
    wgpuBufferUnmap(stagingBuffer);
    wgpuBufferRelease(stagingBuffer);
    if (isInteger) {
        // single -> integer host cast (rounds half away from zero and saturates)
        result.promoteType(integerClass);
    }
    return result;
}
//=============================================================================
// static helper functions
//=============================================================================
static WGPUBuffer
createDeviceBuffer(uint64_t byteSize, WGPUBufferUsage usage, const char* label)
{
    GpuContext* context = GpuContext::getInstance();
    WGPUBufferDescriptor bufferDesc = WGPU_BUFFER_DESCRIPTOR_INIT;
    bufferDesc.label.data = label;
    bufferDesc.label.length = strlen(label);
    bufferDesc.usage = usage;
    bufferDesc.size = byteSize;
    return wgpuDeviceCreateBuffer(context->device(), &bufferDesc);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
