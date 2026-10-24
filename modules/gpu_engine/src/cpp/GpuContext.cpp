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
#include "GpuContext.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
toStdString(WGPUStringView view);
static std::string
backendTypeToString(WGPUBackendType backendType);
static void
onAdapterRequestEnded(WGPURequestAdapterStatus status, WGPUAdapter adapter, WGPUStringView message,
    void* userdata1, void* userdata2);
static void
onDeviceRequestEnded(WGPURequestDeviceStatus status, WGPUDevice device, WGPUStringView message,
    void* userdata1, void* userdata2);
static void
onQueueWorkDone(
    WGPUQueueWorkDoneStatus status, WGPUStringView message, void* userdata1, void* userdata2);
static void
onPipelineErrorScope(WGPUPopErrorScopeStatus status, WGPUErrorType type, WGPUStringView message,
    void* userdata1, void* userdata2);
//=============================================================================
GpuContext* GpuContext::m_instance = nullptr;
//=============================================================================
GpuContext*
GpuContext::getInstance()
{
    if (m_instance == nullptr) {
        m_instance = new GpuContext();
    }
    return m_instance;
}
//=============================================================================
void
GpuContext::destroy()
{
    if (m_instance != nullptr) {
        delete m_instance;
        m_instance = nullptr;
    }
}
//=============================================================================
GpuContext::~GpuContext()
{
    for (auto& entry : m_pipelineCache) {
        if (entry.second != nullptr) {
            wgpuComputePipelineRelease(entry.second);
        }
    }
    m_pipelineCache.clear();
    if (m_wgpuQueue != nullptr) {
        wgpuQueueRelease(m_wgpuQueue);
        m_wgpuQueue = nullptr;
    }
    if (m_wgpuDevice != nullptr) {
        wgpuDeviceRelease(m_wgpuDevice);
        m_wgpuDevice = nullptr;
    }
    if (m_wgpuAdapter != nullptr) {
        wgpuAdapterRelease(m_wgpuAdapter);
        m_wgpuAdapter = nullptr;
    }
    if (m_wgpuInstance != nullptr) {
        wgpuInstanceRelease(m_wgpuInstance);
        m_wgpuInstance = nullptr;
    }
}
//=============================================================================
bool
GpuContext::isAvailable()
{
    tryInitialize();
    return m_available;
}
//=============================================================================
int
GpuContext::deviceCount()
{
    return isAvailable() ? 1 : 0;
}
//=============================================================================
std::string
GpuContext::deviceName()
{
    tryInitialize();
    return m_deviceName;
}
//=============================================================================
std::string
GpuContext::vendorName()
{
    tryInitialize();
    return m_vendorName;
}
//=============================================================================
std::string
GpuContext::architectureName()
{
    tryInitialize();
    return m_architectureName;
}
//=============================================================================
std::string
GpuContext::backendName()
{
    tryInitialize();
    return m_backendName;
}
//=============================================================================
uint64_t
GpuContext::maxBufferSize()
{
    tryInitialize();
    return m_maxBufferSize;
}
//=============================================================================
WGPUInstance
GpuContext::instance()
{
    tryInitialize();
    return m_wgpuInstance;
}
//=============================================================================
WGPUDevice
GpuContext::device()
{
    tryInitialize();
    return m_wgpuDevice;
}
//=============================================================================
WGPUQueue
GpuContext::queue()
{
    tryInitialize();
    return m_wgpuQueue;
}
//=============================================================================
bool
GpuContext::waitForFuture(WGPUFuture future)
{
    if (m_wgpuInstance == nullptr) {
        return false;
    }
    WGPUFutureWaitInfo waitInfo = WGPU_FUTURE_WAIT_INFO_INIT;
    waitInfo.future = future;
    WGPUWaitStatus status = wgpuInstanceWaitAny(m_wgpuInstance, 1, &waitInfo, UINT64_MAX);
    return (status == WGPUWaitStatus_Success);
}
//=============================================================================
void
GpuContext::waitIdle()
{
    if (!isAvailable()) {
        return;
    }
    WGPUQueueWorkDoneCallbackInfo callbackInfo = WGPU_QUEUE_WORK_DONE_CALLBACK_INFO_INIT;
    callbackInfo.mode = WGPUCallbackMode_WaitAnyOnly;
    callbackInfo.callback = onQueueWorkDone;
    waitForFuture(wgpuQueueOnSubmittedWorkDone(m_wgpuQueue, callbackInfo));
}
//=============================================================================
WGPUComputePipeline
GpuContext::pipelineFor(const std::string& key, const char* wgslSource, const char* entryPoint)
{
    if (!isAvailable()) {
        return nullptr;
    }
    auto found = m_pipelineCache.find(key);
    if (found != m_pipelineCache.end()) {
        return found->second;
    }
    WGPUShaderSourceWGSL wgslDesc = WGPU_SHADER_SOURCE_WGSL_INIT;
    wgslDesc.code.data = wgslSource;
    wgslDesc.code.length = strlen(wgslSource);

    WGPUShaderModuleDescriptor shaderDesc = WGPU_SHADER_MODULE_DESCRIPTOR_INIT;
    shaderDesc.nextInChain = &wgslDesc.chain;
    shaderDesc.label.data = key.c_str();
    shaderDesc.label.length = key.size();

    // capture WGSL validation errors instead of failing silently
    wgpuDevicePushErrorScope(m_wgpuDevice, WGPUErrorFilter_Validation);

    WGPUShaderModule shaderModule = wgpuDeviceCreateShaderModule(m_wgpuDevice, &shaderDesc);

    WGPUComputePipeline pipeline = nullptr;
    if (shaderModule != nullptr) {
        WGPUComputePipelineDescriptor pipelineDesc = WGPU_COMPUTE_PIPELINE_DESCRIPTOR_INIT;
        pipelineDesc.label.data = key.c_str();
        pipelineDesc.label.length = key.size();
        pipelineDesc.compute.module = shaderModule;
        pipelineDesc.compute.entryPoint.data = entryPoint;
        pipelineDesc.compute.entryPoint.length = strlen(entryPoint);
        pipeline = wgpuDeviceCreateComputePipeline(m_wgpuDevice, &pipelineDesc);
        wgpuShaderModuleRelease(shaderModule);
    }

    m_lastPipelineError.clear();
    WGPUPopErrorScopeCallbackInfo popCallbackInfo = WGPU_POP_ERROR_SCOPE_CALLBACK_INFO_INIT;
    popCallbackInfo.mode = WGPUCallbackMode_WaitAnyOnly;
    popCallbackInfo.callback = onPipelineErrorScope;
    popCallbackInfo.userdata1 = &m_lastPipelineError;
    waitForFuture(wgpuDevicePopErrorScope(m_wgpuDevice, popCallbackInfo));
    if (!m_lastPipelineError.empty()) {
        // a validation error means the pipeline is unusable (dispatch no-ops)
        if (pipeline != nullptr) {
            wgpuComputePipelineRelease(pipeline);
        }
        return nullptr;
    }
    if (pipeline != nullptr) {
        m_pipelineCache[key] = pipeline;
    }
    return pipeline;
}
//=============================================================================
std::string
GpuContext::lastPipelineError() const
{
    return m_lastPipelineError;
}
//=============================================================================
void
GpuContext::tryInitialize()
{
    if (m_initialized) {
        return;
    }
    m_initialized = true;
    m_available = false;

    WGPUInstanceFeatureName requiredFeature = WGPUInstanceFeatureName_TimedWaitAny;
    WGPUInstanceDescriptor instanceDesc = WGPU_INSTANCE_DESCRIPTOR_INIT;
    instanceDesc.requiredFeatureCount = 1;
    instanceDesc.requiredFeatures = &requiredFeature;
    m_wgpuInstance = wgpuCreateInstance(&instanceDesc);
    if (m_wgpuInstance == nullptr) {
        return;
    }

    WGPURequestAdapterOptions adapterOptions = WGPU_REQUEST_ADAPTER_OPTIONS_INIT;
    adapterOptions.powerPreference = WGPUPowerPreference_HighPerformance;

    WGPURequestAdapterCallbackInfo adapterCallbackInfo = WGPU_REQUEST_ADAPTER_CALLBACK_INFO_INIT;
    adapterCallbackInfo.mode = WGPUCallbackMode_WaitAnyOnly;
    adapterCallbackInfo.callback = onAdapterRequestEnded;
    adapterCallbackInfo.userdata1 = &m_wgpuAdapter;

    waitForFuture(wgpuInstanceRequestAdapter(m_wgpuInstance, &adapterOptions, adapterCallbackInfo));
    if (m_wgpuAdapter == nullptr) {
        return;
    }

    WGPUAdapterInfo adapterInfo = WGPU_ADAPTER_INFO_INIT;
    if (wgpuAdapterGetInfo(m_wgpuAdapter, &adapterInfo) == WGPUStatus_Success) {
        m_deviceName = toStdString(adapterInfo.device);
        if (m_deviceName.empty()) {
            m_deviceName = toStdString(adapterInfo.description);
        }
        m_vendorName = toStdString(adapterInfo.vendor);
        m_architectureName = toStdString(adapterInfo.architecture);
        m_backendName = backendTypeToString(adapterInfo.backendType);
        wgpuAdapterInfoFreeMembers(adapterInfo);
    }

    WGPULimits limits = WGPU_LIMITS_INIT;
    if (wgpuAdapterGetLimits(m_wgpuAdapter, &limits) == WGPUStatus_Success) {
        m_maxBufferSize = limits.maxBufferSize;
    }

    WGPUDeviceDescriptor deviceDesc = WGPU_DEVICE_DESCRIPTOR_INIT;

    WGPURequestDeviceCallbackInfo deviceCallbackInfo = WGPU_REQUEST_DEVICE_CALLBACK_INFO_INIT;
    deviceCallbackInfo.mode = WGPUCallbackMode_WaitAnyOnly;
    deviceCallbackInfo.callback = onDeviceRequestEnded;
    deviceCallbackInfo.userdata1 = &m_wgpuDevice;

    waitForFuture(wgpuAdapterRequestDevice(m_wgpuAdapter, &deviceDesc, deviceCallbackInfo));
    if (m_wgpuDevice == nullptr) {
        return;
    }

    m_wgpuQueue = wgpuDeviceGetQueue(m_wgpuDevice);
    if (m_wgpuQueue == nullptr) {
        return;
    }
    m_available = true;
}
//=============================================================================
// static helper functions
//=============================================================================
static std::string
toStdString(WGPUStringView view)
{
    if (view.data == nullptr || view.length == 0) {
        return {};
    }
    return std::string(view.data, view.length);
}
//=============================================================================
static std::string
backendTypeToString(WGPUBackendType backendType)
{
    switch (backendType) {
    case WGPUBackendType_D3D11:
        return "D3D11";
    case WGPUBackendType_D3D12:
        return "D3D12";
    case WGPUBackendType_Metal:
        return "Metal";
    case WGPUBackendType_Vulkan:
        return "Vulkan";
    case WGPUBackendType_OpenGL:
        return "OpenGL";
    case WGPUBackendType_OpenGLES:
        return "OpenGLES";
    case WGPUBackendType_WebGPU:
        return "WebGPU";
    default:
        return "Unknown";
    }
}
//=============================================================================
static void
onAdapterRequestEnded(WGPURequestAdapterStatus status, WGPUAdapter adapter, WGPUStringView message,
    void* userdata1, void* userdata2)
{
    auto* destination = static_cast<WGPUAdapter*>(userdata1);
    if (status == WGPURequestAdapterStatus_Success) {
        *destination = adapter;
    } else {
        *destination = nullptr;
    }
}
//=============================================================================
static void
onDeviceRequestEnded(WGPURequestDeviceStatus status, WGPUDevice device, WGPUStringView message,
    void* userdata1, void* userdata2)
{
    auto* destination = static_cast<WGPUDevice*>(userdata1);
    if (status == WGPURequestDeviceStatus_Success) {
        *destination = device;
    } else {
        *destination = nullptr;
    }
}
//=============================================================================
static void
onQueueWorkDone(
    WGPUQueueWorkDoneStatus status, WGPUStringView message, void* userdata1, void* userdata2)
{
}
//=============================================================================
static void
onPipelineErrorScope(WGPUPopErrorScopeStatus status, WGPUErrorType type, WGPUStringView message,
    void* userdata1, void* userdata2)
{
    auto* destination = static_cast<std::string*>(userdata1);
    if (type != WGPUErrorType_NoError && message.data != nullptr && message.length > 0) {
        destination->assign(message.data, message.length);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
