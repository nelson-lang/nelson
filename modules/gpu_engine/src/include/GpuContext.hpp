//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <map>
#include <string>
#include "GpuWebGpu.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Lazy-initialized WebGPU (Dawn) device context.
 *
 * The context is NOT initialized when the module loads: the first call to
 * isAvailable() (or any accessor) triggers a single initialization attempt.
 * When no compatible adapter/device is found, the context stays unavailable
 * and every query reports that without crashing or throwing.
 */
class NLSGPU_ENGINE_IMPEXP GpuContext
{
public:
    static GpuContext*
    getInstance();
    static void
    destroy();

    /** Trigger lazy initialization; true when a WebGPU device is usable. */
    bool
    isAvailable();
    /** 0 when no usable adapter, 1 otherwise (single adapter for now). */
    int
    deviceCount();

    std::string
    deviceName();
    std::string
    vendorName();
    std::string
    architectureName();
    std::string
    backendName();
    uint64_t
    maxBufferSize();

    WGPUInstance
    instance();
    WGPUDevice
    device();
    WGPUQueue
    queue();

    /** Block until the future completes (TimedWaitAny). */
    bool
    waitForFuture(WGPUFuture future);
    /** Block until all submitted GPU work completed. */
    void
    waitIdle();

    /** Compile (once) and cache a compute pipeline keyed by name. */
    WGPUComputePipeline
    pipelineFor(const std::string& key, const char* wgslSource, const char* entryPoint);

    /** WGSL validation message from the last failed pipelineFor(), if any. */
    std::string
    lastPipelineError() const;

private:
    GpuContext() = default;
    ~GpuContext();
    GpuContext(const GpuContext&) = delete;
    GpuContext&
    operator=(const GpuContext&)
        = delete;

    void
    tryInitialize();

    static GpuContext* m_instance;

    bool m_initialized = false;
    bool m_available = false;

    WGPUInstance m_wgpuInstance = nullptr;
    WGPUAdapter m_wgpuAdapter = nullptr;
    WGPUDevice m_wgpuDevice = nullptr;
    WGPUQueue m_wgpuQueue = nullptr;

    std::string m_deviceName;
    std::string m_vendorName;
    std::string m_architectureName;
    std::string m_backendName;
    uint64_t m_maxBufferSize = 0;

    std::map<std::string, WGPUComputePipeline> m_pipelineCache;
    std::string m_lastPipelineError;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
