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
// Frozen extern "C" ABI (form C2): an opaque NFlow headless host for the stdio
// kernel. The kernel resolves these five symbols dynamically in
// libnlsNflow_gui_builtin (nflow stays optional, no hard web_gui -> nflow link).
// All strings are nul-terminated UTF-8. The host owns the whole run-state (runId,
// partials, pause/cancel, detached worker) and drives runNFlowSimulationCore.
//=============================================================================
#include "nlsNflow_gui_builtin_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    // Push sink: invoked by the host (possibly from a worker thread) to emit
    // nflow.sim.partial / nflow.sim.final / nflow.library.loaded. channel and
    // payloadJson are valid only for the duration of the call; the kernel copies them.
    typedef void (*NFlowKernelPushFn)(void* user, const char* channel, const char* payloadJson);
    //=============================================================================
    // Creates a headless host bound to the kernel Evaluator and a push sink.
    // evalPtr is an Evaluator* passed as void*. Returns an opaque host, or nullptr on
    // failure. The host does NOT install ensureNelsonWorkspaceBridge yet: it marshals
    // that to the interpreter thread on the first run (see NFlowSimSession).
    NLSNFLOW_GUI_BUILTIN_IMPEXP void*
    nlsNFlowKernelCreate(void* evalPtr, NFlowKernelPushFn push, void* pushUser);
    //=============================================================================
    // Dispatches an nflow.* method (method, paramsJson). Returns a newly allocated
    // UTF-8 JSON string of the form {"result": <any>} on success or
    // {"error": {"message": "..."}} on error. Free it with nlsNFlowKernelFreeString
    // (the allocating module frees, since allocators differ per DLL on Windows).
    NLSNFLOW_GUI_BUILTIN_IMPEXP char*
    nlsNFlowKernelDispatch(void* host, const char* method, const char* paramsJson);
    //=============================================================================
    // Absolute path of the NFlow dist directory (getNFlowDistributionDirectory),
    // UTF-8, allocated. Free it with nlsNFlowKernelFreeString. No host needed.
    NLSNFLOW_GUI_BUILTIN_IMPEXP char*
    nlsNFlowKernelDistDir(void);
    //=============================================================================
    // Frees a string returned by nlsNFlowKernelDispatch or nlsNFlowKernelDistDir.
    NLSNFLOW_GUI_BUILTIN_IMPEXP void
    nlsNFlowKernelFreeString(char* s);
    //=============================================================================
    // Cancels any run, joins the worker and destroys the host.
    NLSNFLOW_GUI_BUILTIN_IMPEXP void
    nlsNFlowKernelDestroy(void* host);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
