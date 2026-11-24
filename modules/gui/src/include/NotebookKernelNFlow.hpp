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
#include <functional>
#include <string>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
// Routes nflow.* for the stdio notebook kernel by driving a headless NFlow host
// that lives in libnlsNflow_gui_builtin. The host owns all the C++ nflow state
// (dispatch core, run-state, worker); this kernel side only resolves the flat C
// surface at runtime, so gui keeps no hard link on nflow and nflow stays
// optional. The ABI is the frozen contract step3-nflow-kernel-cabi-contract.md:
//   void* nlsNFlowKernelCreate(void* eval, NFlowKernelPushFn push, void* user);
//   char* nlsNFlowKernelDispatch(void* host, const char* method, const char* params);
//   char* nlsNFlowKernelDistDir(void);
//   void  nlsNFlowKernelFreeString(char* s);
//   void  nlsNFlowKernelDestroy(void* host);
// Dispatch returns {"result":<any>} or {"error":{"message":...}}, allocated by
// the nflow module and freed with nlsNFlowKernelFreeString (never free() here).
class NotebookKernelNFlow
{
public:
    using PushFn = std::function<void(const std::string& channel, const std::string& payloadJson)>;
    using ResultWriter
        = std::function<void(const nlohmann::json& id, const nlohmann::json& result)>;
    using ErrorWriter
        = std::function<void(const nlohmann::json& id, int code, const std::string& message)>;
    //=============================================================================
    // push relays a host push as a JSON-RPC notification; writeResult/writeError
    // write a dispatch reply back as JSON-RPC, so the re-wrap stays in this file.
    // Resolves the nflow entry points; check available() before reporting support.
    NotebookKernelNFlow(PushFn push, ResultWriter writeResult, ErrorWriter writeError);
    ~NotebookKernelNFlow();
    NotebookKernelNFlow(const NotebookKernelNFlow&) = delete;
    NotebookKernelNFlow&
    operator=(const NotebookKernelNFlow&)
        = delete;
    //=============================================================================
    // True when the nflow entry points resolved (the build ships nflow).
    bool
    available() const
    {
        return m_available;
    }
    //=============================================================================
    // True for an nflow.* method this helper should take.
    bool
    handles(const std::string& method) const;
    //=============================================================================
    // Dispatch one nflow.* method and write its JSON-RPC result or error.
    void
    handle(const nlohmann::json& id, const std::string& method, const nlohmann::json& params);
    //=============================================================================
    // Adds capabilities.nflow and, when available, nflowDistDir to a
    // kernel.initialize result.
    void
    augmentInitialize(nlohmann::json& result);
    //=============================================================================
private:
    // Push thunk the host calls; user is this.
    static void
    pushThunk(void* user, const char* channel, const char* payloadJson);
    // Creates the host on first use (any thread; it does not touch the interpreter).
    void
    ensureHost();
    std::string
    distDir();
    //=============================================================================
    // Resolved flat C entry points (frozen contract).
    using PushFnC = void (*)(void*, const char*, const char*);
    using CreateFn = void* (*)(void*, PushFnC, void*);
    using DispatchFn = char* (*)(void*, const char*, const char*);
    using DistDirFn = char* (*)(void);
    using FreeFn = void (*)(char*);
    using DestroyFn = void (*)(void*);
    //=============================================================================
    PushFn m_push;
    ResultWriter m_writeResult;
    ErrorWriter m_writeError;
    bool m_available = false;
    CreateFn m_fnCreate = nullptr;
    DispatchFn m_fnDispatch = nullptr;
    DistDirFn m_fnDistDir = nullptr;
    FreeFn m_fnFree = nullptr;
    DestroyFn m_fnDestroy = nullptr;
    void* m_host = nullptr;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
