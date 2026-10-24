//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonWorkspaceBridge.hpp"
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    static NelsonCallFn g_call;
    static WorkspaceReadFn g_read;
    static WorkspaceWriteFn g_write;
    static std::mutex g_bridgeMutex;
    void
    setNelsonWorkspaceBridge(
        const NelsonCallFn& call, const WorkspaceReadFn& read, const WorkspaceWriteFn& write)
    {
        std::lock_guard<std::mutex> lock(g_bridgeMutex);
        g_call = call;
        g_read = read;
        g_write = write;
    }
    //=============================================================================
    bool
    hasNelsonWorkspaceBridge()
    {
        std::lock_guard<std::mutex> lock(g_bridgeMutex);
        return (bool)g_call;
    }
    //=============================================================================
    NelsonCallResult
    callNelsonFunction(
        const std::string& fcnSpec, const std::vector<double>& u, const std::vector<double>* uImag)
    {
        NelsonCallFn call;
        {
            std::lock_guard<std::mutex> lock(g_bridgeMutex);
            call = g_call;
        }
        if (call) {
            return call(fcnSpec, u, uImag);
        }
        NelsonCallResult r;
        r.errorMessage = "nelson workspace bridge is not installed";
        return r;
    }
    //=============================================================================
    WorkspaceSignal
    readWorkspaceSignal(const std::string& variableName)
    {
        WorkspaceReadFn read;
        {
            std::lock_guard<std::mutex> lock(g_bridgeMutex);
            read = g_read;
        }
        if (read) {
            return read(variableName);
        }
        WorkspaceSignal s;
        s.errorMessage = "nelson workspace bridge is not installed";
        return s;
    }
    //=============================================================================
    bool
    writeWorkspaceVariable(const WorkspacePayload& payload, std::string& errorMessage)
    {
        WorkspaceWriteFn write;
        {
            std::lock_guard<std::mutex> lock(g_bridgeMutex);
            write = g_write;
        }
        if (write) {
            return write(payload, errorMessage);
        }
        errorMessage = "nelson workspace bridge is not installed";
        return false;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
