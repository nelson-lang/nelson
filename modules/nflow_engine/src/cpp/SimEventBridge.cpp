//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEventBridge.hpp"
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    static std::function<std::vector<SimEvent>()> g_fetcher;
    static std::mutex g_fetcherMutex;
    void
    setSimEventFetcher(const std::function<std::vector<SimEvent>()>& f)
    {
        std::lock_guard<std::mutex> lock(g_fetcherMutex);
        g_fetcher = f;
    }
    //=============================================================================
    std::vector<SimEvent>
    fetchSimEvents()
    {
        std::function<std::vector<SimEvent>()> fetcher;
        {
            std::lock_guard<std::mutex> lock(g_fetcherMutex);
            fetcher = g_fetcher;
        }
        if (fetcher) {
            return fetcher();
        }
        return {};
    }
    //=============================================================================
    static SimCallbackInvoker g_callbackInvoker;
    static std::mutex g_callbackInvokerMutex;
    void
    setSimCallbackInvoker(const SimCallbackInvoker& invoker)
    {
        std::lock_guard<std::mutex> lock(g_callbackInvokerMutex);
        g_callbackInvoker = invoker;
    }
    //=============================================================================
    bool
    invokeSimCallback(const std::string& code, const std::string& blockId, std::string& err)
    {
        SimCallbackInvoker invoker;
        {
            std::lock_guard<std::mutex> lock(g_callbackInvokerMutex);
            invoker = g_callbackInvoker;
        }
        if (invoker) {
            return invoker(code, blockId, err);
        }
        return false;
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
