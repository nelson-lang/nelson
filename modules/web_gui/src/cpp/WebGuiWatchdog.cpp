//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiServer.hpp"
#include <chrono>
//=============================================================================
extern "C"
{
#include "webui.h"
}
//=============================================================================
namespace Nelson {
//=============================================================================
static long long
nowMs()
{
    return std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now().time_since_epoch())
        .count();
}
//=============================================================================
void
WebGuiServer::heartbeat()
{
    m_lastBeatMs.store(nowMs());
}
//=============================================================================
void
WebGuiServer::startWatchdog()
{
    if (m_watchdogThread.joinable()) {
        return;
    }
    m_watchdogThread = std::thread([this]() {
        std::unique_lock<std::mutex> lock(m_watchdogMutex);
        bool wasShown = false;
        while (!m_shuttingDown.load()) {
            m_watchdogCv.wait_for(
                lock, std::chrono::seconds(1), [this]() { return m_shuttingDown.load(); });
            if (m_shuttingDown.load()) {
                return;
            }
            const bool shown = webui_is_shown(m_window);
            // a browser that just left gets the whole grace period, whatever its last beat date
            if (wasShown && !shown) {
                heartbeat();
            }
            wasShown = shown;
            const long long last = m_lastBeatMs.load();
            if (last >= 0 && nowMs() - last > 4000 && !shown) {
                requestExit();
                return;
            }
        }
    });
}
//=============================================================================
void
WebGuiServer::stopWatchdog()
{
    m_watchdogCv.notify_all();
    if (m_watchdogThread.joinable()) {
        m_watchdogThread.join();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
