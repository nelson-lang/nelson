//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include <QtWidgets/QApplication>
#include <QtCore/QException>
#include <mutex>
#include <atomic>
#include "ProcessEvents.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define WAIT_20_MS 20
//=============================================================================
class ProcessEventsManager
{
public:
    //=============================================================================
    static ProcessEventsManager&
    getInstance()
    {
        static ProcessEventsManager instance;
        return instance;
    }
    //=============================================================================
    void
    setWaitTime(int _waitTime)
    {
        waitTime = _waitTime;
    }
    //=============================================================================
    void
    restoreWaitTime()
    {
        waitTime = WAIT_20_MS;
    }
    //=============================================================================
    void
    processEvents(bool bWaitEvents)
    {
        std::lock_guard<std::recursive_mutex> lock(mutex);

        auto now = getEpoch();
        if ((now - timerLoopEvents) > WAIT_20_MS) {
            if (bWaitEvents) {
                QCoreApplication::processEvents(QEventLoop::WaitForMoreEvents);
            } else {
                QCoreApplication::processEvents(QEventLoop::AllEvents);
            }
            timerLoopEvents = now;
        }
    }
    //=============================================================================
private:
    ProcessEventsManager() : timerLoopEvents(getEpoch()), waitTime(WAIT_20_MS) { }
    //=============================================================================
    uint64
    getEpoch()
    {
        auto now = std::chrono::system_clock::now();
        auto duration = now.time_since_epoch();
        return std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
    }

    //=============================================================================
    std::atomic<uint64> timerLoopEvents;
    std::recursive_mutex mutex;
    uint64 waitTime;
    //=============================================================================
    // Disable copy constructor and assignment operator
    ProcessEventsManager(const ProcessEventsManager&) = delete;
    ProcessEventsManager&
    operator=(const ProcessEventsManager&)
        = delete;
    //=============================================================================
};
//=============================================================================
void
ProcessEvents(bool bWaitEvents)
{
    ProcessEventsManager::getInstance().processEvents(bWaitEvents);
}
//=============================================================================
}
//=============================================================================
// namespace Nelson
//=============================================================================
void
NelSonProcessEvents(bool bWaitEvents)
{
    Nelson::ProcessEvents(bWaitEvents);
}
//=============================================================================
