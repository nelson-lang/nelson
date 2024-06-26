//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <chrono>
#include <mutex>
#include <QtWidgets/QApplication>
#include "ProcessEvents.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define WAIT_20_MS std::chrono::milliseconds(20)
//=============================================================================
class EventProcessor
{
public:
    //=============================================================================
    static EventProcessor&
    getInstance()
    {
        static EventProcessor instance;
        return instance;
    }
    //=============================================================================
    void
    ProcessEvents(bool bWaitEvents)
    {
        std::lock_guard<std::mutex> lock(mutex_);
        if (doOnce) {
            doOnce = false;
            startTime = std::chrono::steady_clock::now();
        }
        auto now = std::chrono::steady_clock::now();
        if (now - startTime > WAIT_20_MS) {
            startTime = now;
            if (bWaitEvents) {
                QApplication::processEvents(QEventLoop::WaitForMoreEvents);
            } else {
                QApplication::processEvents(QEventLoop::AllEvents);
            }
        }
    }
    //=============================================================================
private:
    //=============================================================================
    EventProcessor() : doOnce(true) { }
    ~EventProcessor() { }
    //=============================================================================
    EventProcessor(EventProcessor const&) = delete;
    void
    operator=(EventProcessor const&)
        = delete;
    EventProcessor(EventProcessor&&) = delete;
    void
    operator=(EventProcessor&&)
        = delete;
    //=============================================================================
    std::chrono::steady_clock::time_point startTime;
    bool doOnce;
    std::mutex mutex_;
    //=============================================================================
};
//=============================================================================
void
ProcessEvents(bool bWaitEvents)
{
    Nelson::EventProcessor::getInstance().ProcessEvents(bWaitEvents);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
void
NelSonProcessEvents(bool bWaitEvents)
{
    Nelson::EventProcessor::getInstance().ProcessEvents(bWaitEvents);
}
//=============================================================================
