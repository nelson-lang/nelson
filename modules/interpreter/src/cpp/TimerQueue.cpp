//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TimerQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
TimerQueue* TimerQueue::m_pInstance = nullptr;
//=============================================================================
TimerQueue::TimerQueue() { std::lock_guard<std::mutex> lock(m_mutex); }
//=============================================================================
TimerQueue::~TimerQueue()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.clear();
    inProgressCallbacks.clear();
}
//=============================================================================
TimerQueue*
TimerQueue::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new TimerQueue();
    }
    return m_pInstance;
}
//=============================================================================
void
TimerQueue::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
TimerQueue::isEmpty()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return inProgressCallbacks.empty() && waitingCallbacks.empty();
}
//=============================================================================
void
TimerQueue::add(const TimerCallback& timerCallback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.push_back(timerCallback);
}
//=============================================================================
void
TimerQueue::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.clear();
}
//=============================================================================
bool
TimerQueue::processCallback(Evaluator* eval)
{
    if (inProgressCallbacks.empty() && waitingCallbacks.empty()) {
        return false;
    }
    if (!eval) {
        return false;
    }
    if (!eval->haveEventsLoop()) {
        return false;
    }

    if (!inProgressCallbacks.empty()) {
        TimerCallback& lastCallback = inProgressCallbacks.back();
        if (!waitingCallbacks.empty()) {
            TimerCallback timerCallback = waitingCallbacks.front();
            waitingCallbacks.erase(
                std::remove(waitingCallbacks.begin(), waitingCallbacks.end(), timerCallback),
                waitingCallbacks.end());
            return false;
        }
    }

    if (!waitingCallbacks.empty()) {

        TimerCallback timerCallback = waitingCallbacks.front();
        inProgressCallbacks.push_back(timerCallback);
        waitingCallbacks.erase(
            std::remove(waitingCallbacks.begin(), waitingCallbacks.end(), timerCallback),
            waitingCallbacks.end());

        timerCallback.execute(eval);
        inProgressCallbacks.erase(
            std::remove(inProgressCallbacks.begin(), inProgressCallbacks.end(), timerCallback),
            inProgressCallbacks.end());
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
