//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CallbackQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
CallbackQueue* CallbackQueue::m_pInstance = nullptr;
//=============================================================================
CallbackQueue::CallbackQueue() { std::lock_guard<std::mutex> lock(m_mutex); }
//=============================================================================
CallbackQueue::~CallbackQueue()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.clear();
    inProgressCallbacks.clear();
}
//=============================================================================
CallbackQueue*
CallbackQueue::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new CallbackQueue();
    }
    return m_pInstance;
}
//=============================================================================
void
CallbackQueue::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
CallbackQueue::isEmpty()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return inProgressCallbacks.empty() && waitingCallbacks.empty();
}
//=============================================================================
void
CallbackQueue::add(const GraphicCallback& graphicCallback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.push_back(graphicCallback);
}
//=============================================================================
void
CallbackQueue::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    waitingCallbacks.clear();
}
//=============================================================================
bool
CallbackQueue::processCallback(Evaluator* eval)
{
    if (inProgressCallbacks.empty() && waitingCallbacks.empty()) {
        return false;
    }

    if (!inProgressCallbacks.empty()) {
        GraphicCallback& lastCallback = inProgressCallbacks.back();
        if (lastCallback.isInterruptible() && !waitingCallbacks.empty()) {

            GraphicCallback graphicCallback = waitingCallbacks.front();

            waitingCallbacks.erase(
                std::remove(waitingCallbacks.begin(), waitingCallbacks.end(), graphicCallback),
                waitingCallbacks.end());

            inProgressCallbacks.push_back(graphicCallback);

            graphicCallback.execute(eval);

            inProgressCallbacks.erase(std::remove(inProgressCallbacks.begin(),
                                          inProgressCallbacks.end(), graphicCallback),
                inProgressCallbacks.end());
            return true;
        } else if (!lastCallback.isInterruptible() && !waitingCallbacks.empty()) {
            GraphicCallback graphicCallback = waitingCallbacks.front();
            if (graphicCallback.getBusyActionState() == BUSY_ACTION::CANCEL) {
                waitingCallbacks.erase(
                    std::remove(waitingCallbacks.begin(), waitingCallbacks.end(), graphicCallback),
                    waitingCallbacks.end());
            }
            return false;
        }
    }

    if (!waitingCallbacks.empty()) {

        GraphicCallback graphicCallback = waitingCallbacks.front();
        inProgressCallbacks.push_back(graphicCallback);
        waitingCallbacks.erase(
            std::remove(waitingCallbacks.begin(), waitingCallbacks.end(), graphicCallback),
            waitingCallbacks.end());

        graphicCallback.execute(eval);
        inProgressCallbacks.erase(
            std::remove(inProgressCallbacks.begin(), inProgressCallbacks.end(), graphicCallback),
            inProgressCallbacks.end());
        return true;
    }
    return false;
}

//=============================================================================

} // namespace Nelson
//=============================================================================
