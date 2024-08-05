//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    callbacks.clear();
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
    return callbacks.empty();
}
//=============================================================================
void
CallbackQueue::add(const GraphicCallback& graphicCallback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    callbacks.push_back(graphicCallback);
}
//=============================================================================
void
CallbackQueue::clear()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    callbacks.clear();
}
//=============================================================================
bool
CallbackQueue::get(GraphicCallback& graphicCallback)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    if (!callbacks.empty()) {
        graphicCallback = callbacks.front();
        callbacks.erase(callbacks.begin());
        return true;
    }
    callbacks.clear();
    return false;
}
//=============================================================================
bool
CallbackQueue::processCallback(Evaluator* eval)
{
    if (CallbackQueue::getInstance()->isEmpty()) {
        return false;
    }

    if (eval->InCallback) {
        if (eval->IsInterruptible) {
            GraphicCallback graphicCallback;
            CallbackQueue::getInstance()->get(graphicCallback);
            graphicCallback.execute(eval);
            return true;
        }
        return false;
    }

    eval->InCallback = true;
    GraphicCallback graphicCallback;
    CallbackQueue::getInstance()->get(graphicCallback);
    eval->IsInterruptible = graphicCallback.isInterruptible();
    graphicCallback.execute(eval);
    eval->InCallback = false;
    return true;
}

//=============================================================================

} // namespace Nelson
//=============================================================================
