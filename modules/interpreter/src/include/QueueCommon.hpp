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
#include <algorithm>
#include <mutex>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
namespace detail {
    //=============================================================================
    template <typename CallbackType> struct QueueDefaultTraits
    {
        static bool
        execute(CallbackType& callback, Evaluator* eval)
        {
            return callback.execute(eval);
        }

        static bool
        canInterrupt(const CallbackType&, const CallbackType&)
        {
            return false;
        }

        static bool
        shouldDropWaitingWhenBusy(const CallbackType&)
        {
            return false;
        }
    };
    //=============================================================================
    /**
     * @brief A thread-safe queue implementation that supports customizable behavior for processing
     * callbacks.
     *
     * @tparam CallbackType The type of the callback objects stored in the queue.
     * @tparam Traits A traits class that defines the behavior of the queue, such as how callbacks
     * are executed, whether they can interrupt each other, and whether waiting callbacks should be
     * dropped when busy. Defaults to QueueDefaultTraits<CallbackType>.
     *
     * This class provides a base implementation for managing a queue of callbacks with thread-safe
     * operations. It supports enqueuing callbacks, processing them, and handling scenarios where
     * callbacks may interrupt or replace each other based on the provided traits.
     */
    template <typename CallbackType, typename Traits = QueueDefaultTraits<CallbackType>>
    class QueueCommon
    {
    protected:
        //=============================================================================
        QueueCommon() = default;
        ~QueueCommon() = default;
        //=============================================================================
        void
        clearPending()
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            waitingCallbacks.clear();
        }
        //=============================================================================
        void
        clearAllQueues()
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            waitingCallbacks.clear();
            inProgressCallbacks.clear();
        }
        //=============================================================================
        bool
        empty()
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            return waitingCallbacks.empty() && inProgressCallbacks.empty();
        }
        //=============================================================================
        void
        enqueue(const CallbackType& callback)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            waitingCallbacks.push_back(callback);
        }
        //=============================================================================
        void
        enqueueFront(const CallbackType& callback)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            waitingCallbacks.insert(waitingCallbacks.begin(), callback);
        }
        //=============================================================================
        bool
        tryPopFront(CallbackType& callback)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            if (waitingCallbacks.empty()) {
                return false;
            }
            callback = waitingCallbacks.front();
            waitingCallbacks.erase(waitingCallbacks.begin());
            return true;
        }
        //=============================================================================
        bool
        tryPopBack(CallbackType& callback)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            if (waitingCallbacks.empty()) {
                return false;
            }
            callback = waitingCallbacks.back();
            waitingCallbacks.pop_back();
            return true;
        }
        //=============================================================================
        bool
        process(Evaluator* eval)
        {
            std::unique_lock<std::mutex> lock(m_mutex);
            if (waitingCallbacks.empty() && inProgressCallbacks.empty()) {
                return false;
            }

            if (!inProgressCallbacks.empty()) {
                if (waitingCallbacks.empty()) {
                    return false;
                }

                CallbackType currentWaiting(waitingCallbacks.front());
                CallbackType& running = inProgressCallbacks.back();

                if (Traits::canInterrupt(running, currentWaiting)) {
                    inProgressCallbacks.push_back(currentWaiting);
                    waitingCallbacks.erase(waitingCallbacks.begin());
                    lock.unlock();
                    Traits::execute(currentWaiting, eval);

                    lock.lock();
                    inProgressCallbacks.erase(std::remove(inProgressCallbacks.begin(),
                                                  inProgressCallbacks.end(), currentWaiting),
                        inProgressCallbacks.end());
                    return true;
                }

                if (Traits::shouldDropWaitingWhenBusy(currentWaiting)) {
                    waitingCallbacks.erase(std::remove(waitingCallbacks.begin(),
                                               waitingCallbacks.end(), currentWaiting),
                        waitingCallbacks.end());
                }
                return false;
            }

            if (waitingCallbacks.empty()) {
                return false;
            }

            CallbackType current(waitingCallbacks.front());
            inProgressCallbacks.push_back(current);
            waitingCallbacks.erase(waitingCallbacks.begin());
            lock.unlock();

            Traits::execute(current, eval);

            lock.lock();
            inProgressCallbacks.erase(
                std::remove(inProgressCallbacks.begin(), inProgressCallbacks.end(), current),
                inProgressCallbacks.end());
            return true;
        }
        //=============================================================================
        std::mutex m_mutex;
        std::vector<CallbackType> waitingCallbacks;
        std::vector<CallbackType> inProgressCallbacks;
    };
    //=============================================================================
} // namespace detail
//=============================================================================
} // namespace Nelson
//=============================================================================
