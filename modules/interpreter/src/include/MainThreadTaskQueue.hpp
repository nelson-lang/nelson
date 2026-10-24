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
#include <atomic>
#include <chrono>
#include <deque>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include "nlsInterpreter_exports.h"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "RuntimeDispatch.hpp"
#include "SingletonHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * @brief Queue of C++ tasks posted by background threads and executed on the
 * interpreter thread.
 *
 * A background thread (e.g. a simulation worker) calls runOnInterpreterThread()
 * and blocks until the interpreter thread has run the task, or until the
 * timeout cancels it. The interpreter thread drains the queue from
 * processPendingCallbacksAndTimers(), Evaluator::processCliCallbacks() and the
 * bytecode statement-boundary hook, so tasks execute at the prompt, inside
 * pause()/timer waits, and between statements of a running script.
 *
 * Unlike commandQueue / CallbackQueue / EventQueue, entries must NEVER be
 * discarded by the prompt-entry clear() calls of the terminals: a poster is
 * blocked on its future and a dropped entry would strand it until timeout.
 */
class NLSINTERPRETER_IMPEXP MainThreadTaskQueue : public SingletonHelper<MainThreadTaskQueue>
{
    friend class SingletonHelper<MainThreadTaskQueue>;

public:
    using Task = std::function<void(Evaluator*)>;
    //=============================================================================
    static MainThreadTaskQueue*
    getInstance();
    //=============================================================================
    bool
    isEmpty()
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_entries.empty();
    }
    //=============================================================================
    /**
     * Runs every pending task on the calling thread, which must be the
     * interpreter thread owning eval. Returns true if at least one task ran.
     */
    bool
    processTasks(Evaluator* eval)
    {
        bool ranAny = false;
        for (;;) {
            std::shared_ptr<Entry> entry;
            {
                std::lock_guard<std::mutex> lock(m_mutex);
                if (m_entries.empty()) {
                    return ranAny;
                }
                entry = m_entries.front();
                m_entries.pop_front();
            }
            int expected = STATE_PENDING;
            if (!entry->state.compare_exchange_strong(expected, STATE_RUNNING)) {
                // cancelled by a poster whose timeout elapsed
                continue;
            }
            try {
                entry->fn(eval);
            } catch (...) {
                // a task must transport its own failure to the poster
            }
            entry->state.store(STATE_COMPLETED);
            entry->done.set_value();
            ranAny = true;
        }
    }
    //=============================================================================
    /**
     * Called from a background thread: enqueues fn and blocks until it has run
     * on the interpreter thread. Returns false if timeoutMs elapsed first (the
     * task is then cancelled and will never run).
     */
    bool
    runOnInterpreterThread(const Task& fn, int timeoutMs = 30000)
    {
        auto entry = std::make_shared<Entry>();
        entry->fn = fn;
        std::future<void> fut = entry->done.get_future();
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_entries.push_back(entry);
        }
        notifyPosted();
        if (fut.wait_for(std::chrono::milliseconds(timeoutMs)) == std::future_status::ready) {
            return true;
        }
        int expected = STATE_PENDING;
        if (entry->state.compare_exchange_strong(expected, STATE_CANCELLED)) {
            return false;
        }
        // The interpreter claimed the task right at the timeout boundary; wait
        // for completion so a callable capturing the poster's stack cannot
        // outlive it.
        fut.wait();
        return true;
    }
    //=============================================================================
    /**
     * Fire-and-forget variant of runOnInterpreterThread: enqueue fn and return
     * without waiting. Used by GUI-thread callers (e.g. QML signal handlers): in
     * desktop GUI mode the interpreter thread is parked inside a GUI-thread
     * blocking call (GuiTerminal getLine), so a blocking hand-off would deadlock.
     * processTasks() runs it at the next statement boundary or prompt.
     */
    void
    postToInterpreterThread(const Task& fn)
    {
        auto entry = std::make_shared<Entry>();
        entry->fn = fn;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_entries.push_back(entry);
        }
        notifyPosted();
    }
    //=============================================================================
    /**
     * Frontends whose prompt blocks on a condition variable (web_gui) register
     * a hook so a freshly posted task wakes the prompt without going through
     * Interface::interruptGetLineByEvent. Polling frontends need no hook.
     */
    void
    setWakeupHook(const std::function<void()>& hook)
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_wakeupHook = hook;
    }
    //=============================================================================
    /**
     * The interpreter thread marks itself idle while it blocks at the prompt.
     * The advanced terminal sets this around its blocking line read so that
     * background posters (nelson-adv-cli --webview figure RPCs) can run their
     * task in place instead of waking the prompt.
     */
    void
    setInterpreterIdle(bool idle)
    {
        std::lock_guard<std::mutex> lock(m_idleMutex);
        m_interpreterIdle = idle;
    }
    //=============================================================================
    /**
     * Run fn immediately on the CALLING thread if the interpreter is parked at
     * the prompt (idle), otherwise enqueue it and block until the interpreter
     * runs it. Direct execution is safe only while the interpreter is idle: the
     * m_idleMutex serialises against setInterpreterIdle(false), so a command
     * cannot start until an in-flight direct call returns. Figure interactions
     * (rotate/zoom) then stay smooth at the prompt without injecting a newline.
     *
     * In desktop GUI mode the idle prompt loop is the GUI thread, which a task
     * from a background thread may need (figure creation) while that loop is
     * blocked on setInterpreterIdle(false): such a task is queued instead and
     * drained by the prompt loop on the GUI thread
     * (canRunIdleInterpreterTaskOnCallingThread).
     */
    bool
    runNowIfIdleOrQueue(const Task& fn, int timeoutMs = 30000)
    {
        {
            std::lock_guard<std::mutex> lock(m_idleMutex);
            if (m_interpreterIdle && canRunIdleInterpreterTaskOnCallingThread()) {
                auto* eval = static_cast<Evaluator*>(
                    NelsonConfiguration::getInstance()->getMainEvaluator());
                fn(eval);
                return true;
            }
        }
        return runOnInterpreterThread(fn, timeoutMs);
    }
    //=============================================================================
private:
    MainThreadTaskQueue() = default;
    //=============================================================================
    static constexpr int STATE_PENDING = 0;
    static constexpr int STATE_RUNNING = 1;
    static constexpr int STATE_COMPLETED = 2;
    static constexpr int STATE_CANCELLED = 3;
    //=============================================================================
    struct Entry
    {
        Task fn;
        std::promise<void> done;
        std::atomic<int> state { STATE_PENDING };
    };
    //=============================================================================
    void
    notifyPosted()
    {
        // Wake a console prompt blocked on a key so it runs the task now.
        notifyInterpreterWakeup();
        std::function<void()> hook;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            hook = m_wakeupHook;
        }
        if (hook) {
            hook();
            return;
        }
        auto* eval
            = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
        if (eval != nullptr) {
            Interface* io = eval->getInterface();
            if (io != nullptr) {
                io->interruptGetLineByEvent();
            }
        }
    }
    //=============================================================================
    std::mutex m_mutex;
    std::deque<std::shared_ptr<Entry>> m_entries;
    std::function<void()> m_wakeupHook;
    std::mutex m_idleMutex;
    bool m_interpreterIdle { false };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
