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
#include <cstddef>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <utility>
#include <vector>
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
class BackgroundWorkerPool
{
public:
    ~BackgroundWorkerPool() { stopAndJoin({}); }
    //=============================================================================
    void
    beginStopping()
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        m_stopping = true;
    }
    //=============================================================================
    bool
    start(std::function<void()> task)
    {
        reapFinished();
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_stopping) {
            return false;
        }
        m_workers.push_back(std::make_unique<Worker>());
        Worker* worker = m_workers.back().get();
        try {
            worker->thread = std::thread([worker, task = std::move(task)]() {
                try {
                    task();
                } catch (...) {
                }
                worker->finished.store(true);
            });
        } catch (...) {
            m_workers.pop_back();
            throw;
        }
        return true;
    }
    //=============================================================================
    void
    stopAndJoin(const std::function<void()>& poll)
    {
        beginStopping();
        for (;;) {
            bool allFinished = true;
            {
                std::lock_guard<std::mutex> lock(m_mutex);
                for (const auto& worker : m_workers) {
                    if (!worker->finished.load()) {
                        allFinished = false;
                        break;
                    }
                }
            }
            if (allFinished) {
                break;
            }
            if (poll) {
                poll();
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
        }

        std::vector<std::unique_ptr<Worker>> workers;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            workers.swap(m_workers);
        }
        join(workers);
    }
    //=============================================================================
    std::size_t
    activeCount() const
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_workers.size();
    }

private:
    struct Worker
    {
        std::atomic<bool> finished { false };
        std::thread thread;
    };
    //=============================================================================
    static void
    join(std::vector<std::unique_ptr<Worker>>& workers)
    {
        for (auto& worker : workers) {
            if (worker->thread.joinable()) {
                worker->thread.join();
            }
        }
    }
    //=============================================================================
    void
    reapFinished()
    {
        std::vector<std::unique_ptr<Worker>> finished;
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            auto it = m_workers.begin();
            while (it != m_workers.end()) {
                if ((*it)->finished.load()) {
                    finished.push_back(std::move(*it));
                    it = m_workers.erase(it);
                } else {
                    ++it;
                }
            }
        }
        join(finished);
    }
    //=============================================================================
    mutable std::mutex m_mutex;
    std::vector<std::unique_ptr<Worker>> m_workers;
    bool m_stopping = false;
};
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
