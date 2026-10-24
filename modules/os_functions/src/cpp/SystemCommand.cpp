//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <winsock2.h>
#undef min
#else
#include <fcntl.h>
#include <csignal>
#endif
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "SystemCommandTask.hpp"
#include "nlsBuildConfig.h"
#include <ctime>
#include <thread>
#include <chrono>
#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <cstdio>
#include <memory>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
std::tuple<int, std::wstring, uint64>
SystemCommand(const std::wstring& command, uint64 timeout, bool withEventsLoop, size_t evaluatorID)
{
    const wstringVector commands = { command };
    const std::vector<uint64> timeouts = { timeout };

    auto results = ParallelSystemCommand(commands, timeouts, withEventsLoop, evaluatorID);
    if (results.size() != 1) {
        Error(L"Nelson:system:noResult", L"system does not return result.");
    }
    return results[0];
}
//=============================================================================
std::vector<std::tuple<int, std::wstring, uint64>>
ParallelSystemCommand(const wstringVector& commands, const std::vector<uint64>& timeouts,
    bool withEventsLoop, size_t evaluatorID)
{
#ifdef __EMSCRIPTEN__
    Error(L"Nelson:system:unsupportedPlatform",
        L"Native process execution is not available in WebAssembly.");
    return {};
#else
    (void)withEventsLoop;
    const size_t nbCommands = commands.size();
    std::vector<std::tuple<int, std::wstring, uint64>> results(nbCommands);
    std::vector<std::unique_ptr<SystemCommandTask>> taskList;
    taskList.reserve(nbCommands);

    if (nbCommands == 1) {
        SystemCommandTask task;
        task.evaluateCommand(commands[0], timeouts[0]);
        results[0] = task.getResult();
        return results;
    }

#if defined(__APPLE__) or (defined(_WIN32) && not defined(_WIN64))

    for (size_t k = 0; k < nbCommands; k++) {
        taskList.emplace_back(std::make_unique<SystemCommandTask>());
    }

#if not defined(__APPLE__)
#if WITH_OPENMP
#pragma omp parallel for
#endif
#endif
    for (ompIndexType k = 0; k < static_cast<ompIndexType>(nbCommands); k++) {
        taskList[k]->evaluateCommand(commands[k], timeouts[k]);
    }
#else
    std::vector<std::thread> threadList;
    threadList.reserve(nbCommands);
    std::mutex waitMutex;
    std::condition_variable waitCondition;
    std::atomic<size_t> finishedTasks { 0 };
    for (size_t k = 0; k < nbCommands; k++) {
        try {
            taskList.emplace_back(std::make_unique<SystemCommandTask>());
            threadList.emplace_back(
                [&taskList, &commands, &timeouts, &waitCondition, &finishedTasks, k]() {
                    taskList[k]->evaluateCommand(commands[k], timeouts[k]);
                    finishedTasks.fetch_add(1, std::memory_order_release);
                    waitCondition.notify_all();
                });
        } catch (const std::bad_alloc&) {
            Error(L"Nelson:outOfMemory",
                L"Memory allocation error... You may have run out of memory!");
        }
    }

    bool allTasksFinished = false;
    do {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            for (auto& task : taskList) {
                task->terminate();
            }
            for (auto& thread : threadList) {
                if (thread.joinable()) {
                    thread.join();
                }
            }
            break;
        }
        {
            std::unique_lock<std::mutex> lock(waitMutex);
            waitCondition.wait_for(
                lock, std::chrono::milliseconds(10), [&finishedTasks, nbCommands]() {
                    return finishedTasks.load(std::memory_order_acquire) == nbCommands;
                });
        }

        allTasksFinished = std::all_of(threadList.begin(), threadList.end(),
            [](const auto& thread) { return thread.joinable(); });

        allTasksFinished = allTasksFinished
            && std::all_of(taskList.begin(), taskList.end(),
                [](const auto& task) { return !task->isRunning(); });

        if (allTasksFinished) {
            for (auto& thread : threadList) {
                if (thread.joinable()) {
                    thread.join();
                }
            }
            break;
        }
    } while (!allTasksFinished);
#endif
    for (size_t k = 0; k < nbCommands; k++) {
        if (taskList[k]) {
            results[k] = taskList[k]->getResult();
        }
    }
    return results;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
