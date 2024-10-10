//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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

// #include <BS_thread_pool.hpp>
#include <ctime>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cstdio>
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "SystemCommandTask.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
ProcessEventsDynamicFunction();
//=============================================================================
static void
initGuiDynamicLibrary();
//=============================================================================
std::tuple<int, std::wstring, uint64>
SystemCommand(const std::wstring& command, uint64 timeout, bool withEventsLoop, size_t evaluatorID)
{
    std::vector<std::tuple<int, std::wstring, uint64>> results;
    wstringVector commands;
    commands.push_back(command);
    std::vector<uint64> timeouts;
    timeouts.push_back(timeout);

    results = ParallelSystemCommand(commands, timeouts, withEventsLoop, evaluatorID);
    if (results.size() != 1) {
        Error(_W("system does not return result."));
    }
    return results[0];
}
//=============================================================================
std::vector<std::tuple<int, std::wstring, uint64>>
ParallelSystemCommand(const wstringVector& commands, const std::vector<uint64>& timeouts,
    bool withEventsLoop, size_t evaluatorID)
{
    std::vector<std::tuple<int, std::wstring, uint64>> results;
    size_t nbCommands = commands.size();
    results.resize(nbCommands);
    size_t nbThreadsMax = (size_t)NelsonConfiguration::getInstance()->getMaxNumCompThreads();
    size_t nbThreads = std::min(nbCommands, nbThreadsMax);

    std::vector<std::thread> threads;
    std::vector<SystemCommandTask*> taskList;

    for (size_t k = 0; k < nbCommands; k++) {
        try {
            SystemCommandTask* task = new SystemCommandTask();
            taskList.push_back(task);
            threads.emplace_back([task, commands, timeouts, k]() {
                task->evaluateCommand(commands[k], timeouts[k]);
            });
        } catch (std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    }

    while (true) {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            for (SystemCommandTask* task : taskList) {
                task->terminate();
            }
            break;
        }

        if (withEventsLoop) {
            ProcessEventsDynamicFunction();
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(1));

        bool allTasksFinished = true;
        for (SystemCommandTask* task : taskList) {
            if (task->isRunning()) {
                allTasksFinished = false;
                break;
            }
        }

        if (allTasksFinished) {
            for (auto& thread : threads) {
                thread.join();
            }
            break;
        }
    }

    for (size_t k = 0; k < nbCommands; k++) {
        if (taskList[k]) {
            results[k] = taskList[k]->getResult();
        }
    }

    for (SystemCommandTask* task : taskList) {
        if (task) {
            delete task;
            task = nullptr;
        }
    }
    taskList.clear();

    return results;
} //=============================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + Nelson::get_dynamic_library_extensionW();
        std::wstring nelsonLibrariesDirectory
            = Nelson::NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        fullpathGuiSharedLibrary
            = nelsonLibrariesDirectory + std::wstring(L"/") + fullpathGuiSharedLibrary;
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_libraryW(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
static void
ProcessEventsDynamicFunction(bool bWait)
{
    using PROC_ProcessEvents = void (*)(bool);
    static PROC_ProcessEvents ProcessEventsPtr = nullptr;
    initGuiDynamicLibrary();
    if (ProcessEventsPtr == nullptr) {
        ProcessEventsPtr = reinterpret_cast<PROC_ProcessEvents>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "NelSonProcessEvents"));
    }
    if (ProcessEventsPtr != nullptr) {
        ProcessEventsPtr(bWait);
    }
}
//=============================================================================
void
ProcessEventsDynamicFunction()
{
    ProcessEventsDynamicFunction(false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
