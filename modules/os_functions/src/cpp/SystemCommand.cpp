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
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "SystemCommandTask.hpp"
#include "nlsBuildConfig.h"
#include <ctime>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cstdio>
#include <memory>
#include <mutex>
//===================================================================================
namespace Nelson {
//===================================================================================
namespace {
    library_handle nlsGuiHandleDynamicLibrary = nullptr;
    std::once_flag guiLibraryInitFlag;

    template <typename FuncPtr>
    FuncPtr
    getFunctionPointer(const char* functionName)
    {
        return reinterpret_cast<FuncPtr>(get_function(nlsGuiHandleDynamicLibrary, functionName));
    }
}
//===================================================================================
static void
initGuiDynamicLibrary()
{
    std::call_once(guiLibraryInitFlag, []() {
        const std::wstring fullpathGuiSharedLibrary
            = L"libnlsGui" + get_dynamic_library_extensionW();
        const std::wstring nelsonLibrariesDirectory
            = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
        const std::wstring libraryPath = nelsonLibrariesDirectory + L"/" + fullpathGuiSharedLibrary;

        nlsGuiHandleDynamicLibrary = load_dynamic_libraryW(libraryPath);
    });
}
//===================================================================================
static void
ProcessEventsDynamicFunction(bool bWait = false)
{
    using PROC_ProcessEvents = void (*)(bool);
    static PROC_ProcessEvents ProcessEventsPtr = nullptr;

    initGuiDynamicLibrary();
    if (ProcessEventsPtr == nullptr) {
        ProcessEventsPtr = getFunctionPointer<PROC_ProcessEvents>("NelSonProcessEvents");
    }
    if (ProcessEventsPtr != nullptr) {
        ProcessEventsPtr(bWait);
    }
}
//===================================================================================
std::tuple<int, std::wstring, uint64>
SystemCommand(const std::wstring& command, uint64 timeout, bool withEventsLoop, size_t evaluatorID)
{
    const wstringVector commands = { command };
    const std::vector<uint64> timeouts = { timeout };

    auto results = ParallelSystemCommand(commands, timeouts, withEventsLoop, evaluatorID);
    if (results.size() != 1) {
        Error(_W("system does not return result."));
    }
    return results[0];
}
//===================================================================================
std::vector<std::tuple<int, std::wstring, uint64>>
ParallelSystemCommand(const wstringVector& commands, const std::vector<uint64>& timeouts,
    bool withEventsLoop, size_t evaluatorID)
{
    const size_t nbCommands = commands.size();
    std::vector<std::tuple<int, std::wstring, uint64>> results(nbCommands);
    std::vector<std::unique_ptr<SystemCommandTask>> taskList;
    taskList.reserve(nbCommands);

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
    for (size_t k = 0; k < nbCommands; k++) {
        try {
            taskList.emplace_back(std::make_unique<SystemCommandTask>());
            threadList.emplace_back([&taskList, &commands, &timeouts, k]() {
                taskList[k]->evaluateCommand(commands[k], timeouts[k]);
            });
        } catch (const std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
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
        if (withEventsLoop) {
            ProcessEventsDynamicFunction();
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(1));

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
}
//===================================================================================
} // namespace Nelson
//===================================================================================
