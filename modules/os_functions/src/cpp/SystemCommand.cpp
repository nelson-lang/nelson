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
#include <BS_thread_pool.hpp>
#include <ctime>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
static void
ProcessEventsDynamicFunction();
//=============================================================================
static std::wstring
CleanCommand(const std::wstring& command);
//=============================================================================
static std::wstring
DetectDetachProcess(const std::wstring& command, bool& haveDetach);
//=============================================================================
static void
initGuiDynamicLibrary();
//=============================================================================
static std::wstring
readFile(const FileSystemWrapper::Path& filePath);
//=============================================================================
class systemTask
{
public:
    //=============================================================================
    void
    terminate()
    {
        if (!_terminate) {
            _terminate = true;
            _running = false;
            this->_exitCode = exitCodeAbort();
            this->_duration = this->getDuration();
        }
    }
    //=============================================================================
    bool
    isRunning()
    {
        return _running;
    }
    //=============================================================================
    std::tuple<int, std::wstring, uint64>
    getResult()
    {
        if (_terminate) {
            return std::make_tuple(_exitCode, L"ABORTED", _duration);
        }
        return std::make_tuple(_exitCode, _message, _duration);
    };
    //=============================================================================
    uint64
    getDuration()
    {
        std::chrono::steady_clock::time_point _currentTimePoint = std::chrono::steady_clock::now();
        return std::chrono::duration_cast<std::chrono::milliseconds>(
            _currentTimePoint - _beginTimePoint)
            .count();
    }
    //=============================================================================
    void
    evaluateCommand(const std::wstring& command, uint64 timeout)
    {
        _beginTimePoint = std::chrono::steady_clock::now();

        _terminate = false;
        _running = true;
        FileSystemWrapper::Path tempOutputFile(FileSystemWrapper::Path::unique_path());
        FileSystemWrapper::Path tempErrorFile(FileSystemWrapper::Path::unique_path());
        bool mustDetach = false;
        std::wstring _command = DetectDetachProcess(command, mustDetach);
        std::wstring argsShell;
#ifdef _MSC_VER
        argsShell = L" /a /c ";
#else
        argsShell = L" -c ";
#endif
        std::wstring cmd = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\""
            + _command + L"\"";
        if (mustDetach) {
            boost::process::child childProcess(cmd);
            childProcess.detach();
            _exitCode = 0;
        } else {
            boost::process::child childProcess(cmd,
                boost::process::std_out > tempOutputFile.generic_string().c_str(),
                boost::process::std_err > tempErrorFile.generic_string().c_str(),
                boost::process::std_in < boost::process::null);
            while (childProcess.running() && !_terminate) {
                std::chrono::steady_clock::time_point _currentTimePoint
                    = std::chrono::steady_clock::now();
                if ((timeout != 0)
                    && (std::chrono::duration_cast<std::chrono::seconds>(
                            _currentTimePoint - _beginTimePoint)
                        >= std::chrono::seconds(timeout))) {
                    _terminate = true;
                    break;
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            }

            if (_terminate) {
                this->_duration = this->getDuration();
                this->_exitCode = exitCodeAbort();
                FileSystemWrapper::Path::remove(tempOutputFile);
                FileSystemWrapper::Path::remove(tempErrorFile);
                childProcess.terminate();
                _running = false;
                return;
            } else {
                this->_exitCode = (int)childProcess.exit_code();
            }

            std::wstring outputResult;
            if (this->_exitCode) {
                outputResult = readFile(tempErrorFile);
                if (outputResult.empty()) {
                    outputResult = readFile(tempOutputFile);
                }
            } else {
                outputResult = readFile(tempOutputFile);
            }
            _message = outputResult;
        }
        FileSystemWrapper::Path::remove(tempOutputFile);
        FileSystemWrapper::Path::remove(tempErrorFile);

        _running = false;
        _duration = this->getDuration();
    }
    //=============================================================================
private:
    //=============================================================================
    std::atomic<bool> _running = false;
    std::atomic<bool> _terminate = false;
    int _exitCode = 0;
    std::wstring _message = std::wstring();
    uint64 _duration = uint64(0);
    std::chrono::steady_clock::time_point _beginTimePoint;
    //=============================================================================
    int
    exitCodeAbort()
    {
#ifdef _MSC_VER
        return int(258); // WAIT_TIMEOUT
#else
        return int(128 + SIGABRT);
#endif
    }
    //=============================================================================
};
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

    std::vector<systemTask*> taskList;
    BS::thread_pool pool((BS::concurrency_t)nbThreads);
    for (size_t k = 0; k < nbCommands; k++) {
        try {
            systemTask* task = new systemTask();
            taskList.push_back(task);
            pool.push_task(&systemTask::evaluateCommand, task, commands[k], timeouts[k]);
        } catch (std::bad_alloc&) {
            Error(ERROR_MEMORY_ALLOCATION);
        }
    }
    do {
        if (NelsonConfiguration::getInstance()->getInterruptPending(evaluatorID)) {
            for (size_t k = 0; k < nbCommands; k++) {
                taskList[k]->terminate();
            }
            break;
        }
        if (withEventsLoop) {
            ProcessEventsDynamicFunction();
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));
    } while (pool.get_tasks_total());

    for (size_t k = 0; k < nbCommands; k++) {
        if (taskList[k]) {
            results[k] = taskList[k]->getResult();
        }
    }
    for (systemTask* task : taskList) {
        if (task) {
            delete task;
            task = nullptr;
        }
    }
    taskList.clear();
    pool.reset((BS::concurrency_t)nbThreads);
    return results;
}
//=============================================================================
std::wstring
DetectDetachProcess(const std::wstring& command, bool& haveDetach)
{
    std::wstring _command = CleanCommand(command);
    if (_command.empty()) {
        haveDetach = true;
    } else {
        if (*_command.rbegin() == L'&') {
            _command.pop_back();
            _command = CleanCommand(_command);
            haveDetach = true;
        } else {
            haveDetach = false;
        }
    }
    if (haveDetach) {
#ifdef _MSC_VER
        _command = L"start " + _command;
#else
        _command = _command + L" &";
#endif
    }
    return _command;
}
//=============================================================================
std::wstring
CleanCommand(const std::wstring& command)
{
    return StringHelpers::trim_right_copy(command);
}
//=============================================================================
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
std::wstring
readFile(const FileSystemWrapper::Path& filePath)
{
    std::string result;
    FILE* pFile;
#ifdef _MSC_VER
    pFile = _wfopen(filePath.wstring().c_str(), L"r");
#else
    pFile = fopen(filePath.string().c_str(), "r");
#endif
    if (pFile != nullptr) {
#define bufferSize 4096
#define bufferSizeMax 4096 * 2
        char buffer[bufferSize];
        result.reserve(bufferSizeMax);
        while (fgets(buffer, sizeof(buffer), pFile)) {
#ifdef _MSC_VER
            std::string str = std::string(buffer);
            boost::replace_all(str, "\r\n", "\n");
            OemToCharBuffA(str.c_str(), const_cast<char*>(str.c_str()), (DWORD)str.size());
            result.append(str);
#else
            result.append(buffer);
#endif
        }
        if (result.size() > 0) {
            if (*result.rbegin() != '\n') {
                result.append("\n");
            }
        }
        fclose(pFile);
    }
#ifdef _MSC_VER
    std::wstring wresult;
    if (utf8_to_wstring_Windows(result, wresult)) {
        return wresult;
    }
#endif
    return utf8_to_wstring(result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
