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
#include <winsock2.h>
#include <Windows.h>
#undef min
#else
#include <fcntl.h>
#endif
#include <BS_thread_pool.hpp>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include <boost/filesystem.hpp>
#include "nlsConfig.h"
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include "NelsonConfiguration.hpp"
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
deleteFile(boost::filesystem::path p);
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
readFile(const boost::filesystem::path& filePath);
//=============================================================================
class systemTask
{
public:
    //=============================================================================
    void
    terminate()
    {
        _terminate = true;
        _result.first = SIGINT + 128;
        _result.second = L"ABORT";
        _running = false;
    }
    //=============================================================================
    bool
    isRunning()
    {
        return _running;
    }
    //=============================================================================
    std::pair<int, std::wstring>
    getResult()
    {
        return _result;
    };
    //=============================================================================
    void
    evaluateCommand(const std::wstring& command)
    {
        _terminate = false;
        _running = true;
        boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
        boost::filesystem::path tempOutputFile = pwd;
        boost::filesystem::path tempErrorFile = pwd;
        tempOutputFile /= boost::filesystem::unique_path();
        tempErrorFile /= boost::filesystem::unique_path();
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
        std::wstring outputResult;
        int ierr;
        if (mustDetach) {
            boost::process::child childProcess(cmd);
            childProcess.detach();
            ierr = 0;
        } else {
            boost::process::child childProcess(cmd,
                boost::process::std_out > tempOutputFile.generic_string().c_str(),
                boost::process::std_err > tempErrorFile.generic_string().c_str(),
                boost::process::std_in < boost::process::null);
            while (childProcess.running()) {
                if (_terminate) {
                    childProcess.terminate();
                    deleteFile(tempOutputFile);
                    deleteFile(tempErrorFile);
                    return;
                } else {
                    std::this_thread::sleep_for(std::chrono::milliseconds(10));
                }
            }
            if (_terminate) {
                ierr = SIGINT + 128;
            } else {
                ierr = childProcess.exit_code();
            }
            FILE* pFile = nullptr;
            if (ierr) {
                outputResult = readFile(tempErrorFile);
                if (outputResult.empty()) {
                    outputResult = readFile(tempOutputFile);
                }
            } else {
                outputResult = readFile(tempOutputFile);
            }
        }
        deleteFile(tempOutputFile);
        deleteFile(tempErrorFile);

        _result.first = ierr;
        _result.second = outputResult;
        _running = false;
    }
    //=============================================================================
private:
    //=============================================================================
    std::atomic<bool> _running = false;
    std::pair<int, std::wstring> _result = { 0, L"" };
    std::atomic<bool> _terminate = false;
    //=============================================================================
};
//=============================================================================
std::pair<int, std::wstring>
SystemCommand(const std::wstring& command, bool withEventsLoop, size_t evaluatorID)
{
    std::vector<std::pair<int, std::wstring>> results;
    wstringVector commands;
    commands.push_back(command);

    results = ParallelSystemCommand(commands, withEventsLoop, evaluatorID);
    if (results.size() != 1) {
        Error(_W("system does not return result."));
    }
    return results[0];
}
//=============================================================================
std::vector<std::pair<int, std::wstring>>
ParallelSystemCommand(const wstringVector& commands, bool withEventsLoop, size_t evaluatorID)
{
    std::vector<std::pair<int, std::wstring>> results;
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
            pool.push_task(&systemTask::evaluateCommand, task, commands[k]);
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
            taskList[k]->terminate();
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
    std::wstring res = boost::algorithm::trim_left_copy(command);
    return boost::algorithm::trim_right_copy(command);
}
//=============================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary
            = "libnlsGui" + Nelson::get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf;
        try {
            buf = new char[MAX_PATH];
        } catch (const std::bad_alloc&) {
            buf = nullptr;
        }
        if (buf != nullptr) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet != 0U) {
                fullpathGuiSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathGuiSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathGuiSharedLibrary);
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
void
deleteFile(boost::filesystem::path p)
{
    if (boost::filesystem::exists(p)) {

#ifdef _MSC_VER
        int res = _wremove(p.generic_wstring().c_str());
#else
        int res = remove(p.generic_string().c_str());
#endif
    }
}
//=============================================================================
std::wstring
readFile(const boost::filesystem::path& filePath)
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
    return utf8_to_wstring(result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
