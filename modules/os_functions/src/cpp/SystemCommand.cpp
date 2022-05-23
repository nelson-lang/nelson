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
#include <algorithm>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include <boost/thread.hpp>
#include <boost/filesystem.hpp>
#include <thread_pool.hpp>
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
static std::pair<int, std::wstring>
internalSystemCommand(const std::wstring& command)
{
    boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
    boost::filesystem::path tempOutputFile = pwd;
    boost::filesystem::path tempErrorFile = pwd;
    tempOutputFile /= boost::filesystem::unique_path();
    tempErrorFile /= boost::filesystem::unique_path();

    std::pair<int, std::wstring> result;
    bool mustDetach = false;
    std::wstring _command = DetectDetachProcess(command, mustDetach);
    std::wstring argsShell;
#ifdef _MSC_VER
    argsShell = L" /a /c ";
#else
    argsShell = L" -c ";
#endif
    std::wstring cmd
        = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command + L"\"";
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
        bool wasTerminated = false;
        while (
            childProcess.running() && !NelsonConfiguration::getInstance()->getInterruptPending()) {
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                childProcess.terminate();
                wasTerminated = true;
                break;
            }
            boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        }
        if (wasTerminated) {
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

    result.first = ierr;
    result.second = outputResult;
    return result;
}
//=============================================================================
std::pair<int, std::wstring>
SystemCommand(const std::wstring& command, bool withEventsLoop)
{
    std::vector<std::pair<int, std::wstring>> results;
    wstringVector commands;
    commands.push_back(command);
    results = ParallelSystemCommand(commands, withEventsLoop);
    return results[0];
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
std::vector<std::pair<int, std::wstring>>
ParallelSystemCommand(const wstringVector& commands, bool withEventsLoop)
{
    std::vector<std::pair<int, std::wstring>> results;
    int nbCommands = (int)commands.size();
    results.resize(nbCommands);
    int nbThreadsMax = NelsonConfiguration::getInstance()->getMaxNumCompThreads();
    const int nbThreads = std::min(nbCommands, nbThreadsMax);

    thread_pool pool(nbThreads);
    std::vector<std::future<std::pair<int, std::wstring>>> systemThreads(nbCommands);

    for (int k = 0; k < nbCommands; k++) {
        systemThreads[k] = pool.submit(internalSystemCommand, commands[k]);
    }
    if (withEventsLoop) {
        bool running = false;
        do {
            std::vector<bool> state;
            state.reserve(nbCommands);
            for (ompIndexType k = 0; k < nbCommands; k++) {
                state.push_back((systemThreads[k].wait_for(std::chrono::milliseconds(10))
                    != std::future_status::ready));
            }
            running = std::any_of(state.begin(), state.end(), [](bool v) { return v; });
            ProcessEventsDynamicFunction();
            boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        } while (running);
    } else {
        pool.wait_for_tasks();
    }

    for (ompIndexType k = 0; k < nbCommands; k++) {
        results[k] = systemThreads[k].get();
    }
    return results;
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
