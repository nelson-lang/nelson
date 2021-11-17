//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include <winsock2.h>
#include <Windows.h>
#else
#include <fcntl.h>
#endif
#include <string>
#include <vector>
#include <cstdio>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include <boost/thread.hpp>
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
static std::wstring
CleanCommand(const std::wstring& command);
//=============================================================================
static std::wstring
DetectDetachProcess(const std::wstring& command, bool& haveDetach);
//=============================================================================
static std::wstring
internalSystemCommand(const std::wstring& command, const boost::filesystem::path& tempOutputFile,
    const boost::filesystem::path& tempErrorFile, int& ierr, bool withEventsLoop)
{
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
    std::string result;
    if (mustDetach) {
        boost::process::child childProcess(cmd);
        childProcess.detach();
        ierr = 0;
    } else {
        cmd = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command
            + L"\"";

        boost::process::child childProcess(cmd,
            boost::process::std_out > tempOutputFile.generic_string().c_str(),
            boost::process::std_err > tempErrorFile.generic_string().c_str(),
            boost::process::std_in < boost::process::null);
        bool wasTerminated = false;
        while (
            childProcess.running() && !NelsonConfiguration::getInstance()->getInterruptPending()) {
            if (withEventsLoop) {
                ProcessEventsDynamicFunction();
            }
            if (NelsonConfiguration::getInstance()->getInterruptPending()) {
                childProcess.terminate();
                wasTerminated = true;
            }
            boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        }
        childProcess.wait();
        if (wasTerminated) {
            ierr = SIGINT + 128;
        } else {
            ierr = childProcess.exit_code();
        }
        FILE* pFile = nullptr;
        if (ierr) {
            int fsize = 0;
            try {
                fsize = (int)boost::filesystem::file_size(tempErrorFile);
            } catch (const boost::filesystem::filesystem_error& e) {
                if (e.code() == boost::system::errc::permission_denied) {
                    // ONLY FOR DEBUG
                }
                fsize = 0;
            }
            if (fsize == 0) {
#ifdef _MSC_VER
                pFile = _wfopen(tempOutputFile.wstring().c_str(), L"r");
#else
                pFile = fopen(tempOutputFile.string().c_str(), "r");
#endif
            } else {
#ifdef _MSC_VER
                pFile = _wfopen(tempErrorFile.wstring().c_str(), L"r");
#else
                pFile = fopen(tempErrorFile.string().c_str(), "r");
#endif
            }
        } else {
#ifdef _MSC_VER
            pFile = _wfopen(tempOutputFile.wstring().c_str(), L"r");
#else
            pFile = fopen(tempOutputFile.string().c_str(), "r");
#endif
        }
        if (pFile != nullptr) {
            char buffer[4096];
            result.reserve(4096 * 2);
            while (fgets(buffer, sizeof(buffer), pFile)) {
#ifdef _MSC_VER
                std::string str = std::string(buffer);
                boost::replace_all(str, "\r\n", "\n");
                // OemToCharA(str.c_str(), const_cast<char*>(str.c_str()));
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
    }
    return utf8_to_wstring(result);
}
//=============================================================================
std::wstring
SystemCommand(const std::wstring& command, int& ierr, bool withEventsLoop)
{
    boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
    boost::filesystem::path tempOutputFile = pwd;
    boost::filesystem::path tempErrorFile = pwd;
    tempOutputFile /= boost::filesystem::unique_path();
    tempErrorFile /= boost::filesystem::unique_path();
    std::wstring result
        = internalSystemCommand(command, tempOutputFile, tempErrorFile, ierr, withEventsLoop);
    deleteFile(tempOutputFile);
    deleteFile(tempErrorFile);
    return result;
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
wstringVector
ParallelSystemCommand(const wstringVector& commands, std::vector<int>& ierrs, bool withEventsLoop)
{
    ompIndexType nbCommands = (ompIndexType)commands.size();
    std::vector<boost::filesystem::path> tempOutputFiles;
    std::vector<boost::filesystem::path> tempErrorFiles;
    tempOutputFiles.reserve(nbCommands);
    tempErrorFiles.reserve(nbCommands);
    boost::filesystem::path pwd = boost::filesystem::temp_directory_path();
    for (ompIndexType k = 0; k < nbCommands; k++) {
        boost::filesystem::path tempOutputFile = pwd;
        boost::filesystem::path tempErrorFile = pwd;
        tempOutputFile /= boost::filesystem::unique_path();
        tempErrorFile /= boost::filesystem::unique_path();
        tempOutputFiles.push_back(tempOutputFile);
        tempErrorFiles.push_back(tempErrorFile);
    }
    wstringVector results;
    results.resize(nbCommands);
    ierrs.resize(nbCommands);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < nbCommands; k++) {
        int ierr;
        results[k] = internalSystemCommand(
            commands[k], tempOutputFiles[k], tempErrorFiles[k], ierr, withEventsLoop);
        ierrs[k] = ierr;
    }
    for (ompIndexType k = 0; k < nbCommands; k++) {
        deleteFile(tempOutputFiles[k]);
        deleteFile(tempErrorFiles[k]);
    }
    return results;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
