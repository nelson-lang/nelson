//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#include <winsock2.h>
#include <Windows.h>
#else
#include <fcntl.h>
#endif
#include <string>
#include <vector>
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include <boost/thread.hpp>
#include "SystemCommand.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
//=============================================================================
static Nelson::library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
//=============================================================================
namespace Nelson {
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
ArrayOf
SystemCommand(const std::wstring& command, int& ierr, bool withEventsLoop)
{
    bool mustDetach = false;
    std::wstring _command = DetectDetachProcess(command, mustDetach);

    std::string output;
    std::string error;
    std::wstring cmd;

    std::wstring argsShell;
#ifdef _MSC_VER
    argsShell = L" /a /c ";
#else
    argsShell = L" -c ";
#endif

    cmd = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command + L"\"";

    if (mustDetach) {
        boost::process::child childProcess(cmd);
        childProcess.detach();
        ierr = 0;
    } else {
        boost::asio::io_service ios;
        std::vector<char> vOut(128 << 10);
        auto outBuffer{ boost::asio::buffer(vOut) };
        boost::process::async_pipe pipeOut(ios);

        std::function<void(const boost::system::error_code& ec, std::size_t n)> onStdOut;
        onStdOut = [&](const boost::system::error_code& ec, size_t n) {
            if (withEventsLoop) {
                ProcessEventsDynamicFunction();
            }
            output.reserve(output.size() + n);
            output.insert(output.end(), vOut.begin(), vOut.begin() + n);
            output.erase(std::remove(output.begin(), output.end(), '\0'), output.end());
#ifdef _MSC_VER
            boost::replace_all(output, "\r\n", "\n");
            OemToCharA(output.c_str(), const_cast<char*>(output.c_str()));
#endif
            if (!ec) {
                boost::asio::async_read(pipeOut, outBuffer, onStdOut);
            }
        };

        std::vector<char> vErr(128 << 10);
        auto errBuffer{ boost::asio::buffer(vErr) };
        boost::process::async_pipe pipeErr(ios);
        std::function<void(const boost::system::error_code& ec, std::size_t n)> onStdErr;
        onStdErr = [&](const boost::system::error_code& ec, size_t n) {
            if (withEventsLoop) {
                ProcessEventsDynamicFunction();
            }
            error.reserve(error.size() + n);
            error.insert(error.end(), vErr.begin(), vErr.begin() + n);
            error.erase(std::remove(error.begin(), error.end(), '\0'), error.end());
#ifdef _MSC_VER
            boost::replace_all(error, "\r\n", "\n");
            OemToCharA(error.c_str(), const_cast<char*>(error.c_str()));
#endif
            if (!ec) {
                boost::asio::async_read(pipeErr, errBuffer, onStdErr);
            }
        };

        cmd = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command
            + L"\"";

        boost::process::child childProcess(cmd, boost::process::std_out > pipeOut,
            boost::process::std_err > pipeErr, boost::process::std_in < boost::process::null);
        boost::asio::async_read(pipeOut, outBuffer, onStdOut);
        boost::asio::async_read(pipeErr, errBuffer, onStdErr);
        ios.run();
        childProcess.wait();
        ierr = childProcess.exit_code();
    }
    ArrayOf res;
    if (ierr == 0) {
        res = ArrayOf::characterArrayConstructor(output);
    } else {
        res = ArrayOf::characterArrayConstructor(error);
    }
    return res;
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
} // namespace Nelson
//=============================================================================
