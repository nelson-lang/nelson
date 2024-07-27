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
#define _WIN32_WINNT 0x0601 // For Windows 7
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#endif
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/process/shell.hpp>
#include <thread>
#include "SystemCommandTask.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
SystemCommandTask::terminate()
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
SystemCommandTask::isRunning()
{
    return _running;
}
//=============================================================================
std::tuple<int, std::wstring, uint64>
SystemCommandTask::getResult()
{
    if (_terminate) {
        return std::make_tuple(_exitCode, L"ABORTED", _duration);
    }
    return std::make_tuple(_exitCode, _message, _duration);
};
//=============================================================================
uint64
SystemCommandTask::getDuration()
{
    std::chrono::steady_clock::time_point _currentTimePoint = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<std::chrono::milliseconds>(
        _currentTimePoint - _beginTimePoint)
        .count();
}
//=============================================================================
void
SystemCommandTask::evaluateCommand(const std::wstring& command, uint64 timeout)
{
    _beginTimePoint = std::chrono::steady_clock::now();

    _terminate = false;
    _running = true;
    FileSystemWrapper::Path tempOutputFile(FileSystemWrapper::Path::unique_path());
    FileSystemWrapper::Path tempErrorFile(FileSystemWrapper::Path::unique_path());
    bool mustDetach = false;
    std::wstring _command = detectDetachProcess(command, mustDetach);
    std::wstring argsShell;
#ifdef _MSC_VER
    argsShell = L" /a /c ";
#else
    argsShell = L" -c ";
#endif
    std::wstring cmd
        = L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command + L"\"";
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
int
SystemCommandTask::exitCodeAbort()
{
#ifdef _MSC_VER
    return int(258); // WAIT_TIMEOUT
#else
    return int(128 + SIGABRT);
#endif
}
//=============================================================================
std::wstring
SystemCommandTask::detectDetachProcess(const std::wstring& command, bool& haveDetach)
{
    std::wstring _command = cleanCommand(command);
    if (_command.empty()) {
        haveDetach = true;
    } else {
        if (*_command.rbegin() == L'&') {
            _command.pop_back();
            _command = cleanCommand(_command);
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
SystemCommandTask::cleanCommand(const std::wstring& command)
{
    return StringHelpers::trim_right_copy(command);
}
//=============================================================================
std::wstring
SystemCommandTask::readFile(const FileSystemWrapper::Path& filePath)
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
}
//=============================================================================
