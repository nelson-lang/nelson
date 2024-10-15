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
#include <thread>
#include <fstream>
#include "SystemCommandTask.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const int SLEEP_DURATION_MS = 10;
//=============================================================================
void
SystemCommandTask::evaluateCommand(const std::wstring& command, uint64 timeout)
{
    _beginTimePoint = std::chrono::steady_clock::now();
    _terminate = false;
    _running = true;

    auto tempOutputFile
        = std::make_unique<FileSystemWrapper::Path>(FileSystemWrapper::Path::unique_path());
    auto tempErrorFile
        = std::make_unique<FileSystemWrapper::Path>(FileSystemWrapper::Path::unique_path());

    try {
        bool mustDetach = false;
        std::wstring _command = detectDetachProcess(command, mustDetach);
        std::wstring cmd = buildCommandString(_command);

        if (mustDetach) {
            executeDetachedProcess(cmd);
        } else {
            executeAttachedProcess(cmd, *tempOutputFile, *tempErrorFile, timeout);
        }
    } catch (const std::exception& e) {
        _message = utf8_to_wstring(e.what());
        _exitCode = -1;
    }

    cleanupTempFiles(*tempOutputFile, *tempErrorFile);

    _running = false;
    _duration = this->getDuration();
}
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
std::wstring
SystemCommandTask::buildCommandString(const std::wstring& _command)
{
    std::wstring argsShell = getPlatformSpecificShellArgs();
    return L"\"" + boost::process::shell().wstring() + L"\" " + argsShell + L"\"" + _command
        + L"\"";
}
//=============================================================================
void
SystemCommandTask::executeDetachedProcess(const std::wstring& cmd)
{
    boost::process::child childProcess(cmd);
    childProcess.detach();
    _exitCode = 0;
}
//=============================================================================
void
SystemCommandTask::executeAttachedProcess(const std::wstring& cmd,
    const FileSystemWrapper::Path& tempOutputFile, const FileSystemWrapper::Path& tempErrorFile,
    uint64 timeout)
{
    boost::process::child childProcess(cmd,
        boost::process::std_out > tempOutputFile.generic_string().c_str(),
        boost::process::std_err > tempErrorFile.generic_string().c_str(),
        boost::process::std_in < boost::process::null);

    monitorChildProcess(childProcess, timeout);

    if (!_terminate) {
        _exitCode = (int)childProcess.exit_code();
        _message = readProcessOutput(tempOutputFile, tempErrorFile);
    }
}
//=============================================================================
void
SystemCommandTask::monitorChildProcess(boost::process::child& childProcess, uint64 timeout)
{
    while (childProcess.running() && !_terminate) {
        auto _currentTimePoint = std::chrono::steady_clock::now();
        if (timeout != 0
            && (std::chrono::duration_cast<std::chrono::seconds>(
                    _currentTimePoint - _beginTimePoint)
                >= std::chrono::seconds(timeout))) {
            _terminate = true;
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(SLEEP_DURATION_MS));
    }

    if (_terminate) {
        _duration = this->getDuration();
        _exitCode = exitCodeAbort();
        childProcess.terminate();
    }
}
//=============================================================================
std::wstring
SystemCommandTask::readProcessOutput(
    const FileSystemWrapper::Path& tempOutputFile, const FileSystemWrapper::Path& tempErrorFile)
{
    std::wstring outputResult;
    if (_exitCode) {

        outputResult = readFile(tempErrorFile);
        if (outputResult.empty()) {
            outputResult = readFile(tempOutputFile);
        }
    } else {
        outputResult = readFile(tempOutputFile);
    }
    return outputResult;
}
//=============================================================================
void
SystemCommandTask::cleanupTempFiles(
    const FileSystemWrapper::Path& tempOutputFile, const FileSystemWrapper::Path& tempErrorFile)
{
    FileSystemWrapper::Path::remove(tempOutputFile);
    FileSystemWrapper::Path::remove(tempErrorFile);
}
//=============================================================================
std::wstring
SystemCommandTask::getPlatformSpecificShellArgs()
{
#ifdef _MSC_VER
    return L" /a /c ";
#else
    return L" -c ";
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
#ifdef _MSC_VER
    FILE* pFile = _wfopen(filePath.wstring().c_str(), L"r");
#else
    FILE* pFile = fopen(filePath.string().c_str(), "r");
#endif
    if (pFile != nullptr) {
        constexpr std::streamsize bufferSize = 16384;
        char buffer[bufferSize];

        while (fgets(buffer, bufferSize * sizeof(char), pFile)) {
#ifdef _MSC_VER
            std::string str = std::string(buffer);
            StringHelpers::replace_all(str, "\r\n", "\n");
            OemToCharBuffA(str.c_str(), const_cast<char*>(str.c_str()), (DWORD)str.size());
            result.append(str);
#else
            result.append(buffer);
#endif
        }
        if (!result.empty() && result.back() != '\n') {
            result.push_back('\n');
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
