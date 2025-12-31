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
#define _WIN32_WINNT 0x0601 // For Windows 7
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#endif
#include "SystemCommandTask.hpp"
#include <thread>
#include <fstream>
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#ifdef _MSC_VER
static std::string
get_last_error_message()
{
    DWORD errorMessageID = ::GetLastError();
    if (errorMessageID == 0) {
        return {};
    }
    LPSTR messageBuffer = nullptr;
    size_t size = FormatMessageA(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0,
        NULL);
    std::string message(messageBuffer, size);
    LocalFree(messageBuffer);
    return message;
}
#endif
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
        // On Windows we need a command line that launches cmd.exe; on POSIX
        // we should pass the raw command to execl("/bin/sh", "sh", "-c", ...).
        std::wstring cmd;
#ifdef _MSC_VER
        cmd = buildCommandString(_command);
#else
        cmd = _command;
#endif
        executeAttachedProcess(cmd, *tempOutputFile, *tempErrorFile, timeout);
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
#ifdef _MSC_VER
    // Use plain cmd.exe token (no surrounding quotes) so CreateProcess can correctly find the
    // executable
    std::wstring shell = L"cmd.exe";
#else
    std::wstring shell = L"/bin/sh";
#endif
    std::wstring argsShell = getPlatformSpecificShellArgs();
    // produce: cmd.exe /a /c "command"
    return shell + L" " + argsShell + L"\"" + _command + L"\"";
}
//=============================================================================
void
SystemCommandTask::executeDetachedProcess(const std::wstring& cmd)
{
#ifdef _WIN32
    // If cmd starts with cmd.exe, call CreateProcess with application name and args to avoid
    // cmd.exe parsing issues with quoted parameters and 'start'.
    std::wstring commandLine = cmd;
    std::wstring appName;
    std::wstring args;
    auto lower = [](std::wstring s) {
        std::transform(s.begin(), s.end(), s.begin(), ::towlower);
        return s;
    };
    std::wstring lc = lower(commandLine);
    size_t pos = std::wstring::npos;
    // look for cmd.exe or "cmd.exe"
    size_t p1 = lc.find(L"cmd.exe");
    if (p1 != std::wstring::npos) {
        // find first space after cmd.exe
        pos = commandLine.find(L' ', p1);
        if (pos == std::wstring::npos) {
            pos = commandLine.size();
        }
        appName = L"cmd.exe";
        if (pos < commandLine.size()) {
            args = commandLine.substr(pos + 1);
        }
    }
    STARTUPINFOW si {};
    PROCESS_INFORMATION pi {};
    si.cb = sizeof(si);
    BOOL ok = FALSE;
    if (!appName.empty()) {
        // use CreateProcess with explicit app name and args
        std::vector<wchar_t> cmdargs(args.begin(), args.end());
        cmdargs.push_back(0);
        ok = CreateProcessW(appName.c_str(), cmdargs.data(), nullptr, nullptr, FALSE,
            CREATE_NEW_CONSOLE, nullptr, nullptr, &si, &pi);
    } else {
        // fallback: let system parse the whole command line
        std::vector<wchar_t> cmdw(commandLine.begin(), commandLine.end());
        cmdw.push_back(0);
        ok = CreateProcessW(nullptr, cmdw.data(), nullptr, nullptr, FALSE, CREATE_NEW_CONSOLE,
            nullptr, nullptr, &si, &pi);
    }
    if (ok) {
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
        _exitCode = 0;
    } else {
        _exitCode = -1;
        _message = utf8_to_wstring(get_last_error_message());
    }
#else
    pid_t pid = fork();
    if (pid < 0) {
        _exitCode = -1;
        _message = L"fork failed";
        return;
    }
    if (pid == 0) {
        // child: detach
        if (setsid() < 0) {
            _exitCode = -1;
        }
        // redirect stdin to /dev/null to avoid blocking on interactive commands
        FILE* in = fopen("/dev/null", "r");
        if (in) {
            dup2(fileno(in), STDIN_FILENO);
            fclose(in);
        }
        // execute shell -c cmd
        std::string utf8cmd = wstring_to_utf8(cmd);
        execl("/bin/sh", "sh", "-c", utf8cmd.c_str(), (char*)nullptr);
        _exitCode = exitCodeAbort();
        exit(127);
    } else {
        // parent returns immediately
        _exitCode = 0;
    }
#endif
}
//=============================================================================
void
SystemCommandTask::executeAttachedProcess(const std::wstring& cmd,
    const FileSystemWrapper::Path& tempOutputFile, const FileSystemWrapper::Path& tempErrorFile,
    uint64 timeout)
{
#ifdef _WIN32
    // Open files for redirect
    HANDLE hOut = CreateFileW(tempOutputFile.wstring().c_str(), GENERIC_WRITE, FILE_SHARE_READ,
        nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
    HANDLE hErr = CreateFileW(tempErrorFile.wstring().c_str(), GENERIC_WRITE, FILE_SHARE_READ,
        nullptr, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
    if (hOut == INVALID_HANDLE_VALUE || hErr == INVALID_HANDLE_VALUE) {
        if (hOut != INVALID_HANDLE_VALUE) {
            CloseHandle(hOut);
        }
        if (hErr != INVALID_HANDLE_VALUE) {
            CloseHandle(hErr);
        }
        _exitCode = -1;
        _message = L"Cannot open temp files for redirect.";
        return;
    }
    // Make handles inheritable so child can inherit them
    SetHandleInformation(hOut, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    SetHandleInformation(hErr, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);

    // Open NUL for child stdin to avoid interactive commands blocking
    HANDLE hIn = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, nullptr);
    if (hIn == INVALID_HANDLE_VALUE) {
        // fallback to non-inheritable stdin if NUL cannot be opened
        hIn = GetStdHandle(STD_INPUT_HANDLE);
    } else {
        SetHandleInformation(hIn, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    }

    // Setup STARTUPINFO
    STARTUPINFOW si {};
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = hOut;
    si.hStdError = hErr;
    si.hStdInput = hIn;

    PROCESS_INFORMATION pi {};
    std::wstring commandLine = cmd; // CreateProcessW may modify buffer
    BOOL ok = CreateProcessW(
        nullptr, &commandLine[0], nullptr, nullptr, TRUE, 0, nullptr, nullptr, &si, &pi);
    if (!ok) {
        if (hIn != nullptr && hIn != INVALID_HANDLE_VALUE
            && hIn != GetStdHandle(STD_INPUT_HANDLE)) {
            CloseHandle(hIn);
        }
        CloseHandle(hOut);
        CloseHandle(hErr);
        _exitCode = -1;
        _message = utf8_to_wstring(get_last_error_message());
        return;
    }
    // we can close our copies of the handles; child has its own inherited handles
    if (hIn != nullptr && hIn != INVALID_HANDLE_VALUE && hIn != GetStdHandle(STD_INPUT_HANDLE)) {
        CloseHandle(hIn);
    }
    CloseHandle(hOut);
    CloseHandle(hErr);

    PROCESS_CHILD child;
    child.processHandle = pi.hProcess;
    child.processId = pi.dwProcessId;
    CloseHandle(pi.hThread);

    monitorChildProcess(child, timeout);

    DWORD exitCode = 0;
    if (!_terminate) {
        if (GetExitCodeProcess(child.processHandle, &exitCode)) {
            _exitCode = static_cast<int>(exitCode);
            _message = readProcessOutput(tempOutputFile, tempErrorFile);
        } else {
            _exitCode = -1;
            _message = utf8_to_wstring(get_last_error_message());
        }
    }
    // ensure process handle closed
    CloseHandle(child.processHandle);
#else
    pid_t pid = fork();
    if (pid < 0) {
        _exitCode = -1;
        _message = L"fork failed";
        return;
    }
    if (pid == 0) {
        // child process
        // open files
        // Always redirect stdin to /dev/null to avoid child blocking on interactive input.
        FILE* in = fopen("/dev/null", "r");
        FILE* out = fopen(tempOutputFile.string().c_str(), "w");
        FILE* err = fopen(tempErrorFile.string().c_str(), "w");
        // consider missing out/err/in an error
        if (!out || !err || !in) {
            _exitCode = -1;
        }
        if (in) {
            dup2(fileno(in), STDIN_FILENO);
            fclose(in);
        }
        if (out) {
            dup2(fileno(out), STDOUT_FILENO);
            fclose(out);
        }
        if (err) {
            dup2(fileno(err), STDERR_FILENO);
            fclose(err);
        }
        // execute shell -c cmd
        std::string utf8cmd = wstring_to_utf8(cmd);
        execl("/bin/sh", "sh", "-c", utf8cmd.c_str(), (char*)nullptr);
        // if exec fails
        exit(127);
    } else {
        // parent
        PROCESS_CHILD child;
        child.pid = pid;
        monitorChildProcess(child, timeout);
        if (!_terminate) {
            // monitorChildProcess now sets _exitCode for normal termination,
            // so just read the process output here.
            _message = readProcessOutput(tempOutputFile, tempErrorFile);
        }
    }
#endif
}
//=============================================================================
void
SystemCommandTask::monitorChildProcess(PROCESS_CHILD& childProcess, uint64 timeout)
{
#ifdef _WIN32
    if (!childProcess.valid()) {
        return;
    }
    while (!_terminate) {
        DWORD res = WaitForSingleObject(childProcess.processHandle, 0);
        if (res == WAIT_OBJECT_0) {
            // process finished
            break;
        }
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
        // terminate process
        TerminateProcess(childProcess.processHandle, (UINT)_exitCode);
    }
#else
    if (!childProcess.valid()) {
        return;
    }
    int status = 0;
    while (!_terminate) {
        pid_t r = waitpid(childProcess.pid, &status, WNOHANG);
        if (r == childProcess.pid) {
            // process finished: record exit code based on status
            if (WIFEXITED(status)) {
                _exitCode = WEXITSTATUS(status);
            } else if (WIFSIGNALED(status)) {
                _exitCode = 128 + WTERMSIG(status);
            } else {
                _exitCode = -1;
            }
            break;
        }
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
        kill(childProcess.pid, SIGKILL);
    }
#endif
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
        // empty command should not be treated as a detached job
        haveDetach = false;
        return _command;
    }
    if (*_command.rbegin() == L'&') {
        // remove trailing '&'
        _command.pop_back();
        _command = cleanCommand(_command);
        // if removing '&' leaves an empty command (original was just "&"),
        // treat it as a no-op to avoid producing " &" which is a shell syntax error
        if (_command.empty()) {
            haveDetach = false;
            return _command;
        }
        haveDetach = true;
    } else {
        haveDetach = false;
    }
    if (haveDetach) {
#ifdef _MSC_VER
        // Use an explicit empty title for 'start' to avoid treating the first quoted
        // argument as a window title: start "" command args
        _command = L"start \"\" " + _command;
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
