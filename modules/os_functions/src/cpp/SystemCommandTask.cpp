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
#include <errno.h>
#endif
#include "SystemCommandTask.hpp"
#include <thread>
#include <fstream>
#include <sstream>
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
static std::string
jsonEscape(const std::string& value)
{
    std::ostringstream escaped;
    for (char c : value) {
        switch (c) {
        case '\\':
            escaped << "\\\\";
            break;
        case '"':
            escaped << "\\\"";
            break;
        case '\n':
            escaped << "\\n";
            break;
        case '\r':
            escaped << "\\r";
            break;
        case '\t':
            escaped << "\\t";
            break;
        default:
            escaped << c;
            break;
        }
    }
    return escaped.str();
}
//=============================================================================
static std::string
registerProcess(uint64 pid, const std::wstring& command, uint64 timeout, const std::string& policy)
{
    auto registryFile = FileSystemWrapper::Path::temp_directory_path()
        / ("nelson_process_registry_" + std::to_string(pid) + ".json");
    std::ofstream stream(registryFile.string(), std::ios::out | std::ios::trunc);
    if (stream.is_open()) {
        stream << "{\n";
        stream << "  \"pid\": " << pid << ",\n";
        stream << "  \"timeout\": " << timeout << ",\n";
        stream << "  \"state\": \"running\",\n";
        stream << "  \"process_policy\": \"" << policy << "\",\n";
        stream << "  \"command\": \"" << jsonEscape(wstring_to_utf8(command)) << "\"\n";
        stream << "}\n";
    }
    return registryFile.string();
}
//=============================================================================
static void
unregisterProcess(const std::string& registryFile)
{
    if (!registryFile.empty()) {
        FileSystemWrapper::Path::remove(registryFile);
    }
}
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
        std::wstring cmd;
#ifdef _MSC_VER
        cmd = buildCommandString(_command);
#else
        cmd = _command;
#endif
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
#ifdef _MSC_VER
    // Plain cmd.exe token without quotes, so CreateProcess can find the executable.
    std::wstring shell = L"cmd.exe";
#else
    std::wstring shell = L"/bin/sh";
#endif
    std::wstring argsShell = getPlatformSpecificShellArgs();
    return shell + L" " + argsShell + L"\"" + _command + L"\"";
}
//=============================================================================
void
SystemCommandTask::executeDetachedProcess(const std::wstring& cmd)
{
#ifdef _WIN32
    // An explicit application name avoids cmd.exe parsing issues with quoted parameters and
    // 'start'.
    std::wstring commandLine = cmd;
    std::wstring appName;
    auto lower = [](std::wstring s) {
        std::transform(s.begin(), s.end(), s.begin(), ::towlower);
        return s;
    };
    std::wstring lc = lower(commandLine);
    size_t pos = std::wstring::npos;
    size_t p1 = lc.find(L"cmd.exe");
    if (p1 != std::wstring::npos) {
        pos = commandLine.find(L' ', p1);
        if (pos == std::wstring::npos) {
            pos = commandLine.size();
        }
        wchar_t systemDirectory[MAX_PATH] = { 0 };
        if (GetSystemDirectoryW(systemDirectory, MAX_PATH) != 0) {
            appName = std::wstring(systemDirectory) + L"\\cmd.exe";
        } else {
            appName = L"cmd.exe";
        }
    }
    STARTUPINFOW si {};
    PROCESS_INFORMATION pi {};
    si.cb = sizeof(si);
    BOOL ok = FALSE;
    if (!appName.empty()) {
        std::vector<wchar_t> cmdargs(commandLine.begin(), commandLine.end());
        cmdargs.push_back(0);
        ok = CreateProcessW(appName.c_str(), cmdargs.data(), nullptr, nullptr, FALSE,
            CREATE_NO_WINDOW, nullptr, nullptr, &si, &pi);
    } else {
        std::vector<wchar_t> cmdw(commandLine.begin(), commandLine.end());
        cmdw.push_back(0);
        ok = CreateProcessW(nullptr, cmdw.data(), nullptr, nullptr, FALSE, CREATE_NO_WINDOW,
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
        if (setsid() < 0) {
            _exitCode = -1;
        }
        FILE* in = fopen("/dev/null", "r");
        if (in) {
            dup2(fileno(in), STDIN_FILENO);
            fclose(in);
        }
        std::string utf8cmd = wstring_to_utf8(cmd);
        execl("/bin/sh", "sh", "-c", utf8cmd.c_str(), (char*)nullptr);
        _exitCode = exitCodeAbort();
        exit(127);
    } else {
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
    SetHandleInformation(hOut, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    SetHandleInformation(hErr, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);

    HANDLE hIn = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, nullptr);
    if (hIn == INVALID_HANDLE_VALUE) {
        hIn = GetStdHandle(STD_INPUT_HANDLE);
    } else {
        SetHandleInformation(hIn, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    }

    STARTUPINFOW si {};
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = hOut;
    si.hStdError = hErr;
    si.hStdInput = hIn;

    HANDLE job = CreateJobObjectW(nullptr, nullptr);
    if (job != nullptr) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION jobInfo {};
        jobInfo.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        if (!SetInformationJobObject(
                job, JobObjectExtendedLimitInformation, &jobInfo, sizeof(jobInfo))) {
            CloseHandle(job);
            job = nullptr;
        }
    }

    PROCESS_INFORMATION pi {};
    std::wstring commandLine = cmd; // CreateProcessW may modify buffer
    bool startSuspended = job != nullptr;
    DWORD creationFlags = startSuspended ? CREATE_SUSPENDED : 0;
    BOOL ok = CreateProcessW(nullptr, &commandLine[0], nullptr, nullptr, TRUE, creationFlags,
        nullptr, nullptr, &si, &pi);
    if (!ok) {
        if (job != nullptr) {
            CloseHandle(job);
        }
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
    if (job != nullptr) {
        if (!AssignProcessToJobObject(job, pi.hProcess)) {
            CloseHandle(job);
            job = nullptr;
        }
    }
    if (startSuspended) {
        ResumeThread(pi.hThread);
    }
    if (hIn != nullptr && hIn != INVALID_HANDLE_VALUE && hIn != GetStdHandle(STD_INPUT_HANDLE)) {
        CloseHandle(hIn);
    }
    CloseHandle(hOut);
    CloseHandle(hErr);

    PROCESS_CHILD child;
    child.processHandle = pi.hProcess;
    child.jobHandle = job;
    child.processId = pi.dwProcessId;
    child.registryFile = registerProcess(child.processId, cmd, timeout,
        child.jobHandle != nullptr ? "windows-job-object" : "windows-process");
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
    CloseHandle(child.processHandle);
    if (child.jobHandle != nullptr) {
        CloseHandle(child.jobHandle);
    }
    unregisterProcess(child.registryFile);
#else
    pid_t pid = fork();
    if (pid < 0) {
        _exitCode = -1;
        _message = L"fork failed";
        return;
    }
    if (pid == 0) {
        // Run the command in its own process group so timeout/abort can kill the whole tree.
        setpgid(0, 0);
        // Always redirect stdin to /dev/null to avoid child blocking on interactive input.
        FILE* in = fopen("/dev/null", "r");
        FILE* out = fopen(tempOutputFile.string().c_str(), "w");
        FILE* err = fopen(tempErrorFile.string().c_str(), "w");
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
        std::string utf8cmd = wstring_to_utf8(cmd);
        execl("/bin/sh", "sh", "-c", utf8cmd.c_str(), (char*)nullptr);
        exit(127);
    } else {
        PROCESS_CHILD child;
        child.pid = pid;
        child.registryFile
            = registerProcess(static_cast<uint64>(child.pid), cmd, timeout, "posix-process-group");
        monitorChildProcess(child, timeout);
        if (!_terminate) {
            _message = readProcessOutput(tempOutputFile, tempErrorFile);
            // Match Windows Job Object cleanup: if the shell exited but left descendants in
            // its process group, do not let them outlive the runner.
            kill(-child.pid, SIGKILL);
        }
        unregisterProcess(child.registryFile);
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
        if (childProcess.jobHandle != nullptr) {
            TerminateJobObject(childProcess.jobHandle, (UINT)_exitCode);
        } else {
            TerminateProcess(childProcess.processHandle, (UINT)_exitCode);
        }
    }
#else
    if (!childProcess.valid()) {
        return;
    }
    int status = 0;
    while (!_terminate) {
        pid_t r = waitpid(childProcess.pid, &status, WNOHANG);
        if (r == childProcess.pid) {
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
        // Kill the process group first, then reap the direct child.
        kill(-childProcess.pid, SIGKILL);
        kill(childProcess.pid, SIGKILL);
        while (waitpid(childProcess.pid, &status, 0) == -1 && errno == EINTR) { }
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
    return L" /a /s /c ";
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
        haveDetach = false;
        return _command;
    }
    if (*_command.rbegin() == L'&') {
        _command.pop_back();
        _command = cleanCommand(_command);
        // A lone "&" is a no-op: " &" would be a shell syntax error.
        if (_command.empty()) {
            haveDetach = false;
            return _command;
        }
        haveDetach = true;
#ifdef _MSC_VER
    } else if (StringHelpers::istarts_with(_command, L"start ")
        || StringHelpers::istarts_with(_command, L"start\t")) {
        haveDetach = true;
#endif
    } else {
        haveDetach = false;
    }
    if (haveDetach) {
#ifndef _MSC_VER
        _command = _command + L" &";
#endif
        // On Windows a trailing '&' runs the command hidden (CREATE_NO_WINDOW) without waiting.
        // It is not wrapped with 'start', which always opens a visible console window.
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
#ifdef _MSC_VER
static bool
isValidUtf8(const std::string& str)
{
    if (str.empty()) {
        return true;
    }
    int result = MultiByteToWideChar(
        CP_UTF8, MB_ERR_INVALID_CHARS, str.c_str(), static_cast<int>(str.size()), nullptr, 0);
    return result > 0;
}
//=============================================================================
static std::wstring
decodeProcessOutput(const std::string& raw)
{
    if (raw.empty()) {
        return {};
    }

    if (isValidUtf8(raw)) {
        std::wstring wresult;
        if (utf8_to_wstring_Windows(raw, wresult)) {
            return wresult;
        }
    }

    // Legacy OEM output (old cmd, batch scripts).
    std::string ansi = raw;
    OemToCharBuffA(ansi.c_str(), ansi.data(), static_cast<DWORD>(ansi.size()));

    int wlen
        = MultiByteToWideChar(CP_ACP, 0, ansi.c_str(), static_cast<int>(ansi.size()), nullptr, 0);
    if (wlen <= 0) {
        return utf8_to_wstring(raw);
    }
    std::wstring wresult(wlen, L'\0');
    MultiByteToWideChar(
        CP_ACP, 0, ansi.c_str(), static_cast<int>(ansi.size()), wresult.data(), wlen);
    return wresult;
}
#endif
//=============================================================================
std::wstring
SystemCommandTask::readFile(const FileSystemWrapper::Path& filePath)
{
    std::string result;
#ifdef _MSC_VER
    FILE* pFile = _wfopen(filePath.wstring().c_str(), L"rb"); // binary: preserve raw bytes
#else
    FILE* pFile = fopen(filePath.string().c_str(), "r");
#endif
    if (pFile != nullptr) {
        constexpr std::streamsize bufferSize = 16384;
        char buffer[bufferSize];
        while (fgets(buffer, static_cast<int>(bufferSize), pFile)) {
            result.append(buffer);
        }
        if (!result.empty() && result.back() != '\n') {
            result.push_back('\n');
        }
        fclose(pFile);
    }

#ifdef _MSC_VER
    StringHelpers::replace_all(result, "\r\n", "\n");
    return decodeProcessOutput(result);
#else
    return utf8_to_wstring(result);
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
