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
#endif
//=============================================================================
#include <algorithm>
#include <chrono>
#include <cstring>
#include <cwctype>
#include <thread>
#if defined(_WIN32)
#include <windows.h>
#elif !defined(__EMSCRIPTEN__)
#include <csignal>
#include <cerrno>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif
#include "ProcessObject.hpp"
#include "HandleManager.hpp"
#include "Interface.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Processes kept alive past their last workspace reference, until delete().
static std::vector<HandleRef> retainedProcesses;
static std::mutex retainedProcessesMutex;
//=============================================================================
static bool
isRetainedProcessRef(const HandleRef& ref, const ProcessObject* object);
#if defined(_WIN32)
static std::wstring
buildWindowsCommandLine(const std::wstring& command, const wstringVector& arguments);
static bool
resolveWindowsCommand(const std::wstring& command, std::wstring& resolved);
#elif !defined(__EMSCRIPTEN__)
static std::vector<std::string>
buildPosixArgv(const std::wstring& command, const wstringVector& arguments);
#endif
//=============================================================================
struct ProcessChannel
{
#if defined(_WIN32)
    HANDLE hProcess = nullptr;
    HANDLE hThread = nullptr;
    HANDLE hJob = nullptr;
    HANDLE hStdinWrite = nullptr;
    HANDLE hStdoutRead = nullptr;
    HANDLE hStderrRead = nullptr;
    DWORD processId = 0;
#elif !defined(__EMSCRIPTEN__)
    pid_t processId = -1;
    int stdinWrite = -1;
    int stdoutRead = -1;
    int stderrRead = -1;
#endif
    bool exited = false;
    int exitStatus = 0;
    bool stdinClosed = false;
};
//=============================================================================
ProcessObject::ProcessObject(const std::wstring& command, const wstringVector& arguments,
    const std::wstring& workingDirectory)
    : HandleGenericObject(NLS_HANDLE_PROCESS_CATEGORY_STR, this, false)
    , command(command)
    , arguments(arguments)
    , workingDirectory(workingDirectory)
    , launched(false)
    , channel(std::make_unique<ProcessChannel>())
{
    propertiesNames = { L"Arguments", L"BytesAvailable", L"Command", L"ExitCode", L"ProcessId",
        L"Running", L"Type", L"WorkingDirectory" };
    methodsNames = { L"close", L"read", L"readline", L"stderr", L"terminate", L"wait", L"write" };
    launch();
}
//=============================================================================
ProcessObject::~ProcessObject()
{
    terminateProcess();
    closeStdin();
    joinReaders();
#if defined(_WIN32)
    if (channel->hProcess != nullptr) {
        CloseHandle(channel->hProcess);
    }
    if (channel->hThread != nullptr) {
        CloseHandle(channel->hThread);
    }
    if (channel->hStdoutRead != nullptr) {
        CloseHandle(channel->hStdoutRead);
    }
    if (channel->hStderrRead != nullptr) {
        CloseHandle(channel->hStderrRead);
    }
    if (channel->hJob != nullptr) {
        CloseHandle(channel->hJob);
    }
#elif !defined(__EMSCRIPTEN__)
    if (channel->stdoutRead != -1) {
        ::close(channel->stdoutRead);
    }
    if (channel->stderrRead != -1) {
        ::close(channel->stderrRead);
    }
#endif
}
//=============================================================================
void
ProcessObject::onDelete(Evaluator* eval)
{
    terminateProcess();
    releaseRetainedReference();
}
//=============================================================================
bool
ProcessObject::started() const
{
    return launched.load();
}
//=============================================================================
std::wstring
ProcessObject::startError() const
{
    return launchError;
}
//=============================================================================
#if defined(__EMSCRIPTEN__)
void
ProcessObject::launch()
{
    launched = false;
    launchError = L"Process creation is not supported in this build.";
}
#elif defined(_WIN32)
void
ProcessObject::launch()
{
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = nullptr;

    HANDLE stdinRead = nullptr, stdinWrite = nullptr;
    HANDLE stdoutRead = nullptr, stdoutWrite = nullptr;
    HANDLE stderrRead = nullptr, stderrWrite = nullptr;
    if (!CreatePipe(&stdinRead, &stdinWrite, &sa, 0)
        || !CreatePipe(&stdoutRead, &stdoutWrite, &sa, 0)
        || !CreatePipe(&stderrRead, &stderrWrite, &sa, 0)) {
        launchError = L"Unable to create process pipes.";
        return;
    }
    SetHandleInformation(stdinWrite, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(stdoutRead, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(stderrRead, HANDLE_FLAG_INHERIT, 0);

    STARTUPINFOW si;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdInput = stdinRead;
    si.hStdOutput = stdoutWrite;
    si.hStdError = stderrWrite;

    PROCESS_INFORMATION pi;
    ZeroMemory(&pi, sizeof(pi));

    std::wstring resolvedCommand;
    bool needsShell = resolveWindowsCommand(command, resolvedCommand);
    std::wstring commandLine;
    if (needsShell) {
        wchar_t* comspec = nullptr;
        size_t comspecLength = 0;
        _wdupenv_s(&comspec, &comspecLength, L"ComSpec");
        std::wstring shell = (comspec != nullptr) ? comspec : L"cmd.exe";
        free(comspec);
        wstringVector shellArguments;
        shellArguments.push_back(L"/c");
        shellArguments.push_back(resolvedCommand);
        for (const std::wstring& argument : arguments) {
            shellArguments.push_back(argument);
        }
        commandLine = buildWindowsCommandLine(shell, shellArguments);
    } else {
        commandLine = buildWindowsCommandLine(resolvedCommand, arguments);
    }
    std::vector<wchar_t> mutableCommandLine(commandLine.begin(), commandLine.end());
    mutableCommandLine.push_back(L'\0');
    const wchar_t* cwd = workingDirectory.empty() ? nullptr : workingDirectory.c_str();

    BOOL ok = CreateProcessW(nullptr, mutableCommandLine.data(), nullptr, nullptr, TRUE,
        CREATE_NO_WINDOW | CREATE_SUSPENDED, nullptr, cwd, &si, &pi);

    CloseHandle(stdinRead);
    CloseHandle(stdoutWrite);
    CloseHandle(stderrWrite);
    if (!ok) {
        CloseHandle(stdinWrite);
        CloseHandle(stdoutRead);
        CloseHandle(stderrRead);
        launchError = L"Unable to start the requested program.";
        return;
    }
    channel->hJob = CreateJobObjectW(nullptr, nullptr);
    if (channel->hJob != nullptr) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
        ZeroMemory(&limits, sizeof(limits));
        limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        SetInformationJobObject(
            channel->hJob, JobObjectExtendedLimitInformation, &limits, sizeof(limits));
        AssignProcessToJobObject(channel->hJob, pi.hProcess);
    }
    ResumeThread(pi.hThread);

    channel->hProcess = pi.hProcess;
    channel->hThread = pi.hThread;
    channel->processId = pi.dwProcessId;
    channel->hStdinWrite = stdinWrite;
    channel->hStdoutRead = stdoutRead;
    channel->hStderrRead = stderrRead;
    launched = true;
    stdoutReader = std::thread([this]() { this->stdoutReaderLoop(); });
    stderrReader = std::thread([this]() { this->stderrReaderLoop(); });
}
#else
void
ProcessObject::launch()
{
    int stdinPipe[2] = { -1, -1 };
    int stdoutPipe[2] = { -1, -1 };
    int stderrPipe[2] = { -1, -1 };
    if (pipe(stdinPipe) != 0 || pipe(stdoutPipe) != 0 || pipe(stderrPipe) != 0) {
        launchError = L"Unable to create process pipes.";
        return;
    }
    std::vector<std::string> argvStorage = buildPosixArgv(command, arguments);
    std::vector<char*> argv;
    argv.reserve(argvStorage.size() + 1);
    for (std::string& item : argvStorage) {
        argv.push_back(item.data());
    }
    argv.push_back(nullptr);
    std::string cwd = wstring_to_utf8(workingDirectory);

    pid_t pid = fork();
    if (pid < 0) {
        ::close(stdinPipe[0]);
        ::close(stdinPipe[1]);
        ::close(stdoutPipe[0]);
        ::close(stdoutPipe[1]);
        ::close(stderrPipe[0]);
        ::close(stderrPipe[1]);
        launchError = L"Unable to start the requested program.";
        return;
    }
    if (pid == 0) {
        setpgid(0, 0);
        dup2(stdinPipe[0], STDIN_FILENO);
        dup2(stdoutPipe[1], STDOUT_FILENO);
        dup2(stderrPipe[1], STDERR_FILENO);
        ::close(stdinPipe[0]);
        ::close(stdinPipe[1]);
        ::close(stdoutPipe[0]);
        ::close(stdoutPipe[1]);
        ::close(stderrPipe[0]);
        ::close(stderrPipe[1]);
        if (!cwd.empty()) {
            if (chdir(cwd.c_str()) != 0) {
                _exit(127);
            }
        }
        execvp(argv[0], argv.data());
        _exit(127);
    }
    setpgid(pid, pid);
    ::close(stdinPipe[0]);
    ::close(stdoutPipe[1]);
    ::close(stderrPipe[1]);
    channel->processId = pid;
    channel->stdinWrite = stdinPipe[1];
    channel->stdoutRead = stdoutPipe[0];
    channel->stderrRead = stderrPipe[0];
    launched = true;
    stdoutReader = std::thread([this]() { this->stdoutReaderLoop(); });
    stderrReader = std::thread([this]() { this->stderrReaderLoop(); });
}
#endif
//=============================================================================
void
ProcessObject::stdoutReaderLoop()
{
#if defined(__EMSCRIPTEN__)
    return;
#elif defined(_WIN32)
    char buffer[4096];
    DWORD count = 0;
    while (ReadFile(channel->hStdoutRead, buffer, sizeof(buffer), &count, nullptr) && count > 0) {
        std::lock_guard<std::mutex> lock(outMutex);
        outBuffer.append(buffer, count);
    }
#else
    char buffer[4096];
    ssize_t count = 0;
    while ((count = ::read(channel->stdoutRead, buffer, sizeof(buffer))) != 0) {
        if (count < 0) {
            if (errno == EINTR) {
                continue;
            }
            break;
        }
        std::lock_guard<std::mutex> lock(outMutex);
        outBuffer.append(buffer, static_cast<size_t>(count));
    }
#endif
}
//=============================================================================
void
ProcessObject::stderrReaderLoop()
{
#if defined(__EMSCRIPTEN__)
    return;
#elif defined(_WIN32)
    char buffer[4096];
    DWORD count = 0;
    while (ReadFile(channel->hStderrRead, buffer, sizeof(buffer), &count, nullptr) && count > 0) {
        std::lock_guard<std::mutex> lock(errMutex);
        errBuffer.append(buffer, count);
    }
#else
    char buffer[4096];
    ssize_t count = 0;
    while ((count = ::read(channel->stderrRead, buffer, sizeof(buffer))) != 0) {
        if (count < 0) {
            if (errno == EINTR) {
                continue;
            }
            break;
        }
        std::lock_guard<std::mutex> lock(errMutex);
        errBuffer.append(buffer, static_cast<size_t>(count));
    }
#endif
}
//=============================================================================
void
ProcessObject::joinReaders()
{
    if (stdoutReader.joinable()) {
        stdoutReader.join();
    }
    if (stderrReader.joinable()) {
        stderrReader.join();
    }
}
//=============================================================================
void
ProcessObject::closeStdin()
{
    if (channel->stdinClosed) {
        return;
    }
    channel->stdinClosed = true;
#if defined(_WIN32)
    if (channel->hStdinWrite != nullptr) {
        CloseHandle(channel->hStdinWrite);
        channel->hStdinWrite = nullptr;
    }
#elif !defined(__EMSCRIPTEN__)
    if (channel->stdinWrite != -1) {
        ::close(channel->stdinWrite);
        channel->stdinWrite = -1;
    }
#endif
}
//=============================================================================
bool
ProcessObject::writeBytes(const std::string& bytes, std::wstring& errorMessage)
{
#if defined(__EMSCRIPTEN__)
    errorMessage = L"Process creation is not supported in this build.";
    return false;
#else
    if (!launched.load()) {
        errorMessage = L"The process is not running.";
        return false;
    }
    std::lock_guard<std::mutex> lock(writeMutex);
    if (channel->stdinClosed) {
        errorMessage = L"The process input stream is closed.";
        return false;
    }
#if defined(_WIN32)
    const char* data = bytes.data();
    size_t remaining = bytes.size();
    while (remaining > 0) {
        DWORD written = 0;
        if (!WriteFile(channel->hStdinWrite, data, static_cast<DWORD>(remaining), &written, nullptr)
            || written == 0) {
            errorMessage = L"Unable to write to the process input stream.";
            return false;
        }
        data += written;
        remaining -= written;
    }
    return true;
#else
    const char* data = bytes.data();
    size_t remaining = bytes.size();
    while (remaining > 0) {
        ssize_t written = ::write(channel->stdinWrite, data, remaining);
        if (written <= 0) {
            if (written < 0 && errno == EINTR) {
                continue;
            }
            errorMessage = L"Unable to write to the process input stream.";
            return false;
        }
        data += written;
        remaining -= static_cast<size_t>(written);
    }
    return true;
#endif
#endif
}
//=============================================================================
void
ProcessObject::closeInput()
{
    std::lock_guard<std::mutex> lock(writeMutex);
    closeStdin();
}
//=============================================================================
std::string
ProcessObject::readAvailable()
{
    std::lock_guard<std::mutex> lock(outMutex);
    std::string data;
    data.swap(outBuffer);
    return data;
}
//=============================================================================
std::string
ProcessObject::readAvailableError()
{
    std::lock_guard<std::mutex> lock(errMutex);
    std::string data;
    data.swap(errBuffer);
    return data;
}
//=============================================================================
bool
ProcessObject::readLine(std::string& line)
{
    std::lock_guard<std::mutex> lock(outMutex);
    size_t position = outBuffer.find('\n');
    if (position == std::string::npos) {
        return false;
    }
    line = outBuffer.substr(0, position + 1);
    outBuffer.erase(0, position + 1);
    return true;
}
//=============================================================================
size_t
ProcessObject::bytesAvailable()
{
    std::lock_guard<std::mutex> lock(outMutex);
    return outBuffer.size();
}
//=============================================================================
bool
ProcessObject::isRunning()
{
#if defined(__EMSCRIPTEN__)
    return false;
#else
    if (!launched.load() || channel->exited) {
        return false;
    }
#if defined(_WIN32)
    DWORD status = WaitForSingleObject(channel->hProcess, 0);
    if (status == WAIT_TIMEOUT) {
        return true;
    }
    DWORD code = 0;
    if (GetExitCodeProcess(channel->hProcess, &code)) {
        channel->exitStatus = static_cast<int>(code);
    }
    channel->exited = true;
    return false;
#else
    int status = 0;
    pid_t result = ::waitpid(channel->processId, &status, WNOHANG);
    if (result == 0) {
        return true;
    }
    if (result == channel->processId) {
        if (WIFEXITED(status)) {
            channel->exitStatus = WEXITSTATUS(status);
        } else if (WIFSIGNALED(status)) {
            channel->exitStatus = 128 + WTERMSIG(status);
        }
        channel->exited = true;
    }
    return false;
#endif
#endif
}
//=============================================================================
bool
ProcessObject::hasExited()
{
    return !isRunning();
}
//=============================================================================
int
ProcessObject::exitCode()
{
    isRunning();
    return channel->exitStatus;
}
//=============================================================================
int64
ProcessObject::processId() const
{
#if defined(__EMSCRIPTEN__)
    return -1;
#else
    return static_cast<int64>(channel->processId);
#endif
}
//=============================================================================
void
ProcessObject::terminateProcess()
{
#if defined(__EMSCRIPTEN__)
    return;
#else
    if (!launched.load() || channel->exited) {
        return;
    }
#if defined(_WIN32)
    if (channel->hJob != nullptr) {
        TerminateJobObject(channel->hJob, 1);
    } else if (channel->hProcess != nullptr) {
        TerminateProcess(channel->hProcess, 1);
    }
#else
    ::kill(-channel->processId, SIGKILL);
    ::kill(channel->processId, SIGKILL);
#endif
    isRunning();
#endif
}
//=============================================================================
void
ProcessObject::waitFor(double timeoutSeconds)
{
#if !defined(__EMSCRIPTEN__)
    auto start = std::chrono::steady_clock::now();
    while (isRunning()) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
        if (timeoutSeconds >= 0) {
            auto elapsed = std::chrono::duration<double>(std::chrono::steady_clock::now() - start);
            if (elapsed.count() >= timeoutSeconds) {
                return;
            }
        }
    }
#endif
}
//=============================================================================
bool
ProcessObject::isProperty(const std::wstring& propertyName)
{
    return std::find(propertiesNames.begin(), propertiesNames.end(), propertyName)
        != propertiesNames.end();
}
//=============================================================================
bool
ProcessObject::isMethod(const std::wstring& methodName)
{
    return std::find(methodsNames.begin(), methodsNames.end(), methodName) != methodsNames.end();
}
//=============================================================================
wstringVector
ProcessObject::properties()
{
    return propertiesNames;
}
//=============================================================================
wstringVector
ProcessObject::methods()
{
    return methodsNames;
}
//=============================================================================
bool
ProcessObject::get(const std::wstring& propertyName, ArrayOf& res)
{
    if (propertyName == L"Command") {
        res = ArrayOf::characterArray(command);
    } else if (propertyName == L"Arguments") {
        res = ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(arguments);
    } else if (propertyName == L"WorkingDirectory") {
        res = ArrayOf::characterArray(workingDirectory);
    } else if (propertyName == L"ProcessId") {
        res = ArrayOf::doubleScalar(static_cast<double>(processId()));
    } else if (propertyName == L"Running") {
        res = ArrayOf::logicalScalar(isRunning());
    } else if (propertyName == L"ExitCode") {
        if (isRunning()) {
            res = ArrayOf::empty();
        } else {
            res = ArrayOf::doubleScalar(static_cast<double>(exitCode()));
        }
    } else if (propertyName == L"BytesAvailable") {
        res = ArrayOf::doubleScalar(static_cast<double>(bytesAvailable()));
    } else if (propertyName == L"Type") {
        res = ArrayOf::characterArray(std::wstring(L"process"));
    } else {
        return false;
    }
    return true;
}
//=============================================================================
bool
ProcessObject::disp(Interface* io)
{
    if (io == nullptr) {
        return false;
    }
    std::wstring runningText = isRunning() ? L"on" : L"off";
    io->outputMessage(L"\n");
    io->outputMessage(L"   Process Object\n\n");
    io->outputMessage(L"            Command: " + command + L"\n");
    io->outputMessage(L"          ProcessId: " + std::to_wstring(processId()) + L"\n");
    io->outputMessage(L"            Running: " + runningText + L"\n");
    if (!isRunning()) {
        io->outputMessage(L"           ExitCode: " + std::to_wstring(exitCode()) + L"\n");
    }
    io->outputMessage(L"     BytesAvailable: " + std::to_wstring(bytesAvailable()) + L"\n");
    return true;
}
//=============================================================================
void
ProcessObject::retainUntilDeleted()
{
    nelson_handle handleId = HandleManager::getInstance()->findByPointerValue(this);
    HandleRef ref = HandleManager::getInstance()->getHandleRef(handleId);
    if (!ref || !ref->isValid()) {
        return;
    }
    std::lock_guard<std::mutex> lock(retainedProcessesMutex);
    auto found = std::find_if(retainedProcesses.begin(), retainedProcesses.end(),
        [this](const HandleRef& current) { return isRetainedProcessRef(current, this); });
    if (found == retainedProcesses.end()) {
        retainedProcesses.push_back(ref);
    }
}
//=============================================================================
void
ProcessObject::releaseRetainedReference()
{
    std::lock_guard<std::mutex> lock(retainedProcessesMutex);
    retainedProcesses.erase(std::remove_if(retainedProcesses.begin(), retainedProcesses.end(),
                                [this](const HandleRef& ref) {
                                    return !ref || !ref->isValid() || ref->nativePointer() == this;
                                }),
        retainedProcesses.end());
}
//=============================================================================
static bool
isRetainedProcessRef(const HandleRef& ref, const ProcessObject* object)
{
    return ref && ref->isValid() && ref->nativePointer() == object;
}
//=============================================================================
#if defined(_WIN32)
static std::wstring
buildWindowsCommandLine(const std::wstring& command, const wstringVector& arguments)
{
    auto quote = [](const std::wstring& token) -> std::wstring {
        if (!token.empty() && token.find_first_of(L" \t\"") == std::wstring::npos) {
            return token;
        }
        std::wstring quoted = L"\"";
        size_t backslashes = 0;
        for (wchar_t ch : token) {
            if (ch == L'\\') {
                backslashes++;
                quoted.push_back(ch);
                continue;
            }
            if (ch == L'"') {
                quoted.append(backslashes + 1, L'\\');
                quoted.push_back(ch);
            } else {
                quoted.push_back(ch);
            }
            backslashes = 0;
        }
        quoted.append(backslashes, L'\\');
        quoted.push_back(L'"');
        return quoted;
    };
    std::wstring line = quote(command);
    for (const std::wstring& argument : arguments) {
        line.push_back(L' ');
        line.append(quote(argument));
    }
    return line;
}
//=============================================================================
static bool
resolveWindowsCommand(const std::wstring& command, std::wstring& resolved)
{
    resolved = command;
    // A path or an explicit extension is used as-is: CreateProcessW appends
    // .exe to a bare name and searches the PATH itself.
    if (command.find_first_of(L"\\/") != std::wstring::npos
        || command.find(L'.') != std::wstring::npos) {
        return false;
    }
    auto splitSemicolon = [](const std::wstring& value) {
        wstringVector parts;
        std::wstring current;
        for (wchar_t ch : value) {
            if (ch == L';') {
                if (!current.empty()) {
                    parts.push_back(current);
                }
                current.clear();
            } else {
                current.push_back(ch);
            }
        }
        if (!current.empty()) {
            parts.push_back(current);
        }
        return parts;
    };
    wchar_t* pathEnv = nullptr;
    size_t pathLength = 0;
    _wdupenv_s(&pathEnv, &pathLength, L"PATH");
    wchar_t* extEnv = nullptr;
    size_t extLength = 0;
    _wdupenv_s(&extEnv, &extLength, L"PATHEXT");
    std::wstring pathValue = (pathEnv != nullptr) ? pathEnv : L"";
    std::wstring pathExtValue = (extEnv != nullptr) ? extEnv : L".COM;.EXE;.BAT;.CMD";
    free(pathEnv);
    free(extEnv);
    wstringVector directories = splitSemicolon(pathValue);
    wstringVector extensions = splitSemicolon(pathExtValue);
    for (const std::wstring& directory : directories) {
        if (directory.empty()) {
            continue;
        }
        for (const std::wstring& extension : extensions) {
            std::wstring candidate = directory;
            wchar_t last = candidate.empty() ? L'\0' : candidate.back();
            if (last != L'\\' && last != L'/') {
                candidate.push_back(L'\\');
            }
            candidate += command + extension;
            DWORD attributes = GetFileAttributesW(candidate.c_str());
            if (attributes != INVALID_FILE_ATTRIBUTES
                && (attributes & FILE_ATTRIBUTE_DIRECTORY) == 0) {
                resolved = candidate;
                std::wstring lowered;
                lowered.reserve(extension.size());
                for (wchar_t ch : extension) {
                    lowered.push_back(static_cast<wchar_t>(std::towlower(ch)));
                }
                return lowered == L".bat" || lowered == L".cmd";
            }
        }
    }
    return false;
}
#elif !defined(__EMSCRIPTEN__)
static std::vector<std::string>
buildPosixArgv(const std::wstring& command, const wstringVector& arguments)
{
    std::vector<std::string> argv;
    argv.push_back(wstring_to_utf8(command));
    for (const std::wstring& argument : arguments) {
        argv.push_back(wstring_to_utf8(argument));
    }
    return argv;
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
