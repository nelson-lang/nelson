//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifndef _MSC_VER
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <crt_externs.h>
#endif
#include <chrono>
#include <cctype>
#include <thread>
#include <vector>
#include "characters_encoding.hpp"
#include "TestChildProcess.hpp"
//=============================================================================
#if !defined(__APPLE__)
extern char** environ;
#endif
//=============================================================================
namespace Nelson::TestsManager {
//=============================================================================
namespace {
    //=============================================================================
    char**
    processEnvironment()
    {
#if defined(__APPLE__)
        return *_NSGetEnviron();
#else
        return environ;
#endif
    }
    //=============================================================================
    void
    closeDescriptor(int& descriptor)
    {
        if (descriptor >= 0) {
            close(descriptor);
        }
        descriptor = -1;
    }
    //=============================================================================
    void
    closePipe(int descriptors[2])
    {
        closeDescriptor(descriptors[0]);
        closeDescriptor(descriptors[1]);
    }
    //=============================================================================
    class ScopedSigpipeBlock
    {
    public:
        ScopedSigpipeBlock()
        {
            sigemptyset(&_signals);
            sigaddset(&_signals, SIGPIPE);
            _active = pthread_sigmask(SIG_BLOCK, &_signals, &_previousMask) == 0;
            if (_active) {
                sigset_t pending;
                sigpending(&pending);
                _wasPending = sigismember(&pending, SIGPIPE) == 1;
            }
        }

        ~ScopedSigpipeBlock()
        {
            if (_active) {
                pthread_sigmask(SIG_SETMASK, &_previousMask, nullptr);
            }
        }

        bool
        active() const
        {
            return _active;
        }

        void
        consumeGeneratedSignal() const
        {
            if (!_active || _wasPending) {
                return;
            }
            sigset_t pending;
            sigpending(&pending);
            if (sigismember(&pending, SIGPIPE) == 1) {
                int received = 0;
                sigwait(&_signals, &received);
            }
        }

    private:
        sigset_t _signals {};
        sigset_t _previousMask {};
        bool _active = false;
        bool _wasPending = false;
    };
    //=============================================================================
    void
    setNonBlocking(int descriptor)
    {
        int flags = fcntl(descriptor, F_GETFL, 0);
        if (flags >= 0) {
            fcntl(descriptor, F_SETFL, flags | O_NONBLOCK);
        }
    }
    //=============================================================================
    void
    readDescriptor(int descriptor, std::string& destination)
    {
        if (descriptor < 0) {
            return;
        }
        char buffer[4096];
        ssize_t count = 0;
        while ((count = read(descriptor, buffer, sizeof(buffer))) > 0) {
            destination.append(buffer, static_cast<size_t>(count));
        }
    }
    //=============================================================================
    std::vector<std::string>
    parseArguments(const std::wstring& arguments)
    {
        std::string input = wstring_to_utf8(arguments);
        std::vector<std::string> values;
        std::string current;
        bool quoted = false;
        bool escaped = false;
        bool started = false;
        for (char character : input) {
            if (escaped) {
                current.push_back(character);
                escaped = false;
                started = true;
            } else if (character == '\\' && quoted) {
                escaped = true;
            } else if (character == '"') {
                quoted = !quoted;
                started = true;
            } else if (std::isspace(static_cast<unsigned char>(character)) && !quoted) {
                if (started) {
                    values.push_back(current);
                    current.clear();
                    started = false;
                }
            } else {
                current.push_back(character);
                started = true;
            }
        }
        if (escaped) {
            current.push_back('\\');
        }
        if (started) {
            values.push_back(current);
        }
        return values;
    }
    //=============================================================================
    int
    statusExitCode(int status)
    {
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
        if (WIFSIGNALED(status)) {
            return 128 + WTERMSIG(status);
        }
        return -1;
    }
    //=============================================================================
} // namespace
//=============================================================================
struct TestChildProcess::Impl
{
    pid_t processId = -1;
    int output = -1;
    int error = -1;
    int input = -1;
    bool active = false;
};
//=============================================================================
TestChildProcess::TestChildProcess() : _impl(std::make_unique<Impl>()) { }
//=============================================================================
TestChildProcess::~TestChildProcess()
{
    if (_impl->active) {
        terminate(0);
    } else if (_impl->processId > 0) {
        kill(-_impl->processId, SIGKILL);
    }
    closeInput();
    closeDescriptor(_impl->output);
    closeDescriptor(_impl->error);
}
//=============================================================================
bool
TestChildProcess::start(const std::wstring& executable, const std::wstring& arguments,
    bool pipeInput, std::wstring& errorText)
{
    int outputPipe[2] { -1, -1 };
    int errorPipe[2] { -1, -1 };
    int inputPipe[2] { -1, -1 };
    if (pipe(outputPipe) != 0) {
        errorText = L"Cannot create process pipes.";
        return false;
    }
    if (pipe(errorPipe) != 0 || (pipeInput && pipe(inputPipe) != 0)) {
        errorText = L"Cannot create process pipes.";
        closePipe(outputPipe);
        closePipe(errorPipe);
        closePipe(inputPipe);
        return false;
    }
    int nullInput = pipeInput ? -1 : open("/dev/null", O_RDONLY);
    if (!pipeInput && nullInput < 0) {
        errorText = L"Cannot open process input.";
        closePipe(outputPipe);
        closePipe(errorPipe);
        return false;
    }
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    posix_spawn_file_actions_adddup2(&actions, outputPipe[1], STDOUT_FILENO);
    posix_spawn_file_actions_adddup2(&actions, errorPipe[1], STDERR_FILENO);
    posix_spawn_file_actions_adddup2(&actions, pipeInput ? inputPipe[0] : nullInput, STDIN_FILENO);
    posix_spawn_file_actions_addclose(&actions, outputPipe[0]);
    posix_spawn_file_actions_addclose(&actions, errorPipe[0]);
    if (outputPipe[1] != STDOUT_FILENO) {
        posix_spawn_file_actions_addclose(&actions, outputPipe[1]);
    }
    if (errorPipe[1] != STDERR_FILENO) {
        posix_spawn_file_actions_addclose(&actions, errorPipe[1]);
    }
    if (pipeInput) {
        posix_spawn_file_actions_addclose(&actions, inputPipe[1]);
        if (inputPipe[0] != STDIN_FILENO) {
            posix_spawn_file_actions_addclose(&actions, inputPipe[0]);
        }
    } else if (nullInput != STDIN_FILENO) {
        posix_spawn_file_actions_addclose(&actions, nullInput);
    }

    posix_spawnattr_t attributes;
    posix_spawnattr_init(&attributes);
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETPGROUP);
    posix_spawnattr_setpgroup(&attributes, 0);
    std::string executableBytes = wstring_to_utf8(executable);
    std::vector<std::string> argumentValues = parseArguments(arguments);
    std::vector<char*> argv;
    argv.reserve(argumentValues.size() + 2);
    argv.push_back(executableBytes.data());
    for (std::string& value : argumentValues) {
        argv.push_back(value.data());
    }
    argv.push_back(nullptr);
    pid_t child = -1;
    int spawnResult = posix_spawnp(
        &child, executableBytes.c_str(), &actions, &attributes, argv.data(), processEnvironment());
    posix_spawnattr_destroy(&attributes);
    posix_spawn_file_actions_destroy(&actions);
    closeDescriptor(outputPipe[1]);
    closeDescriptor(errorPipe[1]);
    closeDescriptor(inputPipe[0]);
    closeDescriptor(nullInput);
    if (spawnResult != 0) {
        closeDescriptor(outputPipe[0]);
        closeDescriptor(errorPipe[0]);
        closeDescriptor(inputPipe[1]);
        errorText = L"Cannot create process. POSIX error: " + std::to_wstring(spawnResult);
        return false;
    }
    _impl->processId = child;
    _impl->output = outputPipe[0];
    _impl->error = errorPipe[0];
    _impl->input = pipeInput ? inputPipe[1] : -1;
    _impl->active = true;
    setNonBlocking(_impl->output);
    setNonBlocking(_impl->error);
    return true;
}
//=============================================================================
bool
TestChildProcess::writeLine(const std::string& value)
{
    if (_impl->input < 0) {
        return false;
    }
    std::string line = value + "\n";
    ScopedSigpipeBlock sigpipeBlock;
    if (!sigpipeBlock.active()) {
        return false;
    }
    size_t offset = 0;
    while (offset < line.size()) {
        ssize_t written = write(_impl->input, line.data() + offset, line.size() - offset);
        if (written < 0 && errno == EINTR) {
            continue;
        }
        if (written <= 0) {
            int writeError = errno;
            if (writeError == EPIPE) {
                sigpipeBlock.consumeGeneratedSignal();
            }
            errno = writeError;
            return false;
        }
        offset += static_cast<size_t>(written);
    }
    return true;
}
//=============================================================================
void
TestChildProcess::closeInput()
{
    closeDescriptor(_impl->input);
}
//=============================================================================
void
TestChildProcess::readAvailable(std::string& output, std::string& errorText)
{
    readDescriptor(_impl->output, output);
    readDescriptor(_impl->error, errorText);
}
//=============================================================================
bool
TestChildProcess::wait(uint64 timeoutMs, int& exitCode)
{
    if (!_impl->active) {
        return true;
    }
    auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeoutMs);
    do {
        int status = 0;
        pid_t result = waitpid(_impl->processId, &status, WNOHANG);
        if (result == _impl->processId) {
            exitCode = statusExitCode(status);
            _impl->active = false;
            return true;
        }
        if (result < 0 && errno != EINTR) {
            _impl->active = false;
            return true;
        }
        if (timeoutMs == 0) {
            break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    } while (std::chrono::steady_clock::now() < deadline);
    return false;
}
//=============================================================================
void
TestChildProcess::terminate(uint64 graceMs)
{
    if (!_impl->active) {
        return;
    }
    kill(-_impl->processId, SIGTERM);
    kill(_impl->processId, SIGTERM);
    int ignored = 0;
    if (wait(graceMs, ignored)) {
        return;
    }
    kill(-_impl->processId, SIGKILL);
    kill(_impl->processId, SIGKILL);
    wait(1000, ignored);
}
//=============================================================================
uint64
TestChildProcess::pid() const
{
    return _impl->processId > 0 ? static_cast<uint64>(_impl->processId) : 0;
}
//=============================================================================
std::wstring
TestChildProcess::policy() const
{
    return L"posix-process-group";
}
//=============================================================================
} // namespace Nelson::TestsManager
//=============================================================================
#endif
//=============================================================================
