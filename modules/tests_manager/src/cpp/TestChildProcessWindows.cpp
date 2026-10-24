//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <algorithm>
#include <vector>
#include "TestChildProcess.hpp"
//=============================================================================
namespace Nelson::TestsManager {
//=============================================================================
namespace {
    //=============================================================================
    std::wstring
    quoteArgument(const std::wstring& value)
    {
        if (value.empty()) {
            return L"\"\"";
        }
        if (value.find_first_of(L" \t\n\v\"") == std::wstring::npos) {
            return value;
        }
        std::wstring result = L"\"";
        size_t backslashes = 0;
        for (wchar_t character : value) {
            if (character == L'\\') {
                ++backslashes;
            } else if (character == L'\"') {
                result.append(backslashes * 2 + 1, L'\\');
                result.push_back(character);
                backslashes = 0;
            } else {
                result.append(backslashes, L'\\');
                backslashes = 0;
                result.push_back(character);
            }
        }
        result.append(backslashes * 2, L'\\');
        result.push_back(L'\"');
        return result;
    }
    //=============================================================================
    bool
    needsPathLookup(const std::wstring& executable)
    {
        return executable.find(L'\\') == std::wstring::npos
            && executable.find(L'/') == std::wstring::npos
            && executable.find(L':') == std::wstring::npos;
    }
    //=============================================================================
    void
    closeHandle(HANDLE& handle)
    {
        if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
            CloseHandle(handle);
        }
        handle = nullptr;
    }
    //=============================================================================
    class InheritGuard
    {
    public:
        InheritGuard()
        {
            _mutex
                = CreateMutexW(nullptr, FALSE, L"Local\\NelsonUnittestCreateProcessInheritMutex");
            if (_mutex != nullptr) {
                WaitForSingleObject(_mutex, INFINITE);
            }
        }

        ~InheritGuard()
        {
            if (_mutex != nullptr) {
                ReleaseMutex(_mutex);
                CloseHandle(_mutex);
            }
        }

    private:
        HANDLE _mutex = nullptr;
    };
    //=============================================================================
    class AttributeList
    {
    public:
        explicit AttributeList(const std::vector<HANDLE>& handles)
        {
            SIZE_T size = 0;
            InitializeProcThreadAttributeList(nullptr, 1, 0, &size);
            _list = reinterpret_cast<PPROC_THREAD_ATTRIBUTE_LIST>(
                HeapAlloc(GetProcessHeap(), 0, size));
            if (_list == nullptr || !InitializeProcThreadAttributeList(_list, 1, 0, &size)) {
                release(false);
                return;
            }
            _initialized = true;
            if (!UpdateProcThreadAttribute(_list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                    const_cast<HANDLE*>(handles.data()), handles.size() * sizeof(HANDLE), nullptr,
                    nullptr)) {
                release(true);
            }
        }

        ~AttributeList() { release(_initialized); }
        PPROC_THREAD_ATTRIBUTE_LIST
        get() const { return _list; }
        bool
        valid() const
        {
            return _list != nullptr;
        }

    private:
        void
        release(bool initialized)
        {
            if (_list != nullptr) {
                if (initialized) {
                    DeleteProcThreadAttributeList(_list);
                }
                HeapFree(GetProcessHeap(), 0, _list);
            }
            _list = nullptr;
            _initialized = false;
        }

        PPROC_THREAD_ATTRIBUTE_LIST _list = nullptr;
        bool _initialized = false;
    };
    //=============================================================================
    void
    readPipe(HANDLE pipe, std::string& destination)
    {
        if (pipe == nullptr) {
            return;
        }
        DWORD available = 0;
        char buffer[4096];
        while (PeekNamedPipe(pipe, nullptr, 0, nullptr, &available, nullptr) && available > 0) {
            DWORD count = (std::min)(available, static_cast<DWORD>(sizeof(buffer)));
            DWORD read = 0;
            if (!ReadFile(pipe, buffer, count, &read, nullptr) || read == 0) {
                break;
            }
            destination.append(buffer, read);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
struct TestChildProcess::Impl
{
    HANDLE process = nullptr;
    HANDLE job = nullptr;
    HANDLE output = nullptr;
    HANDLE error = nullptr;
    HANDLE input = nullptr;
    uint64 processId = 0;
    std::wstring processPolicy = L"windows-process";
    bool active = false;
};
//=============================================================================
TestChildProcess::TestChildProcess() : _impl(std::make_unique<Impl>()) { }
//=============================================================================
TestChildProcess::~TestChildProcess()
{
    if (_impl->active) {
        terminate(0);
    }
    closeInput();
    closeHandle(_impl->output);
    closeHandle(_impl->error);
    closeHandle(_impl->process);
    closeHandle(_impl->job);
}
//=============================================================================
bool
TestChildProcess::start(const std::wstring& executable, const std::wstring& arguments,
    bool pipeInput, std::wstring& errorText)
{
    InheritGuard guard;
    SECURITY_ATTRIBUTES security {};
    security.nLength = sizeof(security);
    security.bInheritHandle = TRUE;
    HANDLE outputWrite = nullptr;
    HANDLE errorWrite = nullptr;
    HANDLE inputRead = nullptr;
    if (!CreatePipe(&_impl->output, &outputWrite, &security, 0)
        || !CreatePipe(&_impl->error, &errorWrite, &security, 0)) {
        errorText = L"Cannot create process output pipes.";
        closeHandle(outputWrite);
        closeHandle(errorWrite);
        return false;
    }
    SetHandleInformation(_impl->output, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(_impl->error, HANDLE_FLAG_INHERIT, 0);
    if (pipeInput) {
        if (!CreatePipe(&inputRead, &_impl->input, &security, 0)) {
            errorText = L"Cannot create process input pipe.";
            closeHandle(outputWrite);
            closeHandle(errorWrite);
            return false;
        }
        SetHandleInformation(_impl->input, HANDLE_FLAG_INHERIT, 0);
    } else {
        inputRead = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ, nullptr, OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL, nullptr);
        if (inputRead != INVALID_HANDLE_VALUE) {
            SetHandleInformation(inputRead, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
        }
    }

    _impl->job = CreateJobObjectW(nullptr, nullptr);
    if (_impl->job != nullptr) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION information {};
        information.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        if (!SetInformationJobObject(
                _impl->job, JobObjectExtendedLimitInformation, &information, sizeof(information))) {
            closeHandle(_impl->job);
        }
    }

    STARTUPINFOEXW startup {};
    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdOutput = outputWrite;
    startup.StartupInfo.hStdError = errorWrite;
    startup.StartupInfo.hStdInput = inputRead;
    std::vector<HANDLE> inherited { outputWrite, errorWrite, inputRead };
    AttributeList attributes(inherited);
    DWORD flags = CREATE_NEW_PROCESS_GROUP;
    if (_impl->job != nullptr) {
        flags |= CREATE_SUSPENDED;
    }
    if (attributes.valid()) {
        startup.lpAttributeList = attributes.get();
        flags |= EXTENDED_STARTUPINFO_PRESENT;
    } else {
        startup.StartupInfo.cb = sizeof(STARTUPINFOW);
    }
    std::wstring command = quoteArgument(executable);
    if (!arguments.empty()) {
        command += L" " + arguments;
    }
    PROCESS_INFORMATION processInfo {};
    LPCWSTR application = needsPathLookup(executable) ? nullptr : executable.c_str();
    LPSTARTUPINFOW startupInfo = &startup.StartupInfo;
    BOOL created = CreateProcessW(application, command.data(), nullptr, nullptr, TRUE, flags,
        nullptr, nullptr, startupInfo, &processInfo);
    closeHandle(outputWrite);
    closeHandle(errorWrite);
    closeHandle(inputRead);
    if (!created) {
        errorText = L"Cannot create process. Windows error: " + std::to_wstring(GetLastError());
        return false;
    }
    _impl->process = processInfo.hProcess;
    _impl->processId = static_cast<uint64>(processInfo.dwProcessId);
    if (_impl->job != nullptr && !AssignProcessToJobObject(_impl->job, processInfo.hProcess)) {
        DWORD assignmentError = GetLastError();
        closeHandle(_impl->job);
        _impl->processPolicy = L"windows-process:" + std::to_wstring(assignmentError);
    } else if (_impl->job != nullptr) {
        _impl->processPolicy = L"windows-job-object";
    }
    if ((flags & CREATE_SUSPENDED) != 0) {
        ResumeThread(processInfo.hThread);
    }
    CloseHandle(processInfo.hThread);
    _impl->active = true;
    return true;
}
//=============================================================================
bool
TestChildProcess::writeLine(const std::string& value)
{
    if (_impl->input == nullptr) {
        return false;
    }
    std::string line = value + "\n";
    size_t offset = 0;
    while (offset < line.size()) {
        DWORD written = 0;
        DWORD remaining
            = static_cast<DWORD>((std::min)(line.size() - offset, static_cast<size_t>(MAXDWORD)));
        if (!WriteFile(_impl->input, line.data() + offset, remaining, &written, nullptr)
            || written == 0) {
            return false;
        }
        offset += written;
    }
    return true;
}
//=============================================================================
void
TestChildProcess::closeInput()
{
    closeHandle(_impl->input);
}
//=============================================================================
void
TestChildProcess::readAvailable(std::string& output, std::string& errorText)
{
    readPipe(_impl->output, output);
    readPipe(_impl->error, errorText);
}
//=============================================================================
bool
TestChildProcess::wait(uint64 timeoutMs, int& exitCode)
{
    if (_impl->process == nullptr) {
        return true;
    }
    DWORD timeout = timeoutMs > MAXDWORD ? MAXDWORD : static_cast<DWORD>(timeoutMs);
    if (WaitForSingleObject(_impl->process, timeout) != WAIT_OBJECT_0) {
        return false;
    }
    DWORD code = 0;
    if (GetExitCodeProcess(_impl->process, &code)) {
        exitCode = static_cast<int>(code);
    }
    _impl->active = false;
    return true;
}
//=============================================================================
void
TestChildProcess::terminate(uint64 graceMs)
{
    if (!_impl->active) {
        return;
    }
    int ignored = 0;
    if (graceMs > 0 && wait(graceMs, ignored)) {
        return;
    }
    if (_impl->job != nullptr) {
        TerminateJobObject(_impl->job, 258);
    } else if (_impl->process != nullptr) {
        TerminateProcess(_impl->process, 258);
    }
    wait(1000, ignored);
}
//=============================================================================
uint64
TestChildProcess::pid() const
{
    return _impl->processId;
}
//=============================================================================
std::wstring
TestChildProcess::policy() const
{
    return _impl->processPolicy;
}
//=============================================================================
} // namespace Nelson::TestsManager
//=============================================================================
#endif
//=============================================================================
