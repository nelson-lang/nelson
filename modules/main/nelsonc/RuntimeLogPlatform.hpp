//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
#include <cerrno>
#include <cstdio>
#ifdef _WIN32
#include <Windows.h>
#include <fcntl.h>
#include <io.h>
#else
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <unistd.h>
#endif
namespace Nelson::RuntimeLogging {
#ifdef _WIN32
using NativeHandle = HANDLE;
inline NativeHandle
invalidHandle()
{
    return INVALID_HANDLE_VALUE;
}
//=============================================================================
inline bool
valid(NativeHandle h)
{
    return h != nullptr && h != INVALID_HANDLE_VALUE;
}
//=============================================================================
inline void
close(NativeHandle h)
{
    if (valid(h)) {
        CloseHandle(h);
    }
}
//=============================================================================
inline NativeHandle
duplicate(NativeHandle handle)
{
    HANDLE result = INVALID_HANDLE_VALUE;
    if (valid(handle)) {
        ApplicationArchive::require(
            DuplicateHandle(GetCurrentProcess(), handle, GetCurrentProcess(), &result, 0, FALSE,
                DUPLICATE_SAME_ACCESS)
                != 0,
            "Cannot duplicate application output handle.");
    }
    return result;
}
#else
using NativeHandle = int;
inline NativeHandle
invalidHandle()
{
    return -1;
}
//=============================================================================
inline bool
valid(NativeHandle h)
{
    return h >= 0;
}
//=============================================================================
inline void
close(NativeHandle h)
{
    if (valid(h)) {
        ::close(h);
    }
}
//=============================================================================
inline NativeHandle
duplicate(NativeHandle handle)
{
    const auto result = fcntl(handle, F_DUPFD_CLOEXEC, 3);
    ApplicationArchive::require(
        result >= 0 || errno == EBADF, "Cannot duplicate application output handle.");
    return result;
}
#endif
class Handle
{
public:
    Handle() = default;
    explicit Handle(NativeHandle value) : value(value) { }
    ~Handle() { close(value); }
    Handle(const Handle&) = delete;
    Handle&
    operator=(const Handle&)
        = delete;
    NativeHandle
    get() const
    {
        return value;
    }
    void
    reset(NativeHandle next = invalidHandle())
    {
        close(value);
        value = next;
    }
    NativeHandle
    release()
    {
        const auto result = value;
        value = invalidHandle();
        return result;
    }

private:
    NativeHandle value = invalidHandle();
};
//=============================================================================
inline bool
writeAll(NativeHandle output, const char* bytes, size_t length)
{
    while (length > 0) {
#ifdef _WIN32
        DWORD count = 0;
        if (!WriteFile(output, bytes, static_cast<DWORD>(length), &count, nullptr) || count == 0) {
            return false;
        }
#else
        const auto count = ::write(output, bytes, length);
        if (count < 0 && errno == EINTR) {
            continue;
        }
        if (count <= 0) {
            return false;
        }
#endif
        bytes += count;
        length -= count;
    }
    return true;
}
//=============================================================================
inline size_t
completeUtf8Prefix(const char* bytes, size_t length)
{
    if (!length) {
        return 0;
    }
    size_t start = length - 1;
    while (start > 0 && length - start < 4
        && (static_cast<unsigned char>(bytes[start]) & 0xc0) == 0x80) {
        --start;
    }
    const auto lead = static_cast<unsigned char>(bytes[start]);
    const size_t expected = lead >= 0xf0 && lead <= 0xf4 ? 4
        : lead >= 0xe0 && lead <= 0xef                   ? 3
        : lead >= 0xc2 && lead <= 0xdf                   ? 2
                                                         : 1;
    return length - start < expected ? start : length;
}
//=============================================================================
inline NativeHandle
openLog(const std::filesystem::path& path, const std::filesystem::path& executable)
{
    using ApplicationArchive::require;
    std::error_code error;
    require(!std::filesystem::equivalent(path, executable, error),
        "The application executable cannot be used as its runtime log.");
#ifdef _WIN32
    Handle handle(CreateFileW(path.c_str(), FILE_APPEND_DATA | FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr));
    require(valid(handle.get()),
        "Cannot open runtime log; its parent directory must exist and be writable.");
    BY_HANDLE_FILE_INFORMATION information {};
    require(GetFileType(handle.get()) == FILE_TYPE_DISK
            && GetFileInformationByHandle(handle.get(), &information)
            && (information.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0,
        "Runtime log must be a regular file.");
#else
    Handle handle(
        ::open(path.c_str(), O_WRONLY | O_APPEND | O_CREAT | O_CLOEXEC | O_NONBLOCK, 0600));
    require(valid(handle.get()),
        "Cannot open runtime log; its parent directory must exist and be writable.");
    struct stat information {}, source {};
    require(fstat(handle.get(), &information) == 0 && S_ISREG(information.st_mode),
        "Runtime log must be a regular file.");
    require(stat(executable.c_str(), &source) == 0
            && (source.st_dev != information.st_dev || source.st_ino != information.st_ino),
        "The application executable cannot be used as its runtime log.");
#endif
    return handle.release();
}
//=============================================================================
inline void
makePipe(Handle& reader, Handle& writer)
{
#ifdef _WIN32
    HANDLE input = nullptr, output = nullptr;
    ApplicationArchive::require(
        CreatePipe(&input, &output, nullptr, 65536) != 0, "Cannot create runtime log pipe.");
    reader.reset(input);
    writer.reset(output);
#else
    int descriptors[2];
    ApplicationArchive::require(pipe(descriptors) == 0, "Cannot create runtime log pipe.");
    reader.reset(descriptors[0]);
    writer.reset(descriptors[1]);
    ApplicationArchive::require(fcntl(descriptors[0], F_SETFL, O_NONBLOCK) == 0
            && fcntl(descriptors[0], F_SETFD, FD_CLOEXEC) == 0
            && fcntl(descriptors[1], F_SETFD, FD_CLOEXEC) == 0,
        "Cannot configure runtime log pipe.");
#endif
}
//=============================================================================
inline int
availableBytes(NativeHandle input)
{
#ifdef _WIN32
    DWORD count = 0;
    return PeekNamedPipe(input, nullptr, 0, nullptr, &count, nullptr) ? static_cast<int>(count) : 0;
#else
    int count = 0;
    return ioctl(input, FIONREAD, &count) == 0 ? count : 0;
#endif
}
//=============================================================================
inline int
readAvailable(NativeHandle input, char* data, size_t length)
{
#ifdef _WIN32
    DWORD available = 0;
    if (!PeekNamedPipe(input, nullptr, 0, nullptr, &available, nullptr)) {
        return -1;
    }
    if (available == 0) {
        return 0;
    }
    DWORD count = 0;
    if (!ReadFile(input, data,
            static_cast<DWORD>((std::min)(length, static_cast<size_t>(available))), &count,
            nullptr)) {
        return -1;
    }
    return static_cast<int>(count);
#else
    const auto count = ::read(input, data, length);
    if (count < 0 && (errno == EAGAIN || errno == EINTR)) {
        return 0;
    }
    return count == 0 ? -1 : static_cast<int>(count);
#endif
}
//=============================================================================
} // namespace Nelson::RuntimeLogging
//=============================================================================
