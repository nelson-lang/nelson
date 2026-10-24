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
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#endif
//=============================================================================
#include <cstdint>
#include <cstdio>
#include <string>
#ifdef _MSC_VER
#include <windows.h>
#else
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>
#include <cerrno>
#include <cstring>
#endif
#include "IpcChannel.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Sanity bound on a single message so a corrupt length never triggers a huge
// allocation. Well above any real serialized workspace variable.
static constexpr uint32_t IPC_CHANNEL_MAX_MESSAGE = 256u * 1024u * 1024u;
//=============================================================================
#ifdef _MSC_VER
static std::string
endpointName(int pid);
static bool
readExact(HANDLE handle, void* buffer, size_t count);
static bool
writeAll(HANDLE handle, const void* buffer, size_t count);
#else
static std::string
endpointDirectory();
static std::string
endpointPath(int pid);
static bool
readExact(int fd, void* buffer, size_t count);
static bool
writeAll(int fd, const void* buffer, size_t count);
#endif
//=============================================================================
IpcChannelServer::~IpcChannelServer() { close(0); }
//=============================================================================
#ifdef _MSC_VER
//=============================================================================
bool
IpcChannelServer::start(int pid)
{
    const std::string name = endpointName(pid);
    HANDLE handle = CreateNamedPipeA(name.c_str(), PIPE_ACCESS_DUPLEX,
        PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1, 64 * 1024, 64 * 1024, 0, nullptr);
    if (handle == INVALID_HANDLE_VALUE) {
        pipeHandle = nullptr;
        return false;
    }
    pipeHandle = handle;
    stopRequested = false;
    return true;
}
//=============================================================================
bool
IpcChannelServer::isValid() const
{
    return pipeHandle != nullptr;
}
//=============================================================================
bool
IpcChannelServer::receive(std::string& message)
{
    message.clear();
    if (pipeHandle == nullptr) {
        return false;
    }
    HANDLE handle = (HANDLE)pipeHandle;
    BOOL connected = ConnectNamedPipe(handle, nullptr);
    if (!connected && GetLastError() != ERROR_PIPE_CONNECTED) {
        return false;
    }
    bool ok = true;
    uint32_t length = 0;
    if (!readExact(handle, &length, sizeof(length))) {
        ok = false;
    }
    if (ok && length > 0 && length <= IPC_CHANNEL_MAX_MESSAGE) {
        message.resize(length);
        if (!readExact(handle, &message[0], length)) {
            message.clear();
            ok = false;
        }
    } else if (length > IPC_CHANNEL_MAX_MESSAGE) {
        ok = false;
    }
    DisconnectNamedPipe(handle);
    if (stopRequested || length == 0 || !ok) {
        return false;
    }
    return true;
}
//=============================================================================
void
IpcChannelServer::requestStop(int pid)
{
    stopRequested = true;
    const std::string name = endpointName(pid);
    HANDLE client = CreateFileA(name.c_str(), GENERIC_WRITE, 0, nullptr, OPEN_EXISTING, 0, nullptr);
    if (client == INVALID_HANDLE_VALUE) {
        return;
    }
    uint32_t zero = 0;
    writeAll(client, &zero, sizeof(zero));
    CloseHandle(client);
}
//=============================================================================
void
IpcChannelServer::close(int /*pid*/)
{
    if (pipeHandle != nullptr) {
        CloseHandle((HANDLE)pipeHandle);
        pipeHandle = nullptr;
    }
}
//=============================================================================
bool
ipcChannelSend(int pid, const std::string& message)
{
    const std::string name = endpointName(pid);
    HANDLE client = INVALID_HANDLE_VALUE;
    for (int attempt = 0; attempt < 10; ++attempt) {
        client = CreateFileA(name.c_str(), GENERIC_WRITE, 0, nullptr, OPEN_EXISTING, 0, nullptr);
        if (client != INVALID_HANDLE_VALUE) {
            break;
        }
        if (GetLastError() != ERROR_PIPE_BUSY) {
            return false;
        }
        if (!WaitNamedPipeA(name.c_str(), 2000)) {
            return false;
        }
    }
    if (client == INVALID_HANDLE_VALUE) {
        return false;
    }
    uint32_t length = (uint32_t)message.size();
    bool ok = writeAll(client, &length, sizeof(length));
    if (ok && length > 0) {
        ok = writeAll(client, message.data(), message.size());
    }
    CloseHandle(client);
    return ok;
}
//=============================================================================
#else
//=============================================================================
bool
IpcChannelServer::start(int pid)
{
    const std::string path = endpointPath(pid);
    if (path.empty()) {
        return false;
    }
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) {
        return false;
    }
    ::unlink(path.c_str());
    struct sockaddr_un address;
    std::memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    std::strncpy(address.sun_path, path.c_str(), sizeof(address.sun_path) - 1);
    if (bind(fd, (struct sockaddr*)&address, sizeof(address)) != 0 || listen(fd, 8) != 0) {
        ::close(fd);
        return false;
    }
    listenFd = fd;
    stopRequested = false;
    return true;
}
//=============================================================================
bool
IpcChannelServer::isValid() const
{
    return listenFd >= 0;
}
//=============================================================================
bool
IpcChannelServer::receive(std::string& message)
{
    message.clear();
    if (listenFd < 0) {
        return false;
    }
    int conn = accept(listenFd, nullptr, nullptr);
    if (conn < 0) {
        return false;
    }
    bool ok = true;
    uint32_t length = 0;
    if (!readExact(conn, &length, sizeof(length))) {
        ok = false;
    }
    if (ok && length > 0 && length <= IPC_CHANNEL_MAX_MESSAGE) {
        message.resize(length);
        if (!readExact(conn, &message[0], length)) {
            message.clear();
            ok = false;
        }
    } else if (length > IPC_CHANNEL_MAX_MESSAGE) {
        ok = false;
    }
    ::close(conn);
    if (stopRequested || length == 0 || !ok) {
        return false;
    }
    return true;
}
//=============================================================================
void
IpcChannelServer::requestStop(int pid)
{
    stopRequested = true;
    const std::string path = endpointPath(pid);
    int client = socket(AF_UNIX, SOCK_STREAM, 0);
    if (client < 0) {
        return;
    }
    struct sockaddr_un address;
    std::memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    std::strncpy(address.sun_path, path.c_str(), sizeof(address.sun_path) - 1);
    if (connect(client, (struct sockaddr*)&address, sizeof(address)) == 0) {
        uint32_t zero = 0;
        writeAll(client, &zero, sizeof(zero));
    }
    ::close(client);
}
//=============================================================================
void
IpcChannelServer::close(int pid)
{
    if (listenFd >= 0) {
        ::close(listenFd);
        listenFd = -1;
        const std::string path = endpointPath(pid);
        if (!path.empty()) {
            ::unlink(path.c_str());
        }
    }
}
//=============================================================================
bool
ipcChannelSend(int pid, const std::string& message)
{
    const std::string path = endpointPath(pid);
    if (path.empty()) {
        return false;
    }
    int client = socket(AF_UNIX, SOCK_STREAM, 0);
    if (client < 0) {
        return false;
    }
    struct sockaddr_un address;
    std::memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    std::strncpy(address.sun_path, path.c_str(), sizeof(address.sun_path) - 1);
    bool ok = connect(client, (struct sockaddr*)&address, sizeof(address)) == 0;
    if (ok) {
        uint32_t length = (uint32_t)message.size();
        ok = writeAll(client, &length, sizeof(length));
        if (ok && length > 0) {
            ok = writeAll(client, message.data(), message.size());
        }
    }
    ::close(client);
    return ok;
}
//=============================================================================
#endif
//=============================================================================
std::string
ipcChannelEndpointDirectory()
{
#ifdef _MSC_VER
    return {};
#else
    return endpointDirectory();
#endif
}
//=============================================================================
#ifdef _MSC_VER
//=============================================================================
static std::string
endpointName(int pid)
{
    char buffer[64];
    std::snprintf(buffer, sizeof(buffer), "\\\\.\\pipe\\nelson_ipc_%d", pid);
    return std::string(buffer);
}
//=============================================================================
static bool
readExact(HANDLE handle, void* buffer, size_t count)
{
    char* out = (char*)buffer;
    size_t done = 0;
    while (done < count) {
        DWORD read = 0;
        if (!ReadFile(handle, out + done, (DWORD)(count - done), &read, nullptr) || read == 0) {
            return false;
        }
        done += read;
    }
    return true;
}
//=============================================================================
static bool
writeAll(HANDLE handle, const void* buffer, size_t count)
{
    const char* in = (const char*)buffer;
    size_t done = 0;
    while (done < count) {
        DWORD written = 0;
        if (!WriteFile(handle, in + done, (DWORD)(count - done), &written, nullptr)
            || written == 0) {
            return false;
        }
        done += written;
    }
    return true;
}
//=============================================================================
#else
//=============================================================================
static std::string
endpointDirectory()
{
    const char* runtimeDir = getenv("XDG_RUNTIME_DIR");
    std::string directory;
    if (runtimeDir != nullptr && runtimeDir[0] != '\0') {
        directory = std::string(runtimeDir) + "/nelson";
    } else {
        char buffer[64];
        std::snprintf(buffer, sizeof(buffer), "/tmp/nelson-%d", (int)geteuid());
        directory = buffer;
    }
    mkdir(directory.c_str(), 0700);
    return directory;
}
//=============================================================================
static std::string
endpointPath(int pid)
{
    const std::string directory = endpointDirectory();
    if (directory.empty()) {
        return {};
    }
    return directory + "/ipc_" + std::to_string(pid) + ".sock";
}
//=============================================================================
static bool
readExact(int fd, void* buffer, size_t count)
{
    char* out = (char*)buffer;
    size_t done = 0;
    while (done < count) {
        ssize_t read = ::read(fd, out + done, count - done);
        if (read <= 0) {
            if (read < 0 && errno == EINTR) {
                continue;
            }
            return false;
        }
        done += (size_t)read;
    }
    return true;
}
//=============================================================================
static bool
writeAll(int fd, const void* buffer, size_t count)
{
    const char* in = (const char*)buffer;
    size_t done = 0;
    while (done < count) {
        ssize_t written = ::write(fd, in + done, count - done);
        if (written <= 0) {
            if (written < 0 && errno == EINTR) {
                continue;
            }
            return false;
        }
        done += (size_t)written;
    }
    return true;
}
//=============================================================================
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
