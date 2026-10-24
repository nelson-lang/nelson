//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Native, dependency-free interprocess transport used between Nelson instances
// on the same machine. A receiver owns one endpoint keyed by its pid (a named
// pipe on Windows, an AF_UNIX stream socket on POSIX); senders connect to that
// pid's endpoint, write one length-framed message and disconnect.
//
// receive() blocks until a full message arrives (no polling). Shutdown is
// cooperative: requestStop() connects to the endpoint and sends an empty
// (zero-length) frame, which wakes a blocked receive() and makes it return
// false, so the receiver loop ends without a hang.
//=============================================================================
class IpcChannelServer
{
public:
    IpcChannelServer() = default;
    ~IpcChannelServer();
    IpcChannelServer(const IpcChannelServer&) = delete;
    IpcChannelServer&
    operator=(const IpcChannelServer&)
        = delete;
    //=============================================================================
    bool
    start(int pid);
    //=============================================================================
    bool
    isValid() const;
    //=============================================================================
    // Block until a full message is received. Returns true with the raw bytes
    // in message; returns false when a stop was requested or on fatal error.
    bool
    receive(std::string& message);
    //=============================================================================
    void
    requestStop(int pid);
    //=============================================================================
    void
    close(int pid);
    //=============================================================================
private:
#ifdef _MSC_VER
    void* pipeHandle = nullptr; // HANDLE
#else
    int listenFd = -1;
#endif
    bool stopRequested = false;
};
//=============================================================================
// Client side: connect to the receiver owning pid, send one framed message and
// close. Returns false if the endpoint does not exist or the write fails.
bool
ipcChannelSend(int pid, const std::string& message);
//=============================================================================
// Directory holding the per-process endpoint files, for stale-endpoint cleanup.
// Empty on Windows, where endpoints are kernel objects with no files to remove.
std::string
ipcChannelEndpointDirectory();
//=============================================================================
} // namespace Nelson
//=============================================================================
