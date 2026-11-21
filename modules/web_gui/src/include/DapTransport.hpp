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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <atomic>
#include <mutex>
#include <string>
#include "nlsWeb_gui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Transport abstraction for the Debug Adapter server. The DAP wire format is a
// sequence of Content-Length framed JSON messages, identical to the LSP and the
// notebook kernel. Keeping the framing behind this interface lets the same
// DapServer drive a standalone `.m` session over stdio and a notebook session
// over a TCP socket, with no change to the DAP logic itself.
class NLSWEB_GUI_IMPEXP IFrameTransport
{
public:
    virtual ~IFrameTransport() = default;
    // Reads one Content-Length framed message; returns an empty string on EOF or
    // a closed link. A stdio transport treats that as the end of the session; a
    // reconnecting (TCP) transport treats it as a client disconnect.
    virtual std::string
    readFrame()
        = 0;
    // Writes one Content-Length framed message. Implementations serialize writes
    // so frames never interleave.
    virtual void
    writeFrame(const std::string& body)
        = 0;
    // True while the transport can still carry a session. A stdio transport is
    // always alive; a TCP transport is alive while its listening socket is open,
    // so a reader loop can tell a client disconnect (re-accept) from a dead
    // listener (stop).
    virtual bool
    isAlive() const
    {
        return true;
    }
    // A reconnecting transport (TCP) can serve a new client after one leaves, so
    // an empty readFrame is a disconnect, not the end of the whole session.
    virtual bool
    isReconnecting() const
    {
        return false;
    }
};
//=============================================================================
// stdin/stdout framed transport. stdin/stdout are switched to binary so the
// Content-Length byte counts match and \r\n is never doubled on Windows.
class NLSWEB_GUI_IMPEXP StdioFrameTransport : public IFrameTransport
{
public:
    StdioFrameTransport();
    std::string
    readFrame() override;
    void
    writeFrame(const std::string& body) override;
};
//=============================================================================
// TCP framed transport: listens on 127.0.0.1:<port> and serves one Debug
// Adapter client at a time (the notebook kernel's debug channel, alongside its
// stdio cell channel). readFrame() accepts a client when none is connected and
// returns an empty string when the client disconnects; the next readFrame()
// accepts the next client, so an editor can detach and reattach to a running
// kernel. Loopback only, so the channel is never reachable off the machine.
class NLSWEB_GUI_IMPEXP TcpFrameTransport : public IFrameTransport
{
public:
#ifdef _WIN32
    using socket_t = unsigned long long; // SOCKET
#else
    using socket_t = int;
#endif
    explicit TcpFrameTransport(int port);
    ~TcpFrameTransport() override;
    std::string
    readFrame() override;
    void
    writeFrame(const std::string& body) override;
    bool
    isAlive() const override;
    bool
    isReconnecting() const override
    {
        return true;
    }

private:
    // Blocks until a client connects on the listening socket; false if the
    // listener is closed. Resets the receive buffer for the new client.
    bool
    acceptClient();
    void
    closeClient();
    // Reads one line (up to '\n') from the client, trimming a trailing '\r'.
    bool
    readLine(std::string& line);
    // Reads exactly n bytes of body from the client.
    bool
    readBytes(size_t n, std::string& out);

    socket_t m_listenFd;
    socket_t m_clientFd;
    std::string m_rxBuffer;
    std::mutex m_writeMutex;
    std::atomic<bool> m_listening { false };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
