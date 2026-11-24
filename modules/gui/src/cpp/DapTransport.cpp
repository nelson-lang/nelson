//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DapTransport.hpp"
#include <cstdio>
#include <cstring>
#include <iostream>
#include <mutex>
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "ws2_32.lib")
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// One lock for the shared stdout: the reader thread and the interpreter thread
// both write frames, and a split header/body would corrupt the stream.
static std::mutex stdioWriteMutex;
//=============================================================================
StdioFrameTransport::StdioFrameTransport()
{
#ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif
}
//=============================================================================
std::string
StdioFrameTransport::readFrame()
{
    std::string line;
    size_t contentLength = 0;
    while (std::getline(std::cin, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (line.empty()) {
            break;
        }
        const std::string prefix = "Content-Length:";
        if (line.rfind(prefix, 0) == 0) {
            try {
                contentLength = static_cast<size_t>(std::stoul(line.substr(prefix.size())));
            } catch (...) {
                contentLength = 0;
            }
        }
    }
    if (contentLength == 0) {
        return {};
    }
    std::string body(contentLength, '\0');
    std::cin.read(&body[0], static_cast<std::streamsize>(contentLength));
    if (!std::cin) {
        return {};
    }
    return body;
}
//=============================================================================
void
StdioFrameTransport::writeFrame(const std::string& body)
{
    std::lock_guard<std::mutex> lock(stdioWriteMutex);
    const std::string header = "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n";
    std::cout.write(header.data(), static_cast<std::streamsize>(header.size()));
    std::cout.write(body.data(), static_cast<std::streamsize>(body.size()));
    std::cout.flush();
}
//=============================================================================
// Platform socket helpers kept local so the header stays free of socket headers.
namespace {
#ifdef _WIN32
    constexpr TcpFrameTransport::socket_t kInvalidSocket = ~0ull; // INVALID_SOCKET
    void
    closeSocket(TcpFrameTransport::socket_t fd)
    {
        if (fd != kInvalidSocket) {
            ::closesocket(static_cast<SOCKET>(fd));
        }
    }
    int
    recvSocket(TcpFrameTransport::socket_t fd, char* buffer, int length)
    {
        return ::recv(static_cast<SOCKET>(fd), buffer, length, 0);
    }
    int
    sendSocket(TcpFrameTransport::socket_t fd, const char* buffer, int length)
    {
        return ::send(static_cast<SOCKET>(fd), buffer, length, 0);
    }
    struct WinsockGuard
    {
        WinsockGuard()
        {
            WSADATA data;
            WSAStartup(MAKEWORD(2, 2), &data);
        }
    };
    // One WSAStartup for the process; WSACleanup is skipped on purpose (the
    // channel lives until exit, like the other native web sockets).
    void
    ensureWinsock()
    {
        static WinsockGuard guard;
        (void)guard;
    }
#else
    constexpr TcpFrameTransport::socket_t kInvalidSocket = -1;
    void
    closeSocket(TcpFrameTransport::socket_t fd)
    {
        if (fd != kInvalidSocket) {
            ::close(fd);
        }
    }
    int
    recvSocket(TcpFrameTransport::socket_t fd, char* buffer, int length)
    {
        return static_cast<int>(::recv(fd, buffer, static_cast<size_t>(length), 0));
    }
    int
    sendSocket(TcpFrameTransport::socket_t fd, const char* buffer, int length)
    {
        return static_cast<int>(::send(fd, buffer, static_cast<size_t>(length), 0));
    }
    void
    ensureWinsock()
    {
    }
#endif
} // namespace
//=============================================================================
TcpFrameTransport::TcpFrameTransport(int port)
    : m_listenFd(kInvalidSocket), m_clientFd(kInvalidSocket)
{
    ensureWinsock();
    m_listenFd = static_cast<socket_t>(::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
    if (m_listenFd == kInvalidSocket) {
        std::fprintf(stderr, "Nelson debug adapter: cannot create socket\n");
        return;
    }
    int reuse = 1;
    ::setsockopt(static_cast<
#ifdef _WIN32
                     SOCKET
#else
                     int
#endif
                     >(m_listenFd),
        SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&reuse), sizeof(reuse));
    sockaddr_in address;
    std::memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_port = htons(static_cast<unsigned short>(port));
    address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (::bind(static_cast<
#ifdef _WIN32
                   SOCKET
#else
                   int
#endif
                   >(m_listenFd),
            reinterpret_cast<sockaddr*>(&address), sizeof(address))
            != 0
        || ::listen(static_cast<
#ifdef _WIN32
                        SOCKET
#else
                        int
#endif
                        >(m_listenFd),
               1)
            != 0) {
        std::fprintf(stderr, "Nelson debug adapter: cannot listen on 127.0.0.1:%d\n", port);
        closeSocket(m_listenFd);
        m_listenFd = kInvalidSocket;
        return;
    }
    m_listening.store(true);
}
//=============================================================================
TcpFrameTransport::~TcpFrameTransport()
{
    closeClient();
    closeSocket(m_listenFd);
    m_listenFd = kInvalidSocket;
    m_listening.store(false);
}
//=============================================================================
bool
TcpFrameTransport::isAlive() const
{
    return m_listening.load();
}
//=============================================================================
bool
TcpFrameTransport::acceptClient()
{
    if (m_listenFd == kInvalidSocket) {
        return false;
    }
    socket_t fd = static_cast<socket_t>(::accept(static_cast<
#ifdef _WIN32
                                                     SOCKET
#else
                                                     int
#endif
                                                     >(m_listenFd),
        nullptr, nullptr));
    if (fd == kInvalidSocket) {
        return false;
    }
    m_clientFd = fd;
    m_rxBuffer.clear();
    return true;
}
//=============================================================================
void
TcpFrameTransport::closeClient()
{
    closeSocket(m_clientFd);
    m_clientFd = kInvalidSocket;
    m_rxBuffer.clear();
}
//=============================================================================
bool
TcpFrameTransport::readLine(std::string& line)
{
    for (;;) {
        const size_t newline = m_rxBuffer.find('\n');
        if (newline != std::string::npos) {
            line = m_rxBuffer.substr(0, newline);
            m_rxBuffer.erase(0, newline + 1);
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            return true;
        }
        char chunk[4096];
        const int got = recvSocket(m_clientFd, chunk, sizeof(chunk));
        if (got <= 0) {
            return false;
        }
        m_rxBuffer.append(chunk, static_cast<size_t>(got));
    }
}
//=============================================================================
bool
TcpFrameTransport::readBytes(size_t n, std::string& out)
{
    while (m_rxBuffer.size() < n) {
        char chunk[4096];
        const int got = recvSocket(m_clientFd, chunk, sizeof(chunk));
        if (got <= 0) {
            return false;
        }
        m_rxBuffer.append(chunk, static_cast<size_t>(got));
    }
    out = m_rxBuffer.substr(0, n);
    m_rxBuffer.erase(0, n);
    return true;
}
//=============================================================================
std::string
TcpFrameTransport::readFrame()
{
    for (;;) {
        if (m_clientFd == kInvalidSocket) {
            if (!acceptClient()) {
                return {};
            }
        }
        std::string line;
        size_t contentLength = 0;
        bool headerOk = true;
        for (;;) {
            if (!readLine(line)) {
                headerOk = false;
                break;
            }
            if (line.empty()) {
                break;
            }
            const std::string prefix = "Content-Length:";
            if (line.rfind(prefix, 0) == 0) {
                try {
                    contentLength = static_cast<size_t>(std::stoul(line.substr(prefix.size())));
                } catch (...) {
                    contentLength = 0;
                }
            }
        }
        if (!headerOk) {
            // Client disconnected: drop it so the next readFrame accepts a new one.
            closeClient();
            return {};
        }
        if (contentLength == 0) {
            continue;
        }
        std::string body;
        if (!readBytes(contentLength, body)) {
            closeClient();
            return {};
        }
        return body;
    }
}
//=============================================================================
void
TcpFrameTransport::writeFrame(const std::string& body)
{
    std::lock_guard<std::mutex> lock(m_writeMutex);
    if (m_clientFd == kInvalidSocket) {
        return;
    }
    const std::string frame = "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n" + body;
    size_t sent = 0;
    while (sent < frame.size()) {
        const int chunk
            = sendSocket(m_clientFd, frame.data() + sent, static_cast<int>(frame.size() - sent));
        if (chunk <= 0) {
            return;
        }
        sent += static_cast<size_t>(chunk);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
