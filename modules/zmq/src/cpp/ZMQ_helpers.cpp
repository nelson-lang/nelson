//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <zmq.h>
#include <vector>
#include "ZMQ_helpers.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void* zmq_context = nullptr;
static std::vector<void*> zmq_sockets;
std::vector<zmq_pollitem_t> zmq_poll_items;
std::vector<std::string> channels;
//=============================================================================
static void
errnoToError(int error)
{
    switch (error) {
    case EINVAL:
        Error(_("The endpoint supplied is invalid."));
        break;
    case EPROTONOSUPPORT:
        Error(_("The requested transport protocol is not supported."));
        break;
    case ENOCOMPATPROTO:
        Error(_("The requested transport protocol is not compatible with the socket type."));
        break;
    case EADDRINUSE:
        Error(_("The requested address is already in use."));
        break;
    case EADDRNOTAVAIL:
        Error(_("The requested address was not local."));
        break;
    case ENODEV:
        Error(_("The requested address specifies a nonexistent interface."));
        break;
    case ETERM:
        Error(_("The ØMQ context associated with the specified socket was terminated."));
        break;
    case ENOTSOCK:
        Error(_("The provided socket was invalid."));
        break;
    case EMTHREAD:
        Error(_("No I/O thread is available to accomplish the task."));
        break;
    default:
        Error(_("Unknown error."));
        break;
    }
}
//=============================================================================
void
zmq_module_init()
{
    zmq_context = zmq_init(1);
}
//=============================================================================
void
zmqSubscribe(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port)
{
    switch (zmqProtocol) {
    case ZMQ_IPC_PROTOCOL: {
        std::wstring zmq_channel = std::wstring(L"ipc:///") + channel;
    } break;
    case ZMQ_TCP_PROTOCOL: {
    } break;
    case ZMQ_PGM_PROTOCOL: {
    } break;
    case ZMQ_ERROR_PROTOCOL:
    default:
        Error(_("Invalid #2 argument: 'ipc', 'tcp', or 'pgm' expected."));
        break;
    }
};
//=============================================================================
int
zmqPublish(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port)
{
    std::wstring zmq_channel;
    if (zmq_context == nullptr) {
        Error(_("zmq not initialized."));
    }
    switch (zmqProtocol) {
    case ZMQ_IPC_PROTOCOL: {
        zmq_channel = std::wstring(L"ipc:///") + channel;
    } break;
    case ZMQ_TCP_PROTOCOL: {
        zmq_channel = std::wstring(L"tcp://*:") + std::to_wstring(port);
    } break;
    case ZMQ_PGM_PROTOCOL: {
        zmq_channel = std::wstring(L"pgm://en0;") + channel + L":" + std::to_wstring(port);
    } break;
    case ZMQ_ERROR_PROTOCOL:
    default:
        Error(_("Invalid #2 argument: 'ipc', 'tcp', or 'pgm' expected."));
        break;
    }
    void* socket = zmq_socket(zmq_context, ZMQ_PUB);
    if (socket == nullptr) {
        Error(_("Cannot not socket."));
    }
    int res = zmq_bind(socket, wstring_to_utf8(zmq_channel).c_str());
    if (res != 0) {
        errnoToError(errno);
    } else {
        zmq_sockets.push_back(socket);
    }

    zmq_pollitem_t zmq_poll_item;
    zmq_poll_item.socket = socket;
    zmq_poll_items.push_back(zmq_poll_item);
    return (int)zmq_poll_items.size() - 1;
 };
//=============================================================================
void
zmqPool(ZMQ_PROTOCOL zmqProtocol)
{
    switch (zmqProtocol) {
    case ZMQ_IPC_PROTOCOL: {
    } break;
    case ZMQ_TCP_PROTOCOL: {
    } break;
    case ZMQ_PGM_PROTOCOL: {
    } break;
    case ZMQ_ERROR_PROTOCOL:
    default:
        Error(_("Invalid #2 argument: 'ipc', 'tcp', or 'pgm' expected."));
        break;
    }
};
//=============================================================================
void
zmqReceive(ZMQ_PROTOCOL zmqProtocol)
{
    switch (zmqProtocol) {
    case ZMQ_IPC_PROTOCOL: {
    } break;
    case ZMQ_TCP_PROTOCOL: {
    } break;
    case ZMQ_PGM_PROTOCOL: {
    } break;
    case ZMQ_ERROR_PROTOCOL:
    default:
        Error(_("Invalid #2 argument: 'ipc', 'tcp', or 'pgm' expected."));
        break;
    }
};
//=============================================================================
}
//=============================================================================
