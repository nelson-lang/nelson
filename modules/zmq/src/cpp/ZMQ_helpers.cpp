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
void
zmq_module_init()
{
    zmq_context = zmq_init(1);
}
//=============================================================================
static int
zmqRegister(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port, int type)
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
    void* socket = zmq_socket(zmq_context, type);
    if (socket == nullptr) {
        Error(_("Cannot not socket."));
    }
    int res = zmq_bind(socket, wstring_to_utf8(zmq_channel).c_str());
    if (res != 0) {
        Error(_(zmq_strerror(errno)));
    } else {
        zmq_sockets.push_back(socket);
    }

    zmq_pollitem_t zmq_poll_item;
    zmq_poll_item.socket = socket;
    zmq_poll_items.push_back(zmq_poll_item);
    return (int)zmq_poll_items.size() - 1;
}
//=============================================================================
int
zmqSubscribe(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port)
{
    return zmqRegister(zmqProtocol, channel, port, ZMQ_SUB);
};
//=============================================================================
int
zmqPublish(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port)
{
    return zmqRegister(zmqProtocol, channel, port, ZMQ_PUB);
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
ArrayOfVector
zmqReceive(int index)
{
    ArrayOfVector res;
    if (zmq_poll_items[index].socket == nullptr) {
        res.push_back(ArrayOf::doubleConstructor((double)zmq_poll_items[index].fd));
        res.push_back(ArrayOf::doubleConstructor(0));
        return res;
    }

#define BUFLEN 1024 * 1024
    char* recv_buffer = nullptr;
    recv_buffer = (char*)malloc(BUFLEN);

    int nbytes = zmq_recv(zmq_sockets[index], recv_buffer, BUFLEN, 0);
    if (nbytes == -1) {
        if (recv_buffer) {
            free(recv_buffer);
            recv_buffer = nullptr;
        }
        Error(_("Do not receive anything."));
    }
    int has_more;
    size_t has_more_size = sizeof(has_more);
    int rc = zmq_getsockopt(zmq_sockets[index], ZMQ_RCVMORE, &has_more, &has_more_size);
    if (rc != 0) {
        if (recv_buffer) {
            free(recv_buffer);
            recv_buffer = nullptr;
        }
        Error(_("Do not receive more."));
    }
    uint8* ptrValue = (uint8* )ArrayOf::allocateArrayOf(NLS_UINT8, nbytes);
    Dimensions dimsValue(1, nbytes);
    ArrayOf value = ArrayOf::ArrayOf(NLS_UINT8, dimsValue, ptrValue);
    memcpy(ptrValue, recv_buffer, nbytes);
    if (recv_buffer) {
        free(recv_buffer);
        recv_buffer = nullptr;
    }
    res.push_back(value);
    res.push_back(ArrayOf::logicalConstructor(has_more));
    return res;
};
//=============================================================================
int
zmqSend(int index, uint8* data, size_t length)
{
    int nbytes = zmq_send(zmq_sockets[index], data, (int)length, 0);
    if (nbytes != (int)length) {
        Error(_("Send incorrect number of bytes."));
    }
    return nbytes;
};
//=============================================================================
}
//=============================================================================
