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
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "nlsZmq_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    ZMQ_CMD_SUBSCRIBE = 0,
    ZMQ_CMD_PUBLISH,
    ZMQ_CMD_POLL,
    ZMQ_CMD_RECEIVE,
    ZMQ_CMD_SEND,
    ZMQ_CMD_ERROR_COMMAND
} ZMQ_COMMAND;
//=============================================================================
typedef enum
{
    ZMQ_IPC_PROTOCOL = 0,
    ZMQ_TCP_PROTOCOL,
    ZMQ_PGM_PROTOCOL,
    ZMQ_ERROR_PROTOCOL
} ZMQ_PROTOCOL;
//=============================================================================
NLSZMQ_IMPEXP void
zmq_module_init();
//=============================================================================
NLSZMQ_IMPEXP int
zmqSubscribe(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port);
//=============================================================================
NLSZMQ_IMPEXP int
zmqPublish(ZMQ_PROTOCOL zmqProtocol, std::wstring channel, int port);
//=============================================================================
NLSZMQ_IMPEXP void
zmqPool(ZMQ_PROTOCOL zmqProtocol);
//=============================================================================
NLSZMQ_IMPEXP ArrayOfVector
zmqReceive(int index);
//=============================================================================
NLSZMQ_IMPEXP int
zmqSend(int index, uint8 *data, size_t length);
//=============================================================================
} // namespace Nelson
//=============================================================================
