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
#include "zmqBuiltin.hpp"
#include "Error.hpp"
#include "ZMQ_helpers.hpp"
#include "nlsConfig.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ZMQ_COMMAND
arrayOfToZmqCommand(const ArrayOf& input)
{
    std::wstring cmd = input.getContentAsWideString();
    if (cmd.compare(L"subscribe") == 0) {
        return ZMQ_CMD_SUBSCRIBE;
    }
    if (cmd.compare(L"publish") == 0) {
        return ZMQ_CMD_PUBLISH;
    }
    if (cmd.compare(L"poll") == 0) {
        return ZMQ_CMD_POLL;
    }
    if (cmd.compare(L"receive") == 0) {
        return ZMQ_CMD_RECEIVE;
    }
    return ZMQ_CMD_ERROR_COMMAND;
}
//=============================================================================
static ZMQ_PROTOCOL
arrayOfToZmqProtocol(const ArrayOf& input)
{
    std::wstring protocol = input.getContentAsWideString();
    if (protocol.compare(L"ipc") == 0) {
        return ZMQ_IPC_PROTOCOL;
    }
    if (protocol.compare(L"tcp") == 0) {
        return ZMQ_TCP_PROTOCOL;
    }
    if (protocol.compare(L"pgm") == 0) {
        return ZMQ_PGM_PROTOCOL;
    }
    return ZMQ_ERROR_PROTOCOL;
}
//=============================================================================
ArrayOfVector
Nelson::ZmqGateway::zmqBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ZMQ_COMMAND zmqCommand = arrayOfToZmqCommand(argIn[0]);
    ZMQ_PROTOCOL zmqProtocol = arrayOfToZmqProtocol(argIn[1]);
    switch (zmqCommand) {
    case ZMQ_CMD_SUBSCRIBE: {
        std::wstring channel;
        int port = -1;
        if (zmqProtocol == ZMQ_IPC_PROTOCOL) { 
          channel = argIn[2].getContentAsWideString();
        }
        if (zmqProtocol == ZMQ_TCP_PROTOCOL || zmqProtocol == ZMQ_PGM_PROTOCOL) {
            ArrayOf arg3 = argIn[2]; 
            port = arg3.getContentAsInteger32Scalar(false);
        }
        zmqSubscribe(zmqProtocol, channel, port);
    } break;
    case ZMQ_CMD_PUBLISH: {
        std::wstring channel;
        int port = -1;
        if (zmqProtocol == ZMQ_IPC_PROTOCOL) {
            channel = argIn[2].getContentAsWideString();
        }
        if (zmqProtocol == ZMQ_TCP_PROTOCOL || zmqProtocol == ZMQ_PGM_PROTOCOL) {
            ArrayOf arg3 = argIn[2];
            port = arg3.getContentAsInteger32Scalar(false);
        }
        double index = (double)zmqPublish(zmqProtocol, channel, port);
        retval.push_back(ArrayOf::doubleConstructor(index));
    } break;
    case ZMQ_CMD_POLL: {
        zmqPool(zmqProtocol);
    } break;
    case ZMQ_CMD_RECEIVE: {
        zmqReceive(zmqProtocol);
    } break;
    case ZMQ_CMD_ERROR_COMMAND:
    default: {
        Error(_("Invalid #1 argument: 'subscribe', 'publish', 'poll' or 'receive' expected."));
    } break;
    }
    return retval;
}
//=============================================================================
