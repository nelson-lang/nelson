//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <sstream>
#include <sio_message.h>
#include "SioClientCommand.hpp"
#include "characters_encoding.hpp"
#include "SioClientListener.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
SioClientCommand* SioClientCommand::m_pInstance = nullptr;
sio::socket::ptr SioClientCommand::_socket = nullptr;
//=============================================================================
bool
SioClientCommand::create(const std::string& ipAddress)
{
    if (!_initialized) {
        if (!ipAddress.empty()) {
            return createConnection(ipAddress);
        }
    }
    return false;
}
//=============================================================================
bool
SioClientCommand::isInitialized()
{
    return _initialized;
}
//=============================================================================
SioClientCommand::SioClientCommand() { _initialized = false; }
//=============================================================================
bool
SioClientCommand::createConnection(const std::string& ipAddress)
{
    if (!ipAddress.empty()) {
        _command.clear();
        _sioClient.set_open_listener(
            std::bind(&SioClientListener::on_connected, &_sioClientListener));
        _sioClient.set_close_listener(
            std::bind(&SioClientListener::on_close, &_sioClientListener, std::placeholders::_1));
        _sioClient.set_fail_listener(std::bind(&SioClientListener::on_fail, &_sioClientListener));
        _ipAddress = ipAddress;
        _sioClient.connect(_ipAddress);

        _sioClientListener.lock.lock();
        if (!_sioClientListener.connection_finish) {
            _sioClientListener.condition.wait(_sioClientListener.lock);
        }

        _sioClientListener.lock.unlock();
        _socket = _sioClient.socket();

        _socket->on("command",
            sio::socket::event_listener_aux(
                [&](std::string const& name, sio::message::ptr const& data, bool isAck,
                    sio::message::list& ack_resp) {
                    _sioClientListener.lock.lock();
                    _command = data->get_map()["data"]->get_string();
                    _sioClientListener.lock.unlock();
                    _socket->emit("command_received");
                }));

        _socket->on("stop",
            sio::socket::event_listener_aux(
                [&](std::string const& name, sio::message::ptr const& data, bool isAck,
                    sio::message::list& ack_resp) {
                    NelsonConfiguration::getInstance()->setInterruptPending(true, 0);
                }));
        _initialized = true;
        return true;
    }
    return false;
}
//=============================================================================
SioClientCommand*
SioClientCommand::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new SioClientCommand();
        } catch (std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
SioClientCommand::reply(const std::string& stringToReply)
{
    std::stringstream output;
    output << stringToReply;
    sio::message::ptr send_data(sio::object_message::create());
    std::map<std::string, sio::message::ptr>& map = send_data->get_map();
    map.insert(std::make_pair("output", sio::string_message::create(output.str())));
    _socket->emit("reply", send_data);
}
//=============================================================================
void
SioClientCommand::clc()
{
    _socket->emit("clc");
}
//=============================================================================
void
SioClientCommand::available()
{
    _socket->emit("available");
}
//=============================================================================
void
SioClientCommand::unavailable()
{
    _socket->emit("unavailable");
}
//=============================================================================
std::string
SioClientCommand::getCommand()
{
    std::string returnedCommand = _command;
    _command.clear();
    return returnedCommand;
}
//=============================================================================
void
SioClientCommand::updateCommand(const std::string& newCommand)
{
    _command = newCommand;
}
//=============================================================================
void
SioClientCommand::sioemit(const std::string& name, const std::string& message)
{
    sio::message::ptr send_data(sio::object_message::create());
    std::map<std::string, sio::message::ptr>& map = send_data->get_map();
    map.insert(std::make_pair("name", sio::string_message::create(name)));
    map.insert(std::make_pair("message", sio::string_message::create(message)));
    _socket->emit("sioemit", send_data);
}
//=============================================================================
void
SioClientCommand::sioregister(const std::string& name, const std::string& function_name)
{
    _socket->on(name,
        sio::socket::event_listener_aux([&](std::string const& name, sio::message::ptr const& data,
                                            bool isAck, sio::message::list& ack_resp) {
            std::string _data = data->get_map()["data"]->get_string();
            void* veval = NelsonConfiguration::getInstance()->getMainEvaluator();
            if (veval != nullptr) {
                auto* eval = static_cast<Evaluator*>(veval);
                ArrayOf dataAsArrayOf = ArrayOf::characterArrayConstructor(_data);
                ArrayOfVector argIn;
                argIn.push_back(dataAsArrayOf);
                Context* context = eval->getContext();
                if (context != nullptr) {
                    FunctionDefPtr funcDef;
                    if (context->lookupFunction(function_name, funcDef, true)) {
                        ArrayOfVector retval = funcDef->evaluateFunction(eval, argIn, 0);
                    }
                }
            }
        }));
}
//=============================================================================
void
SioClientCommand::siounregister(const std::string& name)
{
    _socket->off(name);
}
//=============================================================================
void
SioClientCommand::quit()
{
    _socket->emit("quit");
}
//=============================================================================
void
SioClientCommand::promptUpdated(const std::string& prompt)
{
    _socket->emit("prompt", prompt);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
