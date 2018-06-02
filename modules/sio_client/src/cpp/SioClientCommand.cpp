//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <sstream>
#include <sio_message.h>
#include "SioClientCommand.hpp"
#include "characters_encoding.hpp"
#include "SioClientListener.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	SioClientCommand* SioClientCommand::m_pInstance = nullptr;
	sio::socket::ptr SioClientCommand::_socket = nullptr;
	//=============================================================================
	bool SioClientCommand::create(const std::string &ipAddress)
	{
		if (!_initialized)
		{
			if (!ipAddress.empty())
			{
				return createConnection(ipAddress);
			}
		}
		return false;
	}
	//=============================================================================
	bool SioClientCommand::isInitialized()
	{
		return _initialized;
	}
	//=============================================================================
	SioClientCommand::SioClientCommand()
	{
		_initialized = false;
	}
	//=============================================================================
	bool SioClientCommand::createConnection(const std::string &ipAddress)
	{
		if (!ipAddress.empty())
		{
			_command = "";
			_sioClient.set_open_listener(std::bind(&SioClientListener::on_connected, &_sioClientListener));
			_sioClient.set_close_listener(std::bind(&SioClientListener::on_close, &_sioClientListener, std::placeholders::_1));
			_sioClient.set_fail_listener(std::bind(&SioClientListener::on_fail, &_sioClientListener));
			_sioClient.connect(_ipAddress);
			_ipAddress = ipAddress;

			_sioClientListener.lock.lock();
			if (!_sioClientListener.connection_finish)
			{
				_sioClientListener.condition.wait(_sioClientListener.lock);
			}

			_sioClientListener.lock.unlock();
			_socket = _sioClient.socket();

			_socket->on("command", sio::socket::event_listener_aux([&](std::string const& name, sio::message::ptr const& data, bool isAck, sio::message::list &ack_resp)
			{
				_sioClientListener.lock.lock();
				_command = data->get_map()["data"]->get_string();
				_sioClientListener.lock.unlock();
				_socket->emit("command_received");
			}));
			_initialized = true;
			return true;
		}
		return false;
	}
	//=============================================================================
	SioClientCommand *SioClientCommand::getInstance()
	{
		if (m_pInstance == nullptr)
		{
			try
			{
				m_pInstance = new SioClientCommand();
			}
			catch (std::bad_alloc)
			{
				m_pInstance = nullptr;
			}
		}
		return m_pInstance;
	}
	//=============================================================================
	void SioClientCommand::reply(std::string stringToReply)
	{
		std::stringstream output;
		output << stringToReply;
		sio::message::ptr send_data(sio::object_message::create());
		std::map<std::string, sio::message::ptr>& map = send_data->get_map();
		map.insert(std::make_pair("output", sio::string_message::create(output.str())));
		_socket->emit("reply", send_data);
	}
	//=============================================================================
	std::string SioClientCommand::getCommand()
	{
		std::string returnedCommand = _command;
		_command.clear();
		return returnedCommand;
	}
	//=============================================================================
	void SioClientCommand::updateCommand(std::string newCommand)
	{
		_command = newCommand;
	}
	//=============================================================================
}
//=============================================================================