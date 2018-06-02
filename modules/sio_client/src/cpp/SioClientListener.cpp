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
#include <iostream>
#include <string>
#include "SioClientListener.hpp"
//=============================================================================
SioClientListener::SioClientListener() { connection_finish = false; }
//=============================================================================
SioClientListener::~SioClientListener() { connection_finish = false; }
//=============================================================================
void
SioClientListener::on_fail()
{
    std::cout << "SioClientListener.on_fail" << std::endl;
}
//=============================================================================
void
SioClientListener::on_connected()
{
    std::cout << "SioClientListener.on_connected" << std::endl;
    lock.lock();
    condition.notify_all();
    connection_finish = true;
    lock.unlock();
}
//=============================================================================
void
SioClientListener::on_close(sio::client::close_reason const& reason)
{
    std::cout << "SioClientListener.on_close" << std::endl;
}
//=============================================================================
