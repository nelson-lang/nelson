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
#include "SioClientCommand.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
SioClientCommand* SioClientCommand::m_pInstance = nullptr;
//=============================================================================
bool
SioClientCommand::create(const std::string& ipAddress)
{
    return false;
}
//=============================================================================
bool
SioClientCommand::isInitialized()
{
    return false;
}
//=============================================================================
SioClientCommand::SioClientCommand() 
{
}
//=============================================================================
bool
SioClientCommand::createConnection(const std::string& ipAddress)
{
    return false;
}
//=============================================================================
SioClientCommand*
SioClientCommand::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new SioClientCommand();
        } catch (std::bad_alloc) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
SioClientCommand::reply(std::string stringToReply)
{
}
//=============================================================================
std::string
SioClientCommand::getCommand()
{
    return "";
}
//=============================================================================
void
SioClientCommand::updateCommand(std::string newCommand)
{
}
//=============================================================================
}
//=============================================================================

