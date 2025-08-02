//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    return _initialized;
}
//=============================================================================
SioClientCommand::SioClientCommand() { _initialized = false; }
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
}
//=============================================================================
void
SioClientCommand::clc()
{
}
//=============================================================================
void
SioClientCommand::available()
{
}
//=============================================================================
void
SioClientCommand::unavailable()
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
SioClientCommand::updateCommand(const std::string& newCommand)
{
}
//=============================================================================
void
SioClientCommand::sioemit(const std::string& name, const std::string& message)
{
}
//=============================================================================
void
SioClientCommand::sioregister(const std::string& name, const std::string& function_name)
{
}
//=============================================================================
void
SioClientCommand::siounregister(const std::string& name)
{
}
//=============================================================================
void
SioClientCommand::quit()
{
}
//=============================================================================
void
SioClientCommand::promptUpdated(const std::string& prompt)
{
}
//=============================================================================
} // namespace Nelson
//=============================================================================
