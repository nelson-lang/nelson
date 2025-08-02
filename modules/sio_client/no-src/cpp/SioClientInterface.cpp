//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SioClientInterface.hpp"
#include "SioClientCommand.hpp"
//=============================================================================
namespace Nelson {
SioClientInterface::SioClientInterface() { atPrompt = false; }
//=============================================================================
SioClientInterface::~SioClientInterface() = default;
//=============================================================================
std::wstring
SioClientInterface::getLine(const std::wstring& prompt)
{
    return L"";
}
//=============================================================================
std::string
SioClientInterface::getLine(const std::string& prompt)
{
    return "";
}
//=============================================================================
std::wstring
SioClientInterface::getInput(const std::wstring& prompt)
{
    return L"";
}
//=============================================================================
size_t
SioClientInterface::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
SioClientInterface::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
SioClientInterface::outputMessage(const std::wstring& msg)
{
}
//=============================================================================
void
SioClientInterface::outputMessage(const std::string& msg)
{
}
//=============================================================================
void
SioClientInterface::errorMessage(const std::wstring& msg)
{
}
//=============================================================================
void
SioClientInterface::errorMessage(const std::string& msg)
{
}
//=============================================================================
void
SioClientInterface::warningMessage(const std::wstring& msg)
{
}
//=============================================================================
void
SioClientInterface::warningMessage(const std::string& msg)
{
}
//=============================================================================
void
SioClientInterface::clearTerminal()
{
}
//=============================================================================
bool
SioClientInterface::isAtPrompt()
{
    return false;
}
//=============================================================================
void
SioClientInterface::interruptGetLineByEvent()
{
}
//=============================================================================
std::wstring
SioClientInterface::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    return L"";
}
//=============================================================================
std::string
SioClientInterface::getTextLine(const std::string& prompt, bool bIsInput)
{
    return "";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
