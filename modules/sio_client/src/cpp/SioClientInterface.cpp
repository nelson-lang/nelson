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
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
SioClientInterface::SioClientInterface() { atPrompt = false; }
//=============================================================================
SioClientInterface::~SioClientInterface() = default;
//=============================================================================
std::wstring
SioClientInterface::getLine(const std::wstring& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
SioClientInterface::getLine(const std::string& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::wstring
SioClientInterface::getInput(const std::wstring& prompt)
{
    return getTextLine(prompt, true);
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
    if (atPrompt) {
        outputMessage(wstring_to_utf8(msg) + std::string("\n"));
    } else {
        outputMessage(wstring_to_utf8(msg));
    }
}
//=============================================================================
void
SioClientInterface::outputMessage(const std::string& msg)
{
    SioClientCommand::getInstance()->reply(msg);
    this->diary.writeMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
SioClientInterface::errorMessage(const std::wstring& msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
SioClientInterface::errorMessage(const std::string& msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        _msg = "\n" + msg;
        atPrompt = false;
    }
    SioClientCommand::getInstance()->reply(_msg);
    diary.writeMessage(_msg);
}
//=============================================================================
void
SioClientInterface::warningMessage(const std::wstring& msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
SioClientInterface::warningMessage(const std::string& msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        _msg = "\n" + _msg;
        atPrompt = false;
    }
    SioClientCommand::getInstance()->reply(_msg);
    diary.writeMessage(_msg);
}
//=============================================================================
void
SioClientInterface::clearTerminal()
{
    SioClientCommand::getInstance()->clc();
}
//=============================================================================
bool
SioClientInterface::isAtPrompt()
{
    return atPrompt;
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
    return utf8_to_wstring(getTextLine(wstring_to_utf8(prompt), bIsInput));
}
//=============================================================================
std::string
SioClientInterface::getTextLine(const std::string& prompt, bool bIsInput)
{
    std::string command;
    atPrompt = true;
    SioClientCommand::getInstance()->reply("\n" + prompt);
    if (!prompt.empty()) {
        SioClientCommand::getInstance()->promptUpdated(prompt);
        this->diary.writeMessage("\n" + prompt);
    }
    SioClientCommand::getInstance()->available();
    do {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        command = SioClientCommand::getInstance()->getCommand();
    } while (command.empty());
    SioClientCommand::getInstance()->unavailable();
    this->diary.writeMessage(command);
    if (bIsInput) {
        if (StringHelpers::ends_with(command, "\n")) {
            command.pop_back();
        }
    }
    atPrompt = false;
    return command;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
