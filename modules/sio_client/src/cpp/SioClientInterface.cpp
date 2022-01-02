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
#include "SioClientInterface.hpp"
#include "SioClientCommand.hpp"
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "characters_encoding.hpp"
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
{}
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
        if (boost::algorithm::ends_with(command, L"\n")) {
            command.pop_back();
        }
    }
    atPrompt = false;
    return command;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
