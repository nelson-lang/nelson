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
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "SioClientInterface.hpp"
#include "characters_encoding.hpp"
#include "SioClientCommand.hpp"
//=============================================================================
namespace Nelson {
SioClientInterface::SioClientInterface() { atPrompt = false; }
//=============================================================================
SioClientInterface::~SioClientInterface() {}
//=============================================================================
std::wstring
SioClientInterface::getLine(std::wstring prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
SioClientInterface::getLine(std::string prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::wstring
SioClientInterface::getInput(std::wstring prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
size_t
SioClientInterface::getTerminalWidth()
{
    return WIDTH;
}
//=============================================================================
void
SioClientInterface::outputMessage(std::wstring msg)
{
    if (atPrompt) {
        outputMessage(wstring_to_utf8(msg) + std::string("\n"));
    } else {
        outputMessage(wstring_to_utf8(msg));
    }
}
//=============================================================================
void
SioClientInterface::outputMessage(std::string msg)
{
    SioClientCommand::getInstance()->reply(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
void
SioClientInterface::errorMessage(std::wstring msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
SioClientInterface::errorMessage(std::string msg)
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
SioClientInterface::warningMessage(std::wstring msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
SioClientInterface::warningMessage(std::string msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        msg = "\n" + msg;
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
std::wstring
SioClientInterface::getTextLine(std::wstring prompt, bool bIsInput)
{
    return utf8_to_wstring(getTextLine(wstring_to_utf8(prompt), bIsInput));
}
//=============================================================================
std::string
SioClientInterface::getTextLine(std::string prompt, bool bIsInput)
{
    std::string command;
    atPrompt = true;
    SioClientCommand::getInstance()->reply("\n" + prompt);
    if (!prompt.empty()) {
        this->diary.writeMessage("\n" + prompt);
    }
    do {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        command = SioClientCommand::getInstance()->getCommand();
    } while (command.empty());
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
}
//=============================================================================
