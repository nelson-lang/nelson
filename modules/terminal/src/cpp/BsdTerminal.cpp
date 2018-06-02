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
#include "BsdTerminal.hpp"
#include "NelsonHistory.hpp"
#include "characters_encoding.hpp"
#include "linenoise.h"
#include <boost/algorithm/string/predicate.hpp>
#include <cstring>
#include <iostream>
//=============================================================================
BsdTerminal::BsdTerminal()
{
    linenoiseSetMultiLine(1);
    atPrompt = false;
}
//=============================================================================
BsdTerminal::~BsdTerminal() {}
//=============================================================================
std::wstring
BsdTerminal::getTextLine(std::wstring prompt, bool bIsInput)
{
    atPrompt = true;
    if (!prompt.empty()) {
        this->diary.writeMessage(prompt);
    }
    char* line = linenoise(wstring_to_utf8(prompt).c_str());
    std::wstring retLineW = L"";
    if (line) {
        std::string retLine = line;
        linenoiseFree(line);
        line = nullptr;
        retLineW = utf8_to_wstring(retLine);
        if (retLineW.empty()) {
            retLineW = L"\n";
        }
        if (!bIsInput) {
            Nelson::History::addLine(retLineW);
        }
        this->diary.writeMessage(retLineW);
    } else {
        retLineW = L"\n";
        atPrompt = false;
        return retLineW;
    }
    if (bIsInput) {
        if (boost::algorithm::ends_with(retLineW, L"\n")) {
            retLineW.pop_back();
        }
        Nelson::History::setToken(L"");
    }
    atPrompt = false;
    return retLineW;
}
//=============================================================================
std::wstring
BsdTerminal::getInput(std::wstring prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
BsdTerminal::getLine(std::wstring prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
BsdTerminal::getLine(std::string prompt)
{
    std::wstring wline = getLine(utf8_to_wstring(prompt));
    return wstring_to_utf8(wline);
}
//=============================================================================
size_t
BsdTerminal::getTerminalWidth()
{
    return 80;
}
//=============================================================================
void
BsdTerminal::outputMessage(std::wstring msg)
{
    std::string _msg = wstring_to_utf8(msg);
    if (atPrompt) {
        clearLine();
        atPrompt = false;
        interruptReadLine();
    }
    outputMessage(_msg);
}
//=============================================================================
void
BsdTerminal::outputMessage(std::string msg)
{
    fprintf(stdout, "%s", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BsdTerminal::errorMessage(std::wstring msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BsdTerminal::errorMessage(std::string msg)
{
    fprintf(stderr, "%s\n", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BsdTerminal::warningMessage(std::wstring msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BsdTerminal::warningMessage(std::string msg)
{
    fprintf(stdout, "%s", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BsdTerminal::clearTerminal()
{
    linenoiseClearScreen();
}
//=============================================================================
bool
BsdTerminal::isAtPrompt()
{
    return atPrompt;
}
//=============================================================================
