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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include <Windows.h>
#endif
#include "BasicTerminal.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string/predicate.hpp>
#include <cstring>
#include <iostream>
#include <signal.h>
//=============================================================================
static bool bCONTROLC = false;
//=============================================================================
static void
ControlC_Command(void)
{
    bCONTROLC = true;
    Nelson::sigInterrupt(1);
}
//=============================================================================
#ifdef _MSC_VER
static BOOL
CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType) {
    case CTRL_BREAK_EVENT:
    case CTRL_C_EVENT: {
        ControlC_Command();
    }
        return TRUE;
    case CTRL_SHUTDOWN_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_CLOSE_EVENT: {
    }
        return FALSE;
    }
    return FALSE;
}

#endif
//=============================================================================
static void
intHandler(int dummy = 0)
{
    ControlC_Command();
}
//=============================================================================
std::wstring
BasicTerminal::getTextLine(std::wstring prompt, bool bIsInput)
{
    return utf8_to_wstring(getTextLine(wstring_to_utf8(prompt), bIsInput));
}
//=============================================================================
std::string
BasicTerminal::getTextLine(std::string prompt, bool bIsInput)
{
    atPrompt = true;
    fprintf(stdout, "%s", prompt.c_str());
    this->diary.writeMessage(prompt);
    char buffer[CMD_BUFFER_SIZE];
    std::string retLine = "";
    if (fgets(buffer, CMD_BUFFER_SIZE, stdin) != nullptr) {
        retLine = buffer;
    }
    this->diary.writeMessage(retLine);
    if (bIsInput) {
        if (boost::algorithm::ends_with(retLine, L"\n")) {
            retLine.pop_back();
        }
    }
    atPrompt = false;
    return retLine;
}
//=============================================================================
std::wstring
BasicTerminal::getInput(std::wstring prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
BasicTerminal::getLine(std::wstring prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
BasicTerminal::getLine(std::string prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
size_t
BasicTerminal::getTerminalWidth()
{
    return WIDTH;
}
//=============================================================================
void
BasicTerminal::outputMessage(std::wstring msg)
{
    if (atPrompt) {
        outputMessage(L"\n");
    }
    outputMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::outputMessage(std::string msg)
{
    fprintf(stdout, "%s", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BasicTerminal::errorMessage(std::wstring msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::errorMessage(std::string msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        _msg = "\n" + msg;
        atPrompt = false;
    }
    fprintf(stderr, "%s", _msg.c_str());
    diary.writeMessage(_msg);
}
//=============================================================================
void
BasicTerminal::warningMessage(std::wstring msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::warningMessage(std::string msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        msg = "\n" + msg;
        atPrompt = false;
    }
    fprintf(stdout, "%s", _msg.c_str());
    diary.writeMessage(_msg);
}
//=============================================================================
void
BasicTerminal::clearTerminal()
{
    // NOTHING
}
//=============================================================================
BasicTerminal::BasicTerminal()
{
#ifdef _MSC_VER
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
#else
    signal(SIGINT, intHandler);
    signal(SIGTSTP, intHandler);
#endif
    /*
    #ifndef _MSC_VER
         setbuf(stdout, NULL);
         setbuf(stdin, NULL);
    #endif
    */
    atPrompt = false;
}
//=============================================================================
BasicTerminal::~BasicTerminal() {}
//=============================================================================
bool
BasicTerminal::isAtPrompt()
{
    return atPrompt;
}
//=============================================================================
