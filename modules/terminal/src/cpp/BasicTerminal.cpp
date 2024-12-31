//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include <cstring>
#include <iostream>
#include <csignal>
#include <thread>
#include <chrono>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "BasicTerminal.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
static bool bCONTROLC = false;
//=============================================================================
static void
ControlC_Command()
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
BasicTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    return utf8_to_wstring(getTextLine(wstring_to_utf8(prompt), bIsInput));
}
//=============================================================================
std::string
BasicTerminal::getTextLine(const std::string& prompt, bool bIsInput)
{
    atPrompt = true;
    fprintf(stdout, "%s", prompt.c_str());
    this->diary.writeMessage(prompt);
    std::string retLine;

    for (;;) {
        char currrentChar = fgetc(stdin);
        retLine.push_back(currrentChar);
        if (currrentChar == '\n') {
            break;
        }
    }
    this->diary.writeMessage(retLine);
    if (bIsInput) {
        if (StringHelpers::ends_with(retLine, "\n")) {
            retLine.pop_back();
        }
    }
    atPrompt = false;
    return retLine;
}
//=============================================================================
std::wstring
BasicTerminal::getInput(const std::wstring& prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
BasicTerminal::getLine(const std::wstring& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
BasicTerminal::getLine(const std::string& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
size_t
BasicTerminal::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
BasicTerminal::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
BasicTerminal::outputMessage(const std::wstring& msg)
{
    if (atPrompt) {
        outputMessage(L"\n");
    }
    outputMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::outputMessage(const std::string& msg)
{
    fmt::fprintf(stdout, "%s", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BasicTerminal::errorMessage(const std::wstring& msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::errorMessage(const std::string& msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        _msg = "\n" + msg;
        atPrompt = false;
    }
    fmt::fprintf(stderr, "%s", _msg.c_str());
    diary.writeMessage(_msg);
}
//=============================================================================
void
BasicTerminal::warningMessage(const std::wstring& msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::warningMessage(const std::string& msg)
{
    std::string _msg = msg + "\n";
    if (atPrompt) {
        _msg = "\n" + _msg;
        atPrompt = false;
    }
    fmt::fprintf(stdout, "%s", _msg.c_str());
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
    SetConsoleCtrlHandler(reinterpret_cast<PHANDLER_ROUTINE>(CtrlHandler), TRUE);
    SetConsoleOutputCP(65001);
#else
    signal(SIGINT, intHandler);
    signal(SIGTSTP, intHandler);
#endif
    atPrompt = false;
}
//=============================================================================
BasicTerminal::~BasicTerminal() = default;
//=============================================================================
bool
BasicTerminal::isAtPrompt()
{
    return atPrompt;
}
//=============================================================================
void
BasicTerminal::interruptGetLineByEvent()
{
}
//=============================================================================
