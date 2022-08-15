//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
BsdTerminal::~BsdTerminal() = default;
//=============================================================================
std::wstring
BsdTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
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
BsdTerminal::getInput(const std::wstring& prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
BsdTerminal::getLine(const std::wstring& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
BsdTerminal::getLine(const std::string& prompt)
{
    std::wstring wline = getLine(utf8_to_wstring(prompt));
    return wstring_to_utf8(wline);
}
//=============================================================================
size_t
BsdTerminal::getTerminalWidth()
{
    return getWidthLineNoise();
}
//=============================================================================
size_t
BsdTerminal::getTerminalHeight()
{
    return getHeightLineNoise();
}
//=============================================================================
void
BsdTerminal::outputMessage(const std::wstring& msg)
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
BsdTerminal::outputMessage(const std::string& msg)
{
    fprintf(stdout, "%s", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BsdTerminal::errorMessage(const std::wstring& msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BsdTerminal::errorMessage(const std::string& msg)
{
    fprintf(stderr, "%s\n", msg.c_str());
    this->diary.writeMessage(msg);
}
//=============================================================================
void
BsdTerminal::warningMessage(const std::wstring& msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BsdTerminal::warningMessage(const std::string& msg)
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
void
BsdTerminal::interruptGetLineByEvent()
{
}
//=============================================================================
