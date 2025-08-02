//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <iostream>
#include <csignal>
#include <algorithm>
#include "nlsBuildConfig.h"
#include "AdvancedTerminal.hpp"
#include "StringHelpers.hpp"
#include "NelsonHistory.hpp"
#include "characters_encoding.hpp"
#include "Evaluator.hpp"
#if WITH_TEXT_COMPLETION_MODULE
#include "CompleterHelper.hpp"
#endif
//=============================================================================
static void
intHandler(int dummy = 0)
{
    Nelson::sigInterrupt(1);
}
//=============================================================================
AdvancedTerminal::AdvancedTerminal()
{
    signal(SIGINT, intHandler);
#ifndef _MSC_VER
    signal(SIGTSTP, intHandler);
#endif
    atPrompt = false;
}
//=============================================================================
AdvancedTerminal::~AdvancedTerminal() = default;
//=============================================================================
linse::completions
completionHook(std::string_view prefix)
{
    linse::completions lc;
#if WITH_TEXT_COMPLETION_MODULE
    std::string str(prefix);
    std::wstring wprefix = utf8_to_wstring(str);
    wprefix = getPartialLine(wprefix);
    str = wstring_to_utf8(wprefix);
    // basic completion
    stringVector dictionary = getCompletionDictionary(wprefix);

    for (std::size_t i = 0; i < dictionary.size(); ++i) {
        if (strncmp(str.data(), dictionary[i].c_str(), str.size()) == 0) {
            lc.add_completion(std::string_view { dictionary[i] }.substr(str.size()));
        }
    }
#endif
    return lc;
}
//=============================================================================
std::wstring
AdvancedTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    atPrompt = true;
    if (!prompt.empty()) {
        this->diary.writeMessage(prompt);
    }
    wstringVector historyContent = Nelson::History::get();
    for (auto& line : historyContent) {
        ls.history.add(wstring_to_utf8(line));
    }
    ls.completion_callback = linse::word_completion { &completionHook };
    ls.install_window_change_handler();
    auto line = ls(wstring_to_utf8(prompt).c_str());
    std::wstring retLineW = L"";
    if (line.has_value()) {
        retLineW = utf8_to_wstring(line.value());
        if (retLineW.empty()) {
            retLineW = L"\n";
        }
        if (!bIsInput) {
            Nelson::History::addLine(retLineW);
        }
        this->diary.writeMessage(retLineW);
    } else {
        ls.clearLine();
        ls.interruptReadLine(false);
        retLineW = L"\n";
        atPrompt = false;
        return retLineW;
    }
    if (bIsInput) {
        if (StringHelpers::ends_with(retLineW, L"\n")) {
            retLineW.pop_back();
        }
        Nelson::History::setToken(L"");
    }
    atPrompt = false;
    return retLineW;
}
//=============================================================================
std::wstring
AdvancedTerminal::getInput(const std::wstring& prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
AdvancedTerminal::getLine(const std::wstring& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
AdvancedTerminal::getLine(const std::string& prompt)
{
    std::wstring wline = getLine(utf8_to_wstring(prompt));
    return wstring_to_utf8(wline);
}
//=============================================================================
size_t
AdvancedTerminal::getTerminalWidth()
{
    return ls.get_screen_columns();
}
//=============================================================================
size_t
AdvancedTerminal::getTerminalHeight()
{
    return ls.get_screen_rows();
}
//=============================================================================
void
AdvancedTerminal::outputMessage(const std::wstring& msg)
{
    std::string _msg = wstring_to_utf8(msg);
    if (atPrompt) {
        ls.clearLine();
        atPrompt = false;
        ls.interruptReadLine();
    }

    outputMessage(_msg);
}
//=============================================================================
void
AdvancedTerminal::outputMessage(const std::string& msg)
{
    ls.writeStdout(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
void
AdvancedTerminal::errorMessage(const std::wstring& msg)
{
    errorMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
AdvancedTerminal::errorMessage(const std::string& msg)
{
    ls.writeStderr(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
void
AdvancedTerminal::warningMessage(const std::wstring& msg)
{
    warningMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
AdvancedTerminal::warningMessage(const std::string& msg)
{
    ls.writeStdout(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
void
AdvancedTerminal::clearTerminal()
{
    ls.clear_screen();
}
//=============================================================================
bool
AdvancedTerminal::isAtPrompt()
{
    return atPrompt;
}
//=============================================================================
void
AdvancedTerminal::interruptGetLineByEvent()
{
    ls.clearLine();
    ls.interruptReadLine();
}
//=============================================================================
