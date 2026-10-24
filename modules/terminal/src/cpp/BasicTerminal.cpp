//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <io.h>
#endif
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <csignal>
#include <future>
#include <thread>
#include <chrono>
#include "BasicTerminal.hpp"
#include "Evaluator.hpp"
#include "RuntimeDispatch.hpp"
#include "QueueProcessing.hpp"
#include "AnsiSgr.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
static bool bCONTROLC = false;
//=============================================================================
static bool
enableVirtualTerminalProcessing(FILE* stream)
{
#ifdef _WIN32
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
    intptr_t osHandle = _get_osfhandle(_fileno(stream));
    if (osHandle == -1) {
        return false;
    }
    HANDLE handle = reinterpret_cast<HANDLE>(osHandle);
    DWORD mode = 0;
    if (GetConsoleMode(handle, &mode) == 0) {
        return false;
    }
    if ((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
        return true;
    }
    return SetConsoleMode(handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0;
#else
    (void)stream;
    return true;
#endif
}
//=============================================================================
static std::string
readLineBlocking()
{
    std::string retLine;
    for (;;) {
        int currentChar = fgetc(stdin);
        if (currentChar == EOF) {
            break;
        }
        retLine.push_back(static_cast<char>(currentChar));
        if (currentChar == '\n') {
            break;
        }
    }
    return retLine;
}
//=============================================================================
static void
processPromptIdle()
{
    Nelson::processPendingCallbacksAndTimers();
    Nelson::processGuiEvents(false);
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
}
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
    currentPrompt = prompt;
    atPrompt = true;
    fprintf(stdout, "%s", prompt.c_str());
    fflush(stdout);
    this->diary.writeMessage(prompt);

    std::future<std::string> lineFuture = std::async(std::launch::async, readLineBlocking);
    while (lineFuture.wait_for(std::chrono::milliseconds(0)) != std::future_status::ready) {
        processPromptIdle();
        redrawPromptIfNeeded();
    }

    std::string retLine = lineFuture.get();
    this->diary.writeMessage(retLine);
    if (bIsInput) {
        if (StringHelpers::ends_with(retLine, "\n")) {
            retLine.pop_back();
        }
    }
    clearPromptState();
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
    outputMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
BasicTerminal::outputMessage(const std::string& msg)
{
    markPromptInterrupted();
    fmt::print(stdout, "{0}", msg.c_str());
    fflush(stdout);
    if (promptRedrawPending) {
        promptRedrawNeedsNewline = !StringHelpers::ends_with(msg, "\n");
    }
    this->diary.writeMessage(msg);
}
//=============================================================================
bool
BasicTerminal::outputStyledText(const std::wstring& msg, const TextStyle& style)
{
    (void)style;
    outputMessage(msg);
    return false;
}
//=============================================================================
bool
BasicTerminal::outputAnsiText(const std::wstring& msg, bool asError)
{
    if (!containsAnsiEscape(msg)) {
        if (asError) {
            errorMessage(msg);
        } else {
            outputMessage(msg);
        }
        return false;
    }

    std::wstring text = asError ? msg + L"\n" : msg;
    std::wstring plain = stripAnsiSgr(text);
    std::string data = wstring_to_utf8(
        enableVirtualTerminalProcessing(asError ? stderr : stdout) ? text : plain);

    markPromptInterrupted();
    FILE* stream = asError ? stderr : stdout;
    fmt::print(stream, "{0}", data.c_str());
    fflush(stream);
    if (!asError && promptRedrawPending) {
        promptRedrawNeedsNewline = !StringHelpers::ends_with(data, "\n");
    } else if (asError) {
        promptRedrawNeedsNewline = false;
    }
    this->diary.writeMessage(plain);
    return true;
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
    markPromptInterrupted();
    fmt::print(stderr, "{0}", _msg.c_str());
    fflush(stderr);
    promptRedrawNeedsNewline = false;
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
    markPromptInterrupted();
    fmt::print(stdout, "{0}", _msg.c_str());
    fflush(stdout);
    promptRedrawNeedsNewline = false;
    diary.writeMessage(_msg);
}
//=============================================================================
void
BasicTerminal::clearTerminal()
{
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
    promptRedrawPending = false;
    promptRedrawNeedsNewline = false;
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
BasicTerminal::markPromptInterrupted()
{
    if (!atPrompt) {
        return;
    }
    fmt::print(stdout, "\n");
    fflush(stdout);
    diary.writeMessage("\n");
    atPrompt = false;
    promptRedrawPending = !currentPrompt.empty();
    promptRedrawNeedsNewline = false;
}
//=============================================================================
void
BasicTerminal::redrawPromptIfNeeded()
{
    if (!promptRedrawPending || currentPrompt.empty()) {
        return;
    }
    if (promptRedrawNeedsNewline) {
        fmt::print(stdout, "\n");
        fflush(stdout);
        diary.writeMessage("\n");
    }
    fmt::print(stdout, "{0}", currentPrompt.c_str());
    fflush(stdout);
    atPrompt = true;
    promptRedrawPending = false;
    promptRedrawNeedsNewline = false;
}
//=============================================================================
void
BasicTerminal::clearPromptState()
{
    atPrompt = false;
    promptRedrawPending = false;
    promptRedrawNeedsNewline = false;
    currentPrompt.clear();
}
//=============================================================================
void
BasicTerminal::interruptGetLineByEvent()
{
}
//=============================================================================
