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
#include <vector>
#include <cstdio>
#include <unordered_set>
#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <sys/ioctl.h>
#include <unistd.h>
#endif
#ifdef _WIN32
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#endif
#include "nlsBuildConfig.h"
#include "AdvancedTerminal.hpp"
#include "replxx/include/replxx.hxx"
#include "StringHelpers.hpp"
#include "NelsonHistory.hpp"
#include "characters_encoding.hpp"
#include "Evaluator.hpp"
#if WITH_TEXT_COMPLETION_MODULE
#include "CompleterHelper.hpp"
#endif
//=============================================================================
static replxx::Replxx::completions_t
completionHook(const std::string& buffer, int& contextLen)
{
    replxx::Replxx::completions_t completions;
#if WITH_TEXT_COMPLETION_MODULE
    std::wstring currentW = utf8_to_wstring(buffer);
    std::wstring completionPrefix = currentW;
    Nelson::wstringVector files, builtin, macros, variables, fields, properties, methods;

    if (!computeCompletion(currentW, completionPrefix, files, builtin, macros, variables, fields,
            properties, methods)) {
        contextLen = 0;
        return completions;
    }

    std::wstring replacementPrefix
        = files.empty() ? completionPrefix : Nelson::getCompletionLeafPrefix(completionPrefix);
    std::string completionPrefixUtf8 = wstring_to_utf8(replacementPrefix);
    contextLen = static_cast<int>(
        std::min(completionPrefixUtf8.size(), static_cast<std::size_t>(buffer.size())));

    std::unordered_set<std::string> uniqueEntries;
    auto appendCategory = [&](const Nelson::wstringVector& source) {
        for (const auto& entry : source) {
            std::string utf8Entry = wstring_to_utf8(entry);
            if (uniqueEntries.insert(utf8Entry).second) {
                completions.emplace_back(std::move(utf8Entry));
            }
        }
    };

    appendCategory(files);
    appendCategory(builtin);
    appendCategory(macros);
    appendCategory(variables);
    appendCategory(fields);
    appendCategory(properties);
    appendCategory(methods);
#else
    (void)buffer;
    contextLen = 0;
#endif
    return completions;
}
//=============================================================================
static void
intHandler(int dummy = 0)
{
    signal(SIGINT, intHandler);
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
        Nelson::sigInterrupt(1);
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
AdvancedTerminal::AdvancedTerminal() : repl(), syncedHistorySize(0)
{
#ifdef _MSC_VER
    SetConsoleCtrlHandler(reinterpret_cast<PHANDLER_ROUTINE>(CtrlHandler), TRUE);
    signal(SIGINT, intHandler);
#else
    signal(SIGINT, intHandler);
    signal(SIGTSTP, intHandler);
#endif
#ifdef _WIN32
    repl.reset(
        new replxx::Replxx(std::cin, std::cout, _fileno(stdin), _fileno(stdout), _fileno(stderr)));
#else
    repl.reset(new replxx::Replxx(std::cin, std::cout, STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO));
#endif
    if (repl) {
        repl->set_completion_callback(&completionHook);
        repl->set_indent_multiline(true);
    }
    atPrompt = false;
}
//=============================================================================
AdvancedTerminal::~AdvancedTerminal() = default;
//=============================================================================
namespace {
//=============================================================================
static size_t
queryTerminalWidth()
{

#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info)) {
        return static_cast<size_t>(info.srWindow.Right - info.srWindow.Left + 1);
    }
#else
    struct winsize ws {};
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) != -1 && ws.ws_col > 0) {
        return static_cast<size_t>(ws.ws_col);
    }
#endif
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
static size_t
queryTerminalHeight()
{

#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info)) {
        return static_cast<size_t>(info.srWindow.Bottom - info.srWindow.Top + 1);
    }
#else
    struct winsize ws {};
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) != -1 && ws.ws_row > 0) {
        return static_cast<size_t>(ws.ws_row);
    }
#endif
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
static void
injectNewlineIntoInput()
{
#ifdef _WIN32
    HANDLE stdinHandle = GetStdHandle(STD_INPUT_HANDLE);
    if (stdinHandle == INVALID_HANDLE_VALUE) {
        return;
    }
    INPUT_RECORD records[2] = {};
    records[0].EventType = KEY_EVENT;
    records[0].Event.KeyEvent.bKeyDown = TRUE;
    records[0].Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
    records[0].Event.KeyEvent.wVirtualScanCode = MapVirtualKey(VK_RETURN, MAPVK_VK_TO_VSC);
    records[0].Event.KeyEvent.uChar.UnicodeChar = L'\r';
    records[1] = records[0];
    records[1].Event.KeyEvent.bKeyDown = FALSE;
    DWORD written = 0;
    WriteConsoleInputW(stdinHandle, records, 2, &written);
#else
#ifdef TIOCSTI
    char ch = '\n';
    ioctl(STDIN_FILENO, TIOCSTI, &ch);
#endif
#endif
}
//=============================================================================
#ifdef _WIN32
static bool
isConsoleHandle(HANDLE handle)
{
    if (handle == nullptr || handle == INVALID_HANDLE_VALUE) {
        return false;
    }
    DWORD mode = 0;
    return GetConsoleMode(handle, &mode) != 0;
}
//=============================================================================
static void
writePipeOrFile(HANDLE handle, const std::string& msg)
{
    if (msg.empty()) {
        return;
    }
    DWORD totalWritten = 0;
    DWORD remaining = static_cast<DWORD>(msg.size());
    const char* data = msg.data();
    while (remaining > 0) {
        DWORD written = 0;
        if (!WriteFile(handle, data + totalWritten, remaining, &written, nullptr) || written == 0) {
            return;
        }
        totalWritten += written;
        remaining -= written;
    }
    if (GetFileType(handle) == FILE_TYPE_DISK) {
        FlushFileBuffers(handle);
    }
}
//=============================================================================
#endif
//=============================================================================
static void
writeStdoutUtf8(const std::string& msg)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
        if (isConsoleHandle(handle)) {
            std::wstring wide = utf8_to_wstring(msg);
            DWORD written = 0;
            WriteConsoleW(handle, wide.c_str(), static_cast<DWORD>(wide.size()), &written, nullptr);
        } else {
            writePipeOrFile(handle, msg);
        }
        std::cout.flush();
        return;
    }
#endif
    std::cout << msg;
    std::cout.flush();
}
//=============================================================================
static void
writeStderrUtf8(const std::string& msg)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_ERROR_HANDLE);
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
        if (isConsoleHandle(handle)) {
            std::wstring wide = utf8_to_wstring(msg);
            DWORD written = 0;
            WriteConsoleW(handle, wide.c_str(), static_cast<DWORD>(wide.size()), &written, nullptr);
        } else {
            writePipeOrFile(handle, msg);
        }
        std::cerr.flush();
        return;
    }
#endif
    std::cerr << msg;
    std::cerr.flush();
}
//=============================================================================
} // namespace
//=============================================================================
std::wstring
AdvancedTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    atPrompt = true;
    if (!prompt.empty()) {
        this->diary.writeMessage(prompt);
    }
    syncHistory();

    std::string promptUtf8 = wstring_to_utf8(prompt);
    if (!repl) {
        atPrompt = false;
        return L"\n";
    }

    const char* line = repl->input(promptUtf8);
    if (line == nullptr || line[0] == 0) {
        atPrompt = false;
        return L"\n";
    }

    std::string utf8Line(line);

    std::string logLineUtf8 = utf8Line;
    logLineUtf8.push_back('\n');
    std::wstring retLineW = utf8_to_wstring(logLineUtf8);

    if (!bIsInput) {
        Nelson::History::addLine(retLineW);
        ++syncedHistorySize;
        if (repl && !utf8Line.empty()) {
            repl->history_add(utf8Line);
        }
    }

    this->diary.writeMessage(retLineW);
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
    return queryTerminalWidth();
}
//=============================================================================
size_t
AdvancedTerminal::getTerminalHeight()
{
    return queryTerminalHeight();
}
//=============================================================================
void
AdvancedTerminal::outputMessage(const std::wstring& msg)
{
    std::string _msg = wstring_to_utf8(msg);
    if (atPrompt) {
        writeStdoutUtf8("\n");
        atPrompt = false;
    }

    outputMessage(_msg);
}
//=============================================================================
void
AdvancedTerminal::outputMessage(const std::string& msg)
{
    writeStdoutUtf8(msg);
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
    if (atPrompt) {
        writeStdoutUtf8("\n");
        atPrompt = false;
    }
    writeStderrUtf8(msg);
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
    if (atPrompt) {
        writeStdoutUtf8("\n");
        atPrompt = false;
    }
    writeStdoutUtf8(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
void
AdvancedTerminal::clearTerminal()
{
    if (repl) {
        repl->clear_screen();
    }
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
    injectNewlineIntoInput();
}
//=============================================================================
void
AdvancedTerminal::syncHistory()
{
    if (!repl) {
        return;
    }
    Nelson::wstringVector historyContent = Nelson::History::get();
    if (syncedHistorySize > historyContent.size()) {
        syncedHistorySize = 0;
    }

    for (std::size_t i = syncedHistorySize; i < historyContent.size(); ++i) {
        std::wstring cleanedLine = historyContent[i];
        while (
            !cleanedLine.empty() && (cleanedLine.back() == L'\n' || cleanedLine.back() == L'\r')) {
            cleanedLine.pop_back();
        }
        std::string utf8Line = wstring_to_utf8(cleanedLine);
        if (!utf8Line.empty()) {
            repl->history_add(utf8Line);
        }
    }
    syncedHistorySize = historyContent.size();
}
//=============================================================================
