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
#include <string>
#include <csignal>
#include <algorithm>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <unordered_set>
#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <io.h>
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
#include "MainThreadTaskQueue.hpp"
#include "replxx/include/replxx.hxx"
#include "AnsiSgr.hpp"
#include "StringHelpers.hpp"
#include "NelsonHistory.hpp"
#include "characters_encoding.hpp"
#include "Evaluator.hpp"
#include "RuntimeDispatch.hpp"
#if WITH_TEXT_COMPLETION_MODULE
#include "CompleterHelper.hpp"
#include "BuiltinCompleter.hpp"
#include "MacroCompleter.hpp"
#include "VariableCompleter.hpp"
#endif
//=============================================================================
#if WITH_TEXT_COMPLETION_MODULE
// replxx counts contextLen in Unicode code points, not in bytes.
static int
countCodePoints(const std::wstring& text)
{
    int count = 0;
    for (wchar_t ch : text) {
        if (ch < 0xDC00 || ch > 0xDFFF) {
            count++;
        }
    }
    return count;
}
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

    // completionPrefix is the exact suffix of the line every entry replaces.
    contextLen = countCodePoints(completionPrefix);

    std::unordered_set<std::string> uniqueEntries;
    auto appendCategory = [&](const Nelson::wstringVector& source) {
        for (const auto& entry : source) {
            std::string utf8Entry = wstring_to_utf8(entry);
            if (uniqueEntries.insert(utf8Entry).second) {
                completions.emplace_back(std::move(utf8Entry));
            }
        }
    };

    appendCategory(variables);
    appendCategory(fields);
    appendCategory(properties);
    appendCategory(methods);
    appendCategory(macros);
    appendCategory(builtin);
    appendCategory(files);
#else
    (void)buffer;
    contextLen = 0;
#endif
    return completions;
}
//=============================================================================
// Pause after the last keystroke before replxx asks for the inline hint.
static constexpr int HINT_DELAY_MS = 200;
//=============================================================================
// Inline gray hint for a unique completion, from name tables only (no file listing).
static replxx::Replxx::hints_t
hintHook(const std::string& buffer, int& contextLen, replxx::Replxx::Color& color)
{
    replxx::Replxx::hints_t hints;
#if WITH_TEXT_COMPLETION_MODULE
    if (buffer.empty()) {
        return hints;
    }
    std::wstring currentW = utf8_to_wstring(buffer);
    std::wstring filepart = getPartialLineAsPath(currentW);
    std::wstring textpart = getPartialLine(currentW);
    if (isFileCompletionContext(currentW, filepart, textpart)) {
        return hints;
    }
    std::wstring token = getCompletionTokenToReplace(currentW, false);
    if (token.empty()) {
        return hints;
    }
    size_t tokenStart = currentW.size() - token.size();
    bool memberContext = (tokenStart > 0) && (currentW[tokenStart - 1] == L'.');
    std::unordered_set<std::string> uniqueEntries;
    auto collect = [&](const Nelson::wstringVector& source) {
        for (const auto& entry : source) {
            if (StringHelpers::istarts_with(entry, token)) {
                uniqueEntries.insert(wstring_to_utf8(entry));
            }
        }
    };
    if (memberContext) {
        // Indexed member expressions evaluate the object: leave them to TAB.
        size_t chunkStart = currentW.find_last_of(L" \t,;=");
        std::wstring chunk
            = (chunkStart == std::wstring::npos) ? currentW : currentW.substr(chunkStart + 1);
        if (chunk.find_first_of(L"({") != std::wstring::npos) {
            return hints;
        }
        std::wstring completionPrefix = currentW;
        Nelson::wstringVector files, builtin, macros, variables, fields, properties, methods;
        if (!computeCompletion(currentW, completionPrefix, files, builtin, macros, variables,
                fields, properties, methods)) {
            return hints;
        }
        for (const auto* source :
            { &variables, &fields, &properties, &methods, &macros, &builtin }) {
            collect(*source);
        }
    } else {
        collect(VariableCompleter(token));
        collect(MacroCompleter(token));
        collect(BuiltinCompleter(token));
    }
    if (uniqueEntries.size() == 1) {
        const std::string& candidate = *uniqueEntries.begin();
        if (!StringHelpers::iequals(utf8_to_wstring(candidate), token)) {
            contextLen = countCodePoints(token);
            color = replxx::Replxx::Color::GRAY;
            hints.emplace_back(candidate);
        }
    }
#else
    (void)buffer;
    (void)contextLen;
    (void)color;
#endif
    return hints;
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
static bool
isTerminalInputOutput()
{
#ifdef _WIN32
    return (_isatty(_fileno(stdin)) != 0) && (_isatty(_fileno(stdout)) != 0);
#else
    return (isatty(STDIN_FILENO) != 0) && (isatty(STDOUT_FILENO) != 0);
#endif
}
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
    if (isTerminalInputOutput()) {
        repl.reset(new replxx::Replxx(
            std::cin, std::cout, _fileno(stdin), _fileno(stdout), _fileno(stderr)));
    }
#else
    if (isTerminalInputOutput()) {
        repl.reset(
            new replxx::Replxx(std::cin, std::cout, STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO));
    }
#endif
    if (repl) {
        repl->set_completion_callback(&completionHook);
        repl->set_hint_callback(&hintHook);
        repl->set_hint_delay(HINT_DELAY_MS);
        repl->set_max_hint_rows(1);
        repl->set_indent_multiline(true);
        // Queued interpreter work wakes the blocking line read instead of a periodic poll.
        replxx::Replxx* replPtr = repl.get();
        Nelson::setInterpreterWakeupHook([replPtr]() { replPtr->wake(); });
    }
    atPrompt = false;
}
//=============================================================================
AdvancedTerminal::~AdvancedTerminal()
{
    if (repl) {
        // The hook may run on any thread: remove it before the line editor goes away.
        Nelson::setInterpreterWakeupHook(nullptr);
    }
}
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
static bool
enableBracketedPaste(replxx::Replxx* repl)
{
    if (repl == nullptr || !isTerminalInputOutput()) {
        return false;
    }
#ifdef _WIN32
    // The Windows input path of replxx reads key events and cannot decode the
    // bracketed paste markers (ESC [ 200 ~ ... ESC [ 201 ~): they would arrive as
    // literal keystrokes and corrupt the paste. Paste is handled by the pending
    // console input events heuristic, which only works while bracketed paste
    // stays disabled.
    (void)repl;
    return false;
#else
    try {
        repl->enable_bracketed_paste();
    } catch (const std::exception&) {
        return false;
    }
    return true;
#endif
}
//=============================================================================
static void
disableBracketedPaste(replxx::Replxx* repl)
{
    if (repl == nullptr) {
        return;
    }
    try {
        repl->disable_bracketed_paste();
    } catch (const std::exception&) {
    }
}
//=============================================================================
static bool
enableVirtualTerminalProcessing(FILE* stream)
{
#ifdef _WIN32
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif
    if (_isatty(_fileno(stream)) == 0) {
        return false;
    }
    static bool stdoutInitialized = false;
    static bool stdoutEnabled = false;
    static bool stderrInitialized = false;
    static bool stderrEnabled = false;
    bool& initialized = stream == stderr ? stderrInitialized : stdoutInitialized;
    bool& enabled = stream == stderr ? stderrEnabled : stdoutEnabled;
    if (initialized) {
        return enabled;
    }
    initialized = true;
    HANDLE handle = GetStdHandle(stream == stderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
    if (handle == nullptr || handle == INVALID_HANDLE_VALUE) {
        return false;
    }
    DWORD mode = 0;
    if (GetConsoleMode(handle, &mode) == 0) {
        return false;
    }
    if ((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0) {
        enabled = true;
        return true;
    }
    enabled = SetConsoleMode(handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING) != 0;
    return enabled;
#else
    int fd = stream == stderr ? STDERR_FILENO : STDOUT_FILENO;
    if (isatty(fd) == 0) {
        return false;
    }
    const char* term = std::getenv("TERM");
    return term == nullptr || term[0] == '\0' || std::strcmp(term, "dumb") != 0;
#endif
}
//=============================================================================
static void
writeStdoutUtf8(const std::string& msg)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
        std::wstring wide = utf8_to_wstring(msg);
        DWORD written = 0;
        if (WriteConsoleW(handle, wide.c_str(), static_cast<DWORD>(wide.size()), &written, nullptr)
            != 0) {
            return;
        }
    }
#endif
    std::cout << msg;
    std::cout.flush();
}
//=============================================================================
static void
writeAnsiUtf8(FILE* stream, const std::string& msg)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(stream == stderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
        DWORD written = 0;
        if (WriteConsoleA(handle, msg.c_str(), static_cast<DWORD>(msg.size()), &written, nullptr)
            != 0) {
            return;
        }
    }
#endif
    if (stream == stderr) {
        std::cerr << msg;
        std::cerr.flush();
    } else {
        std::cout << msg;
        std::cout.flush();
    }
}
//=============================================================================
static void
writeStderrUtf8(const std::string& msg)
{
#ifdef _WIN32
    HANDLE handle = GetStdHandle(STD_ERROR_HANDLE);
    if (handle != nullptr && handle != INVALID_HANDLE_VALUE) {
        std::wstring wide = utf8_to_wstring(msg);
        DWORD written = 0;
        if (WriteConsoleW(handle, wide.c_str(), static_cast<DWORD>(wide.size()), &written, nullptr)
            != 0) {
            return;
        }
    }
#endif
    std::cerr << msg;
    std::cerr.flush();
}
//=============================================================================
// Read one line straight from a redirected standard input. Returns an empty
// string once the input is exhausted, which is how the basic terminal reports
// end of input and how the evaluator knows to stop.
std::string
readRedirectedLine()
{
    std::string line;
    for (;;) {
        int currentChar = fgetc(stdin);
        if (currentChar == EOF) {
            break;
        }
        line.push_back(static_cast<char>(currentChar));
        if (currentChar == '\n') {
            break;
        }
    }
    return line;
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
        // Standard input is redirected, so there is no line editor to read from.
        // The piped line has to be read here: answering every prompt with an empty
        // line instead spins forever on "echo exit | nelson-adv-cli".
        if (!promptUtf8.empty()) {
            writeStdoutUtf8(promptUtf8);
        }
        std::string pipedLine = readRedirectedLine();
        atPrompt = false;
        if (pipedLine.empty()) {
            return {};
        }
        std::wstring retLineW = utf8_to_wstring(pipedLine);
        this->diary.writeMessage(retLineW);
        if (bIsInput) {
            if (StringHelpers::ends_with(retLineW, L"\n")) {
                retLineW.pop_back();
            }
            Nelson::History::setToken(L"");
        }
        return retLineW;
    }

    bool bracketedPasteEnabled = enableBracketedPaste(repl.get());
    // Parked at the prompt: let background posters (nelson-adv-cli --webview
    // figure interactions) run in place instead of waking the prompt, so mouse
    // rotate/zoom does not flood the terminal with empty prompts.
    Nelson::MainThreadTaskQueue::getInstance()->setInterpreterIdle(true);
    const char* line = repl->input(promptUtf8);
    Nelson::MainThreadTaskQueue::getInstance()->setInterpreterIdle(false);
    if (bracketedPasteEnabled) {
        disableBracketedPaste(repl.get());
    }
    if (line == nullptr) {
        if (atPrompt) {
            writeStdoutUtf8("\n");
            this->diary.writeMessage(L"\n");
        }
        atPrompt = false;
        return L"\n";
    }
    if (line[0] == 0) {
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
    outputMessage(wstring_to_utf8(msg));
}
//=============================================================================
void
AdvancedTerminal::outputMessage(const std::string& msg)
{
    if (atPrompt && repl) {
        // Output produced while parked at the prompt (e.g. a QML/graphics
        // callback pumped from replxx's input wait loop, running on this same
        // thread). Route it through replxx so it erases the prompt line, prints
        // the text, and repaints the prompt: a raw write would leave a stale
        // prompt line that only a manual Return refreshes.
        repl->write(msg.c_str(), static_cast<int>(msg.size()));
        this->diary.writeMessage(msg);
        return;
    }
    writeStdoutUtf8(msg);
    this->diary.writeMessage(msg);
}
//=============================================================================
bool
AdvancedTerminal::outputStyledText(const std::wstring& msg, const TextStyle& style)
{
    if (style.isPlain()) {
        outputMessage(msg);
        return false;
    }
    if (atPrompt) {
        writeStdoutUtf8("\n");
        atPrompt = false;
    }
    std::string plain = wstring_to_utf8(msg);
    if (!enableVirtualTerminalProcessing(stdout)) {
        writeStdoutUtf8(plain);
        this->diary.writeMessage(msg);
        return false;
    }
    writeStdoutUtf8(buildAnsiStyledMessage(plain, style));
    this->diary.writeMessage(msg);
    return true;
}
//=============================================================================
bool
AdvancedTerminal::outputAnsiText(const std::wstring& msg, bool asError)
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
    std::string data = wstring_to_utf8(text);
    if (asError) {
        if (atPrompt) {
            writeStdoutUtf8("\n");
            atPrompt = false;
        }
        if (enableVirtualTerminalProcessing(stderr)) {
            writeAnsiUtf8(stderr, data);
        } else {
            writeStderrUtf8(wstring_to_utf8(plain));
        }
        this->diary.writeMessage(plain);
        return true;
    }

    if (atPrompt) {
        writeStdoutUtf8("\n");
        atPrompt = false;
    }
    if (repl) {
        repl->write(data.c_str(), static_cast<int>(data.size()));
    } else if (enableVirtualTerminalProcessing(stdout)) {
        writeAnsiUtf8(stdout, data);
    } else {
        writeStdoutUtf8(wstring_to_utf8(plain));
    }
    this->diary.writeMessage(plain);
    return true;
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
    // Match BasicTerminal: a warning message is a full line, so it must end
    // with a newline. Without it the next prompt or output sticks to the
    // warning text (for example "f" followed immediately by the next line).
    std::string _msg = msg;
    if (_msg.empty() || _msg.back() != '\n') {
        _msg.push_back('\n');
    }
    writeStdoutUtf8(_msg);
    this->diary.writeMessage(_msg);
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
    if (repl) {
        // Wake the line editor instead of injecting a fake Return in the console input.
        repl->wake();
        return;
    }
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
