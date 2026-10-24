//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtCore/QTimer>
#include <mutex>
#include "GuiTerminal.hpp"
#include "NelsonHistory.hpp"
#include "QtMainWindow.h"
#include "QtTerminal.h"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "HistoryBrowser.hpp"
#include "MainGuiObject.hpp"
#include "RuntimeDispatch.hpp"
#include "AnsiSgr.hpp"
//=============================================================================
QtTerminal*
getQtTerminalInstance();
//=============================================================================
namespace {
//=============================================================================
std::mutex guiOutputMutex;
std::wstring guiOutputBuffer;
bool guiOutputFlushScheduled = false;
//=============================================================================
void
flushGuiOutputBuffer()
{
    std::wstring msg;
    {
        std::lock_guard<std::mutex> lock(guiOutputMutex);
        msg.swap(guiOutputBuffer);
        guiOutputFlushScheduled = false;
    }
    if (msg.empty()) {
        return;
    }
    QtTerminal* qtterm = getQtTerminalInstance();
    if (qtterm) {
        qtterm->outputMessage(msg);
    }
}
//=============================================================================
template <typename Func>
void
runOnGuiThread(Func func)
{
    if (Nelson::hasGuiServices() && !Nelson::isGuiThread()) {
        Nelson::callGuiSync(func);
        return;
    }
    func();
}
//=============================================================================
template <typename Result, typename Func>
Result
runOnGuiThread(Func func, Result defaultValue)
{
    Result result = defaultValue;
    runOnGuiThread([&]() { result = func(); });
    return result;
}
//=============================================================================
template <typename Func>
void
postOnGuiThread(Func func)
{
    if (Nelson::hasGuiServices() && !Nelson::isGuiThread()) {
        Nelson::postToGui(func);
        return;
    }
    func();
}
//=============================================================================
void
enqueueGuiOutput(const std::wstring& msg)
{
    bool scheduleFlush = false;
    {
        std::lock_guard<std::mutex> lock(guiOutputMutex);
        guiOutputBuffer.append(msg);
        if (!guiOutputFlushScheduled) {
            guiOutputFlushScheduled = true;
            scheduleFlush = true;
        }
    }
    if (scheduleFlush) {
        postOnGuiThread([]() { QTimer::singleShot(100, []() { flushGuiOutputBuffer(); }); });
    }
}
//=============================================================================
} // namespace
//=============================================================================
QtTerminal*
getQtTerminalInstance()
{
    QtMainWindow* mainWindow = static_cast<QtMainWindow*>(GetMainGuiObject());
    return mainWindow ? mainWindow->getQtTerminal() : nullptr;
}
//=============================================================================
QtMainWindow*
getQtMainWindowInstance()
{
    return static_cast<QtMainWindow*>(GetMainGuiObject());
}
//=============================================================================
GuiTerminal::GuiTerminal(void* qtMainW)
{
    QtMainWindow* qtMainWindow = reinterpret_cast<QtMainWindow*>(qtMainW);
    QtTerminal* qtterm = qtMainWindow ? qtMainWindow->getQtTerminal() : nullptr;
    if (qtterm) {
        qtterm->setMaxBlockCount(DEFAULT_CONSOLE_MAX_LINE_VISIBLE);
    }
}
//=============================================================================
GuiTerminal::~GuiTerminal() { }
//=============================================================================
void*
GuiTerminal::getQtPointer()
{
    return runOnGuiThread<void*>([]() { return (void*)getQtTerminalInstance(); }, nullptr);
}
//=============================================================================
std::wstring
GuiTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    if (Nelson::hasGuiServices() && !Nelson::isGuiThread()) {
        return runOnGuiThread<std::wstring>(
            [&]() { return getTextLine(prompt, bIsInput); }, std::wstring());
    }

    std::wstring line;
    flushGuiOutputBuffer();
    QtTerminal* qtterm = getQtTerminalInstance();
    if (qtterm) {
        this->diary.writeMessage(L"\n");
        this->diary.writeMessage(prompt);
        line = qtterm->getLine(prompt);
        if (line != L"\n") {
            this->diary.writeMessage(line);
            if (bIsInput) {
                if (StringHelpers::ends_with(line, L"\n")) {
                    line.pop_back();
                }
            }
        }
    }
    if (bIsInput) {
        Nelson::History::setToken(L"");
    } else {
        if (line != L"\n") {
            Nelson::History::addLine(line);
            Nelson::HistoryBrowser::addLine(line);
        }
    }
    return line;
}
//=============================================================================
std::wstring
GuiTerminal::getInput(const std::wstring& prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::string
GuiTerminal::getLine(const std::string& prompt)
{
    std::wstring wline = getLine(utf8_to_wstring(prompt));
    std::string line = wstring_to_utf8(wline);
    return line;
}
//=============================================================================
std::wstring
GuiTerminal::getLine(const std::wstring& prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
size_t
GuiTerminal::getTerminalWidth()
{
    return runOnGuiThread<size_t>(
        []() -> size_t {
            size_t width = DEFAULT_CONSOLE_WIDTH;
            QtTerminal* qtterm = getQtTerminalInstance();
            if (qtterm) {
                width = qtterm->getTerminalWidth();
            }
            return width;
        },
        DEFAULT_CONSOLE_WIDTH);
}
//=============================================================================
size_t
GuiTerminal::getTerminalHeight()
{
    return runOnGuiThread<size_t>(
        []() -> size_t {
            size_t height = DEFAULT_CONSOLE_HEIGHT;
            QtTerminal* qtterm = getQtTerminalInstance();
            if (qtterm) {
                height = qtterm->getTerminalHeight();
            }
            return height;
        },
        DEFAULT_CONSOLE_HEIGHT);
}
//=============================================================================
void
GuiTerminal::outputMessage(const std::string& msg)
{
    this->outputMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
GuiTerminal::outputMessage(const std::wstring& msg)
{
    this->diary.writeMessage(msg);
    if (msg.find(L'\b') != std::wstring::npos) {
        runOnGuiThread([&]() {
            flushGuiOutputBuffer();
            QtTerminal* qtterm = getQtTerminalInstance();
            if (qtterm) {
                qtterm->outputMessage(msg);
            }
        });
        return;
    }
    enqueueGuiOutput(msg);
}
//=============================================================================
bool
GuiTerminal::outputStyledText(const std::wstring& msg, const TextStyle& style)
{
    if (style.isPlain()) {
        outputMessage(msg);
        return false;
    }
    this->diary.writeMessage(msg);
    bool displayed = runOnGuiThread<bool>(
        [&]() {
            flushGuiOutputBuffer();
            QtTerminal* qtterm = getQtTerminalInstance();
            if (!qtterm) {
                return false;
            }
            qtterm->outputStyledText(msg, style);
            return true;
        },
        false);
    if (!displayed) {
        enqueueGuiOutput(msg);
    }
    return displayed;
}
//=============================================================================
bool
GuiTerminal::outputAnsiText(const std::wstring& msg, bool asError)
{
    if (!containsAnsiEscape(msg)) {
        if (asError) {
            errorMessage(msg);
        } else {
            outputMessage(msg);
        }
        return false;
    }

    if (!asError) {
        std::wstring plain = stripAnsiSgr(msg);
        this->diary.writeMessage(plain);
        bool displayed = runOnGuiThread<bool>(
            [&]() {
                flushGuiOutputBuffer();
                QtTerminal* qtterm = getQtTerminalInstance();
                if (!qtterm) {
                    return false;
                }
                qtterm->outputAnsiText(msg, false);
                return true;
            },
            false);
        if (!displayed) {
            enqueueGuiOutput(plain);
        }
        return displayed;
    }

    std::wstring fallbackPlain = stripAnsiSgr(msg + L"\n");
    bool displayed = runOnGuiThread<bool>(
        [&]() {
            flushGuiOutputBuffer();
            QtTerminal* qtterm = getQtTerminalInstance();
            if (!qtterm) {
                return false;
            }
            std::wstring styledMsg = msg + L"\n";
            std::wstring plain = stripAnsiSgr(styledMsg);
            if (qtterm->isAtPrompt()) {
                styledMsg = L"\n" + styledMsg;
                plain = L"\n" + plain;
                qtterm->sendReturnKey();
            }
            qtterm->outputAnsiText(styledMsg, true);
            this->diary.writeMessage(plain);
            return true;
        },
        false);
    if (!displayed) {
        this->diary.writeMessage(fallbackPlain);
        enqueueGuiOutput(fallbackPlain);
    }
    return displayed;
}
//=============================================================================
bool
GuiTerminal::outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
    const std::wstring& plainFallback, const std::wstring& contextName, long long contextValue)
{
    if (!plainFallback.empty()) {
        this->diary.writeMessage(plainFallback);
    }
    bool displayed = runOnGuiThread<bool>(
        [&]() {
            flushGuiOutputBuffer();
            QtTerminal* qtterm = getQtTerminalInstance();
            if (!qtterm) {
                return false;
            }
            qtterm->insertExpandableText(label, expandedText, contextName, contextValue);
            return true;
        },
        false);
    if (!displayed && !plainFallback.empty()) {
        enqueueGuiOutput(plainFallback);
    }
    return displayed;
}
//=============================================================================
bool
GuiTerminal::supportsTruncatedMatrixDisplay()
{
    return true;
}
//=============================================================================
bool
GuiTerminal::supportsHyperlinks()
{
    return true;
}
//=============================================================================
bool
GuiTerminal::outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
    const std::wstring& expandedText, const std::wstring& variableName)
{
    this->diary.writeMessage(expandedText);
    bool displayed = runOnGuiThread<bool>(
        [&]() {
            flushGuiOutputBuffer();
            QtTerminal* qtterm = getQtTerminalInstance();
            if (!qtterm) {
                return false;
            }
            qtterm->insertTruncatedMatrix(truncatedText, label, expandedText, variableName);
            return true;
        },
        false);
    if (!displayed) {
        enqueueGuiOutput(expandedText);
    }
    return displayed;
}
//=============================================================================
void
GuiTerminal::errorMessage(const std::string& msg)
{
    this->errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
GuiTerminal::errorMessage(const std::wstring& msg)
{
    runOnGuiThread([&]() {
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            std::wstring _msg = msg + L"\n";
            if (qtterm->isAtPrompt()) {
                _msg = L"\n" + _msg;
                qtterm->sendReturnKey();
            }
            qtterm->errorMessage(_msg);
            this->diary.writeMessage(_msg);
        }
    });
}
//=============================================================================
void
GuiTerminal::warningMessage(const std::string& msg)
{
    this->warningMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
GuiTerminal::warningMessage(const std::wstring& msg)
{
    runOnGuiThread([&]() {
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            std::wstring _msg = msg + L"\n";
            if (qtterm->isAtPrompt()) {
                _msg = L"\n" + _msg;
                qtterm->sendReturnKey();
            }
            qtterm->warningMessage(_msg);
            this->diary.writeMessage(_msg);
        }
    });
}
//=============================================================================
void
GuiTerminal::clearTerminal()
{
    runOnGuiThread([]() {
        flushGuiOutputBuffer();
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            qtterm->clearTerminal();
        }
    });
}
//=============================================================================
void
GuiTerminal::banner()
{
    runOnGuiThread([]() {
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            qtterm->banner();
        }
    });
}
//=============================================================================
bool
GuiTerminal::insertHtml(const std::wstring& msg)
{
    this->diary.writeMessage(msg);
    postOnGuiThread([msg]() {
        flushGuiOutputBuffer();
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            qtterm->insertHtml(msg);
        }
    });
    return true;
}
//=============================================================================
int
GuiTerminal::getBufferScreenLine()
{
    return runOnGuiThread<int>(
        []() {
            QtTerminal* qtterm = getQtTerminalInstance();
            if (qtterm) {
                return qtterm->maxBlockCount();
            }
            return -1;
        },
        -1);
}
//=============================================================================
void
GuiTerminal::setBufferScreenLine(int newMax)
{
    if (newMax < 0) {
        return;
    }
    runOnGuiThread([newMax]() {
        QtTerminal* qtterm = getQtTerminalInstance();
        if (qtterm) {
            // 0 means unlimited scrollback (QTextDocument convention).
            qtterm->setMaxBlockCount(newMax);
        }
    });
}
//=============================================================================
bool
GuiTerminal::isAtPrompt()
{
    return runOnGuiThread<bool>(
        []() -> bool {
            QtTerminal* qtterm = getQtTerminalInstance();
            if (qtterm) {
                return qtterm->isAtPrompt() != 0;
            }
            return false;
        },
        false);
}
//=============================================================================
void
GuiTerminal::interruptGetLineByEvent()
{
    runOnGuiThread([]() {
        QtMainWindow* qtMainWindow = getQtMainWindowInstance();
        QEvent* qEvent = nullptr;
        try {
            qEvent = new QEvent(QEvent::None);
            qApp->postEvent(qtMainWindow, qEvent, Qt::HighEventPriority);
        } catch (const std::bad_alloc&) {
            qEvent = nullptr;
        }
    });
}
//=============================================================================
