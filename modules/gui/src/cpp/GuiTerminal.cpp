//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include "GuiTerminal.hpp"
#include "NelsonHistory.hpp"
#include "QtMainWindow.h"
#include "QtTerminal.h"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "HistoryBrowser.hpp"
//=============================================================================
static QtTerminal* qtterm = nullptr;
static QtMainWindow* qtMainWindow = nullptr;
//=============================================================================
GuiTerminal::GuiTerminal(void* qtMainW)
{
    qtMainWindow = reinterpret_cast<QtMainWindow*>(qtMainW);
    qtterm = qtMainWindow->getQtTerminal();
    qtterm->setMaxBlockCount(DEFAULT_CONSOLE_MAX_LINE_VISIBLE);
}
//=============================================================================
GuiTerminal::~GuiTerminal()
{
    qtterm = nullptr;
    qtMainWindow = nullptr;
}
//=============================================================================
void*
GuiTerminal::getQtPointer()
{
    return (void*)qtterm;
}
//=============================================================================
std::wstring
GuiTerminal::getTextLine(const std::wstring& prompt, bool bIsInput)
{
    std::wstring line;
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
    size_t width = DEFAULT_CONSOLE_WIDTH;
    if (qtterm) {
        width = qtterm->getTerminalWidth();
    }
    return width;
}
//=============================================================================
size_t
GuiTerminal::getTerminalHeight()
{
    size_t width = DEFAULT_CONSOLE_HEIGHT;
    if (qtterm) {
        width = qtterm->getTerminalHeight();
    }
    return width;
}
//=============================================================================

void
GuiTerminal::outputMessage(const std::string& msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->outputMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::outputMessage(const std::wstring& msg)
{
    if (qtterm) {
        std::wstring _msg = msg;
        if (qtterm->isAtPrompt()) {
            qtterm->clearLine();
            qtterm->sendReturnKey();
        }
        qtterm->outputMessage(_msg);
        this->diary.writeMessage(_msg);
    }
}
//=============================================================================
void
GuiTerminal::errorMessage(const std::string& msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->errorMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::errorMessage(const std::wstring& msg)
{
    if (qtterm) {
        std::wstring _msg = msg + L"\n";
        if (qtterm->isAtPrompt()) {
            _msg = L"\n" + _msg;
            qtterm->sendReturnKey();
        }
        qtterm->errorMessage(_msg);
        this->diary.writeMessage(_msg);
    }
}
//=============================================================================
void
GuiTerminal::warningMessage(const std::string& msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->warningMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::warningMessage(const std::wstring& msg)
{
    if (qtterm) {
        std::wstring _msg = msg + L"\n";
        if (qtterm->isAtPrompt()) {
            _msg = L"\n" + _msg;
            qtterm->sendReturnKey();
        }
        qtterm->warningMessage(_msg);
        this->diary.writeMessage(_msg);
    }
}
//=============================================================================
void
GuiTerminal::clearTerminal()
{
    if (qtterm) {
        qtterm->clearTerminal();
    }
}
//=============================================================================
void
GuiTerminal::banner()
{
    if (qtterm) {
        qtterm->banner();
    }
}
//=============================================================================
void
GuiTerminal::insertHtml(const std::wstring& msg)
{
    if (qtterm) {
        qtterm->insertHtml(msg);
        this->diary.writeMessage(msg);
    }
}
//=============================================================================
int
GuiTerminal::getBufferScreenLine()
{
    if (qtterm) {
        return qtterm->getMaxBlockCount();
    }
    return -1;
}
//=============================================================================
void
GuiTerminal::setBufferScreenLine(int newMax)
{
}
//=============================================================================
bool
GuiTerminal::isAtPrompt()
{
    if (qtterm) {
        return qtterm->isAtPrompt();
    }
    return false;
}
//=============================================================================
void
GuiTerminal::interruptGetLineByEvent()
{
    QEvent* qEvent = nullptr;
    try {
        qEvent = new QEvent(QEvent::None);
        qApp->postEvent(qtMainWindow, qEvent, Qt::HighEventPriority);
    } catch (const std::bad_alloc&) {
        qEvent = nullptr;
    }
}
//=============================================================================
