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
#include "GuiTerminal.hpp"
#include "NelsonHistory.hpp"
#include "QtMainWindow.h"
#include "QtTerminal.h"
#include "characters_encoding.hpp"
#include <boost/algorithm/string/predicate.hpp>
//=============================================================================
QtTerminal* qtterm = nullptr;
//=============================================================================
GuiTerminal::GuiTerminal(void* qtMainW)
{
    QtMainWindow* mw = reinterpret_cast<QtMainWindow*>(qtMainW);
    qtterm = mw->getQtTerminal();
}
//=============================================================================
GuiTerminal::~GuiTerminal() {}
//=============================================================================
std::wstring
GuiTerminal::getTextLine(std::wstring prompt, bool bIsInput)
{
    std::wstring line;
    if (qtterm) {
        this->diary.writeMessage(L"\n");
        this->diary.writeMessage(prompt);
        line = qtterm->getLine(prompt);
        if (line != L"\n") {
            this->diary.writeMessage(line);
            if (bIsInput) {
                if (boost::algorithm::ends_with(line, L"\n")) {
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
        }
    }
    return line;
}
//=============================================================================
std::wstring
GuiTerminal::getInput(std::wstring prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::string
GuiTerminal::getLine(std::string prompt)
{
    std::wstring wline = getLine(utf8_to_wstring(prompt));
    std::string line = wstring_to_utf8(wline);
    return line;
}
//=============================================================================
std::wstring
GuiTerminal::getLine(std::wstring prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
size_t
GuiTerminal::getTerminalWidth()
{
    size_t width = 80;
    if (qtterm) {
        width = qtterm->getTerminalWidth();
    }
    return width;
}
//=============================================================================
void
GuiTerminal::outputMessage(std::string msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->outputMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::outputMessage(std::wstring msg)
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
GuiTerminal::errorMessage(std::string msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->errorMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::errorMessage(std::wstring msg)
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
GuiTerminal::warningMessage(std::string msg)
{
    if (qtterm) {
        std::wstring wmsg = utf8_to_wstring(msg);
        this->warningMessage(wmsg);
    }
}
//=============================================================================
void
GuiTerminal::warningMessage(std::wstring msg)
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
GuiTerminal::insertHtml(std::wstring msg)
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
