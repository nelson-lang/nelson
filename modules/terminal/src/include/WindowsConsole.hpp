//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Interface.hpp"
#include "LineManager.hpp"
#include "nlsTerminal_exports.h"
#include <Windows.h>
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSTERMINAL_IMPEXP WindowsConsole : public Interface
{
private:
    HANDLE Win32OutputStream;
    HANDLE Win32InputStream;
    DWORD OldWin32Mode;
    wchar_t
    getCharacter(bool& bIsAction);
    LineManager lineObj;
    wchar_t
    keysEventsFilter(INPUT_RECORD irBuffer, bool& bIsChar, bool& bIsAction);
    bool
    mouseEventsFilter(INPUT_RECORD irBuffer);
    bool
    isCTRLPressed(const INPUT_RECORD& irBuffer);
    bool
    isALTPressed(const INPUT_RECORD& irBuffer);
    wchar_t
    actionControlKey();
    bool
    isCTRL_VKEY(int VKEY);
    void
    pasteClipBoard();
    void
    clearClipBoard();
    bool
    copyToClipBoard(const std::wstring& txt);
    bool bHaveHisOwnWindow;
    bool bWithColors;
    std::wstring
    getTextLine(const std::wstring& prompt, bool bIsInput);
    bool atPrompt;

public:
    WindowsConsole(bool _bWithColors = true);
    ~WindowsConsole() override;

    std::wstring
    getInput(const std::wstring& prompt) override;
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::string
    getLine(const std::string& prompt) override;

    /**
     *  Return the width of the current "terminal" in
     *  characters.
     */
    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight();
    /**
     *  Output the following text message.
     */
    void
    outputMessage(const std::string& msg) override;
    void
    outputMessage(const std::wstring& msg) override;

    /**
     *  Output the following error message.
     */
    void
    errorMessage(const std::string& msg) override;
    void
    errorMessage(const std::wstring& msg) override;

    /**
     *  Output the following warning message.
     */
    void
    warningMessage(const std::string& msg) override;
    void
    warningMessage(const std::wstring& msg) override;

    void
    clearTerminal() override;

    bool
    hasHisOwnWindow();

    bool
    setConsoleTitle(const std::wstring& title);
    std::wstring
    getConsoleTitle();

    bool
    isAtPrompt() override;

    void
    interruptGetLineByEvent() override;
};
