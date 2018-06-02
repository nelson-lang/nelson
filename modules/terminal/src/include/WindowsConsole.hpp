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
    isCTRLPressed(INPUT_RECORD irBuffer);
    bool
    isALTPressed(INPUT_RECORD irBuffer);
    wchar_t
    actionControlKey(void);
    bool
    isCTRL_VKEY(int VKEY);
    void
    pasteClipBoard(void);
    void
    clearClipBoard(void);
    bool
    copyToClipBoard(std::wstring txt);
    bool bHaveHisOwnWindow;
    bool bWithColors;
    std::wstring
    getTextLine(std::wstring prompt, bool bIsInput);
    bool atPrompt;

public:
    WindowsConsole(bool _bWithColors = true);
    ~WindowsConsole();

    std::wstring
    getInput(std::wstring prompt);
    std::wstring
    getLine(std::wstring prompt);
    std::string
    getLine(std::string prompt);

    /**
     *  Return the width of the current "terminal" in
     *  characters.
     */
    size_t
    getTerminalWidth();
    size_t
    getTerminalHeight();
    /**
     *  Output the following text message.
     */
    void
    outputMessage(std::string msg);
    void
    outputMessage(std::wstring msg);

    /**
     *  Output the following error message.
     */
    void
    errorMessage(std::string msg);
    void
    errorMessage(std::wstring msg);

    /**
     *  Output the following warning message.
     */
    void
    warningMessage(std::string msg);
    void
    warningMessage(std::wstring msg);

    void
    clearTerminal();

    bool
    hasHisOwnWindow();

    bool
    setConsoleTitle(std::wstring title);
    std::wstring
    getConsoleTitle();

    bool
    isAtPrompt();
};
