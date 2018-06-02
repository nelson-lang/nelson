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
#include "WindowsConsole.hpp"
#include "ActionMenu.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "LineManager.hpp"
#include "NelsonHistory.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "characters_encoding.hpp"
#include <Windows.h>
#include <boost/algorithm/string/predicate.hpp>
#include <csignal>
#include <fcntl.h> // _O_UTF16TEXT
#include <io.h> // _setmode
//=============================================================================
#ifdef CR_1
#undef CR_1
#endif
#define CR_1 L'\n'
//=============================================================================
#ifdef CR_2
#undef CR_2
#endif
#define CR_2 L'\r'
//=============================================================================
static bool bCONTROLC = false;
static bool bInterruptGetChar = false;
//=============================================================================
static void
ControlC_Command(void)
{
    bCONTROLC = true;
    Nelson::sigInterrupt(1);
}
//=============================================================================
static void
AltF4_Command(void)
{
    Nelson::doExit();
}
//=============================================================================
static void
Help_Command(void)
{
    Nelson::doHelp();
}
//=============================================================================
static void
Pause_Command(void)
{
    Nelson::doPause();
}
//=============================================================================
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
        AltF4_Command();
    }
        return FALSE;
    }
    return FALSE;
}
//=============================================================================
WindowsConsole::WindowsConsole(bool _bWithColors)
{
    atPrompt = false;
    HWND hwnd = GetConsoleWindow();
    // commented: workaround for fwrite(1,110) crash on windows
    // int old =_setmode(_fileno(stdin), _O_U16TEXT);
    // old = _setmode(_fileno(stdout), _O_U16TEXT);
    // old = _setmode(_fileno(stderr), _O_U16TEXT);
    Win32InputStream = GetStdHandle(STD_INPUT_HANDLE);
    GetConsoleMode(Win32InputStream, &OldWin32Mode);
    SetConsoleMode(Win32InputStream,
        ENABLE_PROCESSED_INPUT | ENABLE_QUICK_EDIT_MODE | ENABLE_INSERT_MODE
            | ENABLE_EXTENDED_FLAGS);
    Win32OutputStream = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
    STARTUPINFOW si = { sizeof(si) };
    GetStartupInfoW(&si);
    bHaveHisOwnWindow = (si.dwFlags & STARTF_USESHOWWINDOW);
    if (bHaveHisOwnWindow) {
        this->setConsoleTitle(L"Nelson");
    }
    if (_bWithColors && bHaveHisOwnWindow) {
        bWithColors = _bWithColors;
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
        lineObj.usesColors();
        HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        COORD Coord;
        DWORD cWritten;
        Coord.X = 0;
        Coord.Y = 0;
        FillConsoleOutputAttribute(hConsole,
            BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY,
            csbi.dwSize.X * csbi.dwSize.Y, Coord, &cWritten);
        SetConsoleTextAttribute(
            hConsole, BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY);
        lineObj.usesColors();
    } else {
        bWithColors = false;
    }
    SetFocus(hwnd);
}
//=============================================================================
WindowsConsole::~WindowsConsole()
{
    SetConsoleMode(Win32InputStream, OldWin32Mode);
    HWND hwnd = GetConsoleWindow();
    HMENU hmenu = GetSystemMenu(hwnd, FALSE);
    EnableMenuItem(hmenu, SC_CLOSE, MF_ENABLED);
    bHaveHisOwnWindow = false;
}
//=============================================================================
std::wstring
WindowsConsole::getTextLine(std::wstring prompt, bool bIsInput)
{
    atPrompt = true;
    std::wstring cmdline = L"";
    lineObj.newLine();
    lineObj.setCurrentPrompt(prompt);
    lineObj.displayPrompt();
    if (prompt != L"") {
        diary.writeMessage(prompt);
    }
    for (;;) {
        bool bIsAction = false;
        wchar_t cur_char = getCharacter(bIsAction);
        if (bIsAction) {
            if (bIsInput) {
                Nelson::History::setToken(L"");
                if (boost::algorithm::ends_with(cmdline, L"\n")) {
                    cmdline.pop_back();
                }
            } else {
                lineObj.clearCurrentLine(false);
            }
            atPrompt = false;
            if (cmdline.empty()) {
                cmdline = L"\n";
            }
            return cmdline;
        } else if ((cur_char == CR_1) || (cur_char == CR_2)) {
            cmdline = lineObj.getCurrentLine();
            lineObj.putCharacter(L'\n', LineManager::STANDARD_INPUT);
            diary.writeMessage(cmdline + L"\n");
            if (!bIsInput) {
                Nelson::History::addLine(cmdline);
            }
            if (bIsInput) {
                Nelson::History::setToken(L"");
                if (boost::algorithm::ends_with(cmdline, L"\n")) {
                    cmdline.pop_back();
                }
            }
            if (cmdline.empty()) {
                cmdline = L"\n";
            }
            atPrompt = false;
            return cmdline;
        } else {
            lineObj.putCharacter(cur_char, LineManager::STANDARD_INPUT);
            lineObj.addCharacterCurrentLine(cur_char);
            std::wstring curline = lineObj.getCurrentLine();
            if (!bIsInput) {
                Nelson::History::setToken(curline);
            }
        }
    }
    atPrompt = false;
    return cmdline;
}
//=============================================================================
std::wstring
WindowsConsole::getInput(std::wstring prompt)
{
    return getTextLine(prompt, true);
}
//=============================================================================
std::wstring
WindowsConsole::getLine(std::wstring prompt)
{
    return getTextLine(prompt, false);
}
//=============================================================================
std::string
WindowsConsole::getLine(std::string prompt)
{
    return wstring_to_utf8(getLine(utf8_to_wstring(prompt)));
}
//=============================================================================
size_t
WindowsConsole::getTerminalWidth()
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    return csbi.dwSize.X;
}
//=============================================================================
size_t
WindowsConsole::getTerminalHeight()
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    return csbi.dwSize.Y;
}
//=============================================================================
void
WindowsConsole::outputMessage(std::string msg)
{
    outputMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
WindowsConsole::outputMessage(std::wstring msg)
{
    std::wstring _msg = msg;
    if (atPrompt) {
        lineObj.clearCurrentLine(false);
        atPrompt = false;
        bInterruptGetChar = true;
    }
    lineObj.printCharacters(_msg, lineObj.STANDARD_OUTPUT);
    diary.writeMessage(_msg);
}
//=============================================================================
void
WindowsConsole::errorMessage(std::string msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
WindowsConsole::errorMessage(std::wstring msg)
{
    std::wstring _msg = msg + L"\n";
    if (atPrompt) {
        _msg = L"\n" + msg;
        atPrompt = false;
        bInterruptGetChar = true;
    }
    lineObj.printCharacters(_msg, lineObj.ERROR_OUTPUT);
    diary.writeMessage(_msg);
}
//=============================================================================
void
WindowsConsole::warningMessage(std::string msg)
{
    warningMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
WindowsConsole::warningMessage(std::wstring msg)
{
    std::wstring _msg = msg + L"\n";
    if (atPrompt) {
        _msg = L"\n" + msg;
        atPrompt = false;
        bInterruptGetChar = true;
    }
    lineObj.printCharacters(_msg, lineObj.WARNING_OUTPUT);
    diary.writeMessage(_msg);
}
//=============================================================================
bool
WindowsConsole::isCTRLPressed(INPUT_RECORD irBuffer)
{
    return ((irBuffer.Event.KeyEvent.dwControlKeyState & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
        != 0);
}
//=============================================================================
bool
WindowsConsole::isALTPressed(INPUT_RECORD irBuffer)
{
    return (
        (irBuffer.Event.KeyEvent.dwControlKeyState & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED)) != 0);
}
//=============================================================================
wchar_t
WindowsConsole::keysEventsFilter(INPUT_RECORD irBuffer, bool& bIsChar, bool& bIsAction)
{
    DWORD n = 0;
    wchar_t ch = 0;
    bIsChar = false;
    bIsAction = false;
    Win32InputStream = GetStdHandle(STD_INPUT_HANDLE);
    SetConsoleMode(Win32InputStream,
        ENABLE_PROCESSED_INPUT | ENABLE_QUICK_EDIT_MODE | ENABLE_INSERT_MODE
            | ENABLE_EXTENDED_FLAGS);
    if (irBuffer.Event.KeyEvent.bKeyDown) {
        if (irBuffer.Event.KeyEvent.dwControlKeyState) {
            if (isCTRLPressed(irBuffer)) {
                wchar_t c = actionControlKey();
                if (c) {
                    ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
                    bIsChar = true;
                    return c;
                } else {
                    if (irBuffer.Event.KeyEvent.uChar.UnicodeChar != L'\0') {
                        ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
                        c = irBuffer.Event.KeyEvent.uChar.UnicodeChar;
                        if ((c > 0) && !iswcntrl(c)) {
                            bIsChar = true;
                            return c;
                        }
                    } else {
                        ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
                    }
                }
                bIsChar = false;
                return 0;
            }
            if (isALTPressed(irBuffer)) {
                if (irBuffer.Event.KeyEvent.uChar.AsciiChar != '\0') {
                    ::ReadConsoleW(Win32InputStream, &ch, 1, &n, NULL);
                    bIsChar = true;
                    return ch;
                } else {
                    WORD vk = 0;
                    ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
                    vk = irBuffer.Event.KeyEvent.wVirtualKeyCode;
                    switch (vk) {
                    case VK_F4:
                        AltF4_Command();
                        bIsAction = true;
                        return 0;
                    default:
                        break;
                    }
                }
            }
        }
        if (irBuffer.Event.KeyEvent.uChar.UnicodeChar != L'\0') {
            wchar_t _ch[2];
            ::ReadConsoleW(Win32InputStream, &_ch, 1, &n, NULL);
            ch = _ch[0];
            switch (ch) {
            case VK_TAB:
                break;
            case VK_BACK:
                lineObj.deletePreviousChar();
                break;
            default: {
                if (!iswcntrl(ch) || (ch == CR_1) || (ch == CR_2)) {
                    bIsChar = true;
                    return ch;
                }
            } break;
            }
        } else {
            WORD vk = 0;
            ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
            vk = irBuffer.Event.KeyEvent.wVirtualKeyCode;
            switch (vk) {
            case VK_F1:
            case VK_HELP:
                Help_Command();
                bIsAction = true;
                return 0;
                break;
            case VK_F2:
                clearTerminal();
                lineObj.displayPrompt();
                break;
            case VK_LEFT:
                lineObj.moveBackSingleChar();
                break;
            case VK_RIGHT:
                lineObj.moveForwardSingleChar();
                break;
            case VK_UP:
                lineObj.moveBackHistory();
                break;
            case VK_DOWN:
                lineObj.moveForwardHistory();
                break;
            case VK_DELETE: {
                lineObj.deleteCurrentChar();
                std::wstring curline = lineObj.getCurrentLine();
                Nelson::History::setToken(curline);
            } break;
            case VK_HOME:
                lineObj.moveBeginningLine();
                break;
            case VK_END:
                lineObj.moveEndLine();
                break;
            case VK_PAUSE:
                Pause_Command();
                bIsAction = true;
                return 0;
            default:
                break;
            }
        }
    } else {
        ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
    }
    return ch;
}
//=============================================================================
static Nelson::Evaluator* eval = nullptr;
//=============================================================================
wchar_t
WindowsConsole::getCharacter(bool& bIsAction)
{
    wchar_t wch = L'\0';
    INPUT_RECORD irBuffer;
    DWORD n = 0;
    bCONTROLC = false;
    do {
        Win32InputStream = GetStdHandle(STD_INPUT_HANDLE);
        SetConsoleMode(Win32InputStream,
            ENABLE_PROCESSED_INPUT | ENABLE_QUICK_EDIT_MODE | ENABLE_INSERT_MODE
                | ENABLE_EXTENDED_FLAGS);
        if (bHaveHisOwnWindow) {
            this->setConsoleTitle(L"Nelson");
        }
        HWND hwnd = GetConsoleWindow();
        HMENU hmenu = GetSystemMenu(hwnd, FALSE);
        EnableMenuItem(hmenu, SC_CLOSE, MF_GRAYED);
        bIsAction = false;
        while (TRUE) {
            ::WaitForSingleObject(Win32InputStream, 30);
            DWORD nbEventsAvailable = 0;
            GetNumberOfConsoleInputEvents(Win32InputStream, &nbEventsAvailable);
            if (nbEventsAvailable > 0) {
                PeekConsoleInputW(Win32InputStream, &irBuffer, 1, &nbEventsAvailable);
                break;
            } else {
                ProcessEventsDynamicFunctionWithoutWait();
            }
            if (eval == nullptr) {
                void* veval = GetNelsonMainEvaluatorDynamicFunction();
                eval = (Nelson::Evaluator*)veval;
            }
            if (!eval->commandQueue.isEmpty() || bInterruptGetChar) {
                bIsAction = true;
                bInterruptGetChar = false;
                return L'\n';
            }
        }
        /*
        ::WaitForSingleObject(Win32InputStream, INFINITE);
        ::PeekConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
        */
        switch (irBuffer.EventType) {
        case KEY_EVENT: {
            bool bIsChar = false;
            wchar_t _wch = keysEventsFilter(irBuffer, bIsChar, bIsAction);
            if (bIsChar || bIsAction) {
                return _wch;
            }
        } break;
        case MENU_EVENT: {
            ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
        } break;
        case MOUSE_EVENT:
        case WINDOW_BUFFER_SIZE_EVENT:
        case FOCUS_EVENT:
        default:
            ::ReadConsoleInputW(Win32InputStream, &irBuffer, 1, &n);
            break;
        }
    } while (TRUE && !bCONTROLC);
    if (bCONTROLC) {
        bCONTROLC = false;
        bIsAction = true;
        wch = 0;
    }
    return wch;
}
//=============================================================================
wchar_t
WindowsConsole::actionControlKey(void)
{
    if (isCTRL_VKEY(L'X')) {
        // managed by the signal
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, GetCurrentProcessId());
        return 0;
    } else if (isCTRL_VKEY(L'C')) {
        // managed by the signal
        GenerateConsoleCtrlEvent(CTRL_C_EVENT, GetCurrentProcessId());
        return 0;
    } else if (isCTRL_VKEY('A')) /* moves to the beginning of the line */
    {
        lineObj.moveBeginningLine();
    } else if (isCTRL_VKEY('B')) /* moves back a single character */
    {
        lineObj.moveBackSingleChar();
    } else if (isCTRL_VKEY('D')) /* deletes the current character */
    {
        lineObj.deleteCurrentChar();
        std::wstring curline = lineObj.getCurrentLine();
        Nelson::History::setToken(curline);
    } else if (isCTRL_VKEY('E')) /* moves to the end of the line */
    {
        lineObj.moveEndLine();
    } else if (isCTRL_VKEY('F')) /* moves forward a single character */
    {
        lineObj.moveForwardSingleChar();
    } else if (isCTRL_VKEY('H')) /* delete the previous character */
    {
        lineObj.deletePreviousChar();
    } else if (isCTRL_VKEY('K')) /* kills from current position to the end of line */
    {
        lineObj.killCurrentPositionToEndLine();
    } else if (isCTRL_VKEY('N')) /* moves forward through history */
    {
        lineObj.moveForwardHistory();
    } else if (isCTRL_VKEY('P')) /* moves back through history */
    {
        lineObj.moveBackHistory();
    } else if (isCTRL_VKEY('R') || isCTRL_VKEY('L')) /* redraw line in case it gets trashed */
    {
        lineObj.redrawLine();
    } else if (isCTRL_VKEY('U')) /* kills the entire line */
    {
        lineObj.clearCurrentLine();
    } else if (isCTRL_VKEY('V')) {
        pasteClipBoard();
    } else if (isCTRL_VKEY('W')) /* kills last word */
    {
        lineObj.killLastWord();
    } else if (isCTRL_VKEY(VK_TAB) || isCTRL_VKEY(VK_SPACE)) /* Completion */
    {
    } else if (isCTRL_VKEY(VK_LEFT)) /* */
    {
        lineObj.moveBackSingleWord();
    } else if (isCTRL_VKEY(VK_RIGHT)) /* */
    {
        lineObj.moveForwardSingleWord();
    }
    return 0;
}
//=============================================================================
bool
WindowsConsole::isCTRL_VKEY(int VKEY)
{
    return (GetKeyState(VKEY) & 0x80) == 0 ? false : true;
}
//=============================================================================
void
WindowsConsole::clearTerminal()
{
    COORD coord;
    DWORD written;
    CONSOLE_SCREEN_BUFFER_INFO info;
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    coord.X = 0;
    coord.Y = 0;
    ::GetConsoleScreenBufferInfo(hConsole, &info);
    ::FillConsoleOutputCharacterW(hConsole, L' ', info.dwSize.X * info.dwSize.Y, coord, &written);
    coord.X = 0;
    coord.Y = 0;
    ::SetConsoleCursorPosition(hConsole, coord);
    if (bWithColors) {
        coord.X = 0;
        coord.Y = 0;
        FillConsoleOutputAttribute(hConsole,
            BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY,
            info.dwSize.X * info.dwSize.Y, coord, &written);
        SetConsoleTextAttribute(
            hConsole, BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY);
    }
}
//=============================================================================
void
WindowsConsole::pasteClipBoard(void)
{
    if (IsClipboardFormatAvailable(CF_UNICODETEXT) || IsClipboardFormatAvailable(CF_TEXT)) {
        HGLOBAL hGMem = nullptr;
        int typeClipboard = 0;
        if (IsClipboardFormatAvailable(CF_UNICODETEXT)) {
            typeClipboard = CF_UNICODETEXT;
        } else {
            typeClipboard = CF_TEXT;
        }
        if (::OpenClipboard(NULL)) {
            hGMem = ::GetClipboardData(typeClipboard);
            if (hGMem) {
                std::wstring currentLine = lineObj.getCurrentLine();
                std::wstring newLine = currentLine;
                if (typeClipboard == CF_UNICODETEXT) {
                    LPWSTR lpMem = (LPWSTR)::GlobalLock(hGMem);
                    if (lpMem) {
                        newLine = newLine + std::wstring(lpMem);
                    }
                } else {
                    LPSTR lpMem = (LPSTR)::GlobalLock(hGMem);
                    if (lpMem) {
                        newLine = newLine + utf8_to_wstring(std::string(lpMem));
                    }
                }
                ::GlobalUnlock(hGMem);
                lineObj.clearCurrentLine();
                lineObj.copyLine(newLine);
            }
            ::CloseClipboard();
        }
    }
}
//=============================================================================
void
WindowsConsole::clearClipBoard(void)
{
    OpenClipboard(NULL);
    EmptyClipboard();
    CloseClipboard();
}
//=============================================================================
bool
WindowsConsole::copyToClipBoard(std::wstring txt)
{
    bool bRes = false;
    size_t lentxt = txt.size();
    if (lentxt > 0) {
        OpenClipboard(NULL);
        EmptyClipboard();
        HGLOBAL hglbCopy = GlobalAlloc(GMEM_MOVEABLE, ((lentxt + 1) * sizeof(wchar_t)));
        if (hglbCopy) {
            wchar_t* lptstrCopy = (wchar_t*)GlobalLock(hglbCopy);
            if (lptstrCopy) {
                memcpy(lptstrCopy, txt.c_str(), lentxt * sizeof(wchar_t));
                lptstrCopy[lentxt] = (wchar_t)0; // null character
                GlobalUnlock(hglbCopy);
                SetClipboardData(CF_UNICODETEXT, hglbCopy);
                bRes = true;
            }
        }
        CloseClipboard();
    }
    return bRes;
}
//=============================================================================
bool
WindowsConsole::hasHisOwnWindow()
{
    return bHaveHisOwnWindow;
}
//=============================================================================
bool
WindowsConsole::setConsoleTitle(std::wstring title)
{
    return ::SetConsoleTitleW(title.c_str()) == FALSE ? false : true;
}
//=============================================================================
std::wstring
WindowsConsole::getConsoleTitle()
{
    wchar_t title[4096];
    GetConsoleTitleW(title, 4096);
    return std::wstring(title);
}
//=============================================================================
bool
WindowsConsole::isAtPrompt()
{
    return atPrompt;
}
//=============================================================================
