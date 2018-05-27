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
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "LineManager.hpp"
#include "NelsonHistory.hpp"
#include <Intsafe.h>
#include <Windows.h>
//=============================================================================
LineManager::LineManager()
{
    current_line_buffer.reserve(10);
    currentPrompt = L"";
    /* current position of the cursor */
    cur_pos = 0;
    max_pos = 0;
    bUsesColors = false;
}
//=============================================================================
LineManager::~LineManager()
{
    current_line_buffer.clear();
    currentPrompt = L"";
    cur_pos = 0;
    max_pos = 0;
    bUsesColors = false;
}
//=============================================================================
void
LineManager::usesColors()
{
    bUsesColors = true;
}
//=============================================================================
std::wstring
LineManager::getCurrentLine(void)
{
    boost::container::vector<wchar_t>::iterator itend = current_line_buffer.begin() + max_pos;
    std::wstring line(current_line_buffer.begin(), itend);
    return line;
}
//=============================================================================
std::wstring
LineManager::getLineBeforeCaret(void)
{
    boost::container::vector<wchar_t>::iterator itend = current_line_buffer.begin() + max_pos;
    std::wstring line(current_line_buffer.begin(), itend);
    if (cur_pos >= current_line_buffer.size()) {
        current_line_buffer.push_back(L'\0');
    }
    line[cur_pos] = L'\0';
    return line;
}
//=============================================================================
std::wstring
LineManager::getLineAfterCaret(void)
{
    boost::container::vector<wchar_t>::iterator itfirst = current_line_buffer.begin() + cur_pos;
    boost::container::vector<wchar_t>::iterator itend = current_line_buffer.begin() + max_pos;
    std::wstring line(itfirst, itend);
    if (cur_pos != max_pos) {
        line[(max_pos - cur_pos) + 1] = L'\0';
    }
    return line;
}
//=============================================================================
void
LineManager::moveBeginningLine(void)
{
    while (cur_pos > 0) {
        cur_pos -= 1;
        backSpace();
    }
}
//=============================================================================
void
LineManager::moveEndLine(void)
{
    boost::container::vector<wchar_t>::iterator itbegin = current_line_buffer.begin() + cur_pos;
    boost::container::vector<wchar_t>::iterator itend = current_line_buffer.begin() + max_pos;
    std::wstring line(itbegin, itend);
    cur_pos = max_pos;
    printCharacters(line, LineManager::STANDARD_INPUT);
}
//=============================================================================
void
LineManager::moveBackSingleChar(void)
{
    if (cur_pos > 0) {
        cur_pos -= 1;
        backSpace();
    }
}
//=============================================================================
void
LineManager::moveForwardSingleChar(void)
{
    if (cur_pos < max_pos) {
        putCharacter(current_line_buffer[cur_pos], LineManager::STANDARD_INPUT);
        cur_pos += 1;
    }
}
//=============================================================================
void
LineManager::moveBackSingleWord(void)
{
    while ((cur_pos > 0) && (isspace(current_line_buffer[cur_pos - 1]))) {
        cur_pos -= 1;
        backSpace();
    }
    while ((cur_pos > 0) && (!isspace(current_line_buffer[cur_pos - 1]))) {
        cur_pos -= 1;
        backSpace();
    }
    refreshLine();
}
//=============================================================================
void
LineManager::moveForwardSingleWord(void)
{
    while ((cur_pos < max_pos) && !isspace(current_line_buffer[cur_pos])) {
        putCharacter(current_line_buffer[cur_pos], LineManager::STANDARD_INPUT);
        cur_pos++;
    }
    while ((cur_pos < max_pos) && isspace(current_line_buffer[cur_pos])) {
        putCharacter(current_line_buffer[cur_pos], LineManager::STANDARD_INPUT);
        cur_pos++;
    }
    refreshLine();
}
//=============================================================================
void
LineManager::killCurrentPositionToEndLine(void)
{
    for (size_t i = cur_pos; i < max_pos; i++) {
        if (i >= current_line_buffer.size()) {
            current_line_buffer.push_back(L'\0');
        }
        current_line_buffer[i] = L'\0';
    }
    for (size_t i = cur_pos; i < max_pos; i++) {
        putCharacter(VK_SPACE, LineManager::STANDARD_INPUT);
    }
    for (size_t i = cur_pos; i < max_pos; i++) {
        backSpace();
    }
    max_pos = cur_pos;
}
//=============================================================================
void
LineManager::deletePreviousChar(void)
{
    if (cur_pos > 0) {
        cur_pos -= 1;
        backSpace();
        for (size_t i = cur_pos; i < max_pos; i++) {
            if (i + 1 < current_line_buffer.size()) {
                current_line_buffer[i] = current_line_buffer[i + 1];
            }
        }
        max_pos -= 1;
        refreshLine();
    } else {
        doBeep();
    }
}
//=============================================================================
void
LineManager::deleteCurrentChar(void)
{
    if (max_pos == 0) {
        doBeep();
    } else {
        if (cur_pos < max_pos) {
            for (size_t i = cur_pos; i < max_pos; i++) {
                if (i + 1 < current_line_buffer.size()) {
                    current_line_buffer[i] = current_line_buffer[i + 1];
                }
            }
            max_pos -= 1;
            refreshLine();
        }
    }
}
//=============================================================================
void
LineManager::moveBackHistory(void)
{
    if (current_line_buffer.size() == 0) {
        current_line_buffer.push_back(L'\0');
    }
    std::wstring line = Nelson::History::getPreviousLine();
    clearCurrentLine();
    copyLine(line);
}
//=============================================================================
void
LineManager::moveForwardHistory(void)
{
    if (current_line_buffer.size() == 0) {
        current_line_buffer.push_back(L'\0');
    }
    std::wstring line = Nelson::History::getNextLine();
    clearCurrentLine();
    copyLine(line);
}
//=============================================================================
void
LineManager::redrawLine(void)
{
    std::wstring line = getCurrentLine();
    displayPrompt();
    for (size_t i = max_pos; i > cur_pos; i--) {
        backSpace();
    }
    if (line.size() > 0) {
        copyLine(line);
    }
}
//=============================================================================
void
LineManager::killLastWord(void)
{
    while ((cur_pos > 0) && (current_line_buffer[cur_pos - 1] == VK_SPACE)) {
        cur_pos -= 1;
        backSpace();
    }
    while ((cur_pos > 0) && (current_line_buffer[cur_pos - 1] != VK_SPACE)) {
        cur_pos -= 1;
        backSpace();
    }
    killCurrentPositionToEndLine();
}
//=============================================================================
void
LineManager::newLine(void)
{
    if (current_line_buffer.size() == 0) {
        current_line_buffer.push_back(L'\0');
    }
    current_line_buffer[0] = L'\0';
    cur_pos = 0;
    max_pos = 0;
}
//=============================================================================
void
LineManager::clearCurrentLine(bool withPrompt)
{
    current_line_buffer.clear();
    current_line_buffer.push_back(L'\0');
    moveBeginningLine();
    std::wstring blanks(max_pos, L' ');
    printCharacters(blanks, LineManager::STANDARD_INPUT);
    putCharacter(L'\r', LineManager::STANDARD_INPUT);
    if (withPrompt) {
        displayPrompt();
        newLine();
    }
}
//=============================================================================
std::wstring
LineManager::getCurrentPrompt()
{
    return currentPrompt;
}
//=============================================================================
void
LineManager::setCurrentPrompt(std::wstring prompt)
{
    currentPrompt = prompt;
}
//=============================================================================
void
LineManager::doBeep()
{
    MessageBeep(MB_OK);
}
//=============================================================================
int
LineManager::putCharacter(wchar_t wch, outputStyle eAsStyle)
{
    std::wstring wchs = L"";
    wchs.push_back(wch);
    return printCharacters(wchs, eAsStyle);
}
//=============================================================================
int
LineManager::printCharacters(std::wstring buffer, outputStyle eAsStyle)
{
    DWORD n = 0;
    int background = 0;
    int foreground = 0;
    if (bUsesColors) {
        background = BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY;
        foreground = 0;
        switch (eAsStyle) {
        case outputStyle::ERROR_OUTPUT: {
            foreground = FOREGROUND_RED | FOREGROUND_INTENSITY;
        } break;
        case outputStyle::STANDARD_INPUT: {
            foreground = FOREGROUND_BLUE | FOREGROUND_INTENSITY;
        } break;
        case outputStyle::WARNING_OUTPUT: {
            foreground = FOREGROUND_RED | FOREGROUND_GREEN;
        } break;
        case outputStyle::PROMPT_OUTPUT: {
            foreground = FOREGROUND_BLUE | FOREGROUND_INTENSITY;
        } break;
        case outputStyle::STANDARD_OUTPUT: {
            foreground = 0; // BLACK
        } break;
        default: { } break; }
    }
    if (eAsStyle == outputStyle::ERROR_OUTPUT) {
        if (bUsesColors) {
            ::SetConsoleTextAttribute(GetStdHandle(STD_ERROR_HANDLE), foreground | background);
        }
        DWORD dwValue = 0;
        SIZETToDWord(buffer.size(), &dwValue);
        if (::WriteConsoleW(GetStdHandle(STD_ERROR_HANDLE), buffer.c_str(), dwValue, &n, NULL)) {
            return n;
        }
    } else {
        if (bUsesColors) {
            ::SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), foreground | background);
        }
        DWORD dwValue = 0;
        SIZETToDWord(buffer.size(), &dwValue);
        if (::WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), buffer.c_str(), dwValue, &n, NULL)) {
            return n;
        }
    }
    return n;
}
//=============================================================================
void
LineManager::displayPrompt(void)
{
    /* check position */
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    ::GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    int X = csbi.dwCursorPosition.X;
    if (X) {
        printCharacters(L"\n");
    }
    printCharacters(getCurrentPrompt(), LineManager::PROMPT_OUTPUT);
}
//=============================================================================
void
LineManager::refreshLine(void)
{
    /* write tail of string */
    boost::container::vector<wchar_t>::iterator itbegin = current_line_buffer.begin() + cur_pos;
    boost::container::vector<wchar_t>::iterator itend = current_line_buffer.begin() + max_pos;
    std::wstring line(itbegin, itend);
    printCharacters(line, LineManager::STANDARD_INPUT);
    /* write a space at the end of the line in case we deleted one */
    putCharacter(VK_SPACE, LineManager::STANDARD_INPUT);
    /* backup to original position */
    for (size_t i = max_pos + 1; i > cur_pos; i--) {
        backSpace();
    }
}
//=============================================================================
void
LineManager::copyLine(std::wstring line)
{
    if (line.size() > 0) {
        printCharacters(line, LineManager::STANDARD_INPUT);
        current_line_buffer = boost::container::vector<wchar_t>(line.begin(), line.end());
        cur_pos = max_pos = (int)line.size();
    }
}
//=============================================================================
void
LineManager::addCharacterCurrentLine(wchar_t ch)
{
    for (size_t i = max_pos; i > cur_pos; i--) {
        if (i >= current_line_buffer.size()) {
            current_line_buffer.push_back(L'\0');
        }
        current_line_buffer[i] = current_line_buffer[i - 1];
    }
    if (cur_pos >= current_line_buffer.size()) {
        current_line_buffer.push_back(L'\0');
    }
    current_line_buffer[cur_pos] = ch;
    cur_pos += 1;
    max_pos += 1;
    if (max_pos >= current_line_buffer.size()) {
        current_line_buffer.push_back(L'\0');
    }
    current_line_buffer[max_pos] = L'\0';
    if (cur_pos < max_pos) {
        refreshLine();
    }
}
//=============================================================================
void
LineManager::backSpace(void)
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    ::GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    int X = csbi.dwCursorPosition.X;
    int Y = csbi.dwCursorPosition.Y;
    if ((X - 1) < 0) {
        X = csbi.srWindow.Right - csbi.srWindow.Left;
        Y = Y - 1;
        COORD pt;
        pt.X = (SHORT)X;
        pt.Y = (SHORT)Y;
        ::SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), pt);
    } else {
        putCharacter(VK_BACK, LineManager::STANDARD_INPUT);
    }
}
//=============================================================================
