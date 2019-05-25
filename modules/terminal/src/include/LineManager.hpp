//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include <boost/container/vector.hpp>
#include <string>
//=============================================================================
class LineManager
{
private:
    std::wstring currentPrompt;
    size_t cur_pos; /* current position of the cursor */
    size_t max_pos;

    boost::container::vector<wchar_t> current_line_buffer;

    bool bUsesColors;

    void
    backSpace();
    void
    doBeep();

public:
    enum outputStyle
    {
        ERROR_OUTPUT = 0,
        WARNING_OUTPUT,
        STANDARD_OUTPUT,
        STANDARD_INPUT,
        PROMPT_OUTPUT
    };
    int
    putCharacter(wchar_t wch, outputStyle eAsStyle = outputStyle::STANDARD_OUTPUT);
    int
    printCharacters(
        const std::wstring& buffer, outputStyle eAsStyle = outputStyle::STANDARD_OUTPUT);

    LineManager();
    ~LineManager();
    void
    usesColors();
    /**
     * returns current line
     * @return current line
     */
    std::wstring
    getCurrentLine();

    /**
     * returns line before caret
     * return line
     */
    std::wstring
    getLineBeforeCaret();

    /**
     * returns line after caret
     * return line
     */
    std::wstring
    getLineAfterCaret();

    /**
     * moves to the beginning of the line
     */
    void
    moveBeginningLine();

    /**
     * moves to the end of the line
     */
    void
    moveEndLine();

    /**
     * moves back a single character
     */
    void
    moveBackSingleChar();

    /**
     * moves forward a single character
     */
    void
    moveForwardSingleChar();

    /**
     * moves back a single word
     */
    void
    moveBackSingleWord();

    /**
     * moves forward a single word
     */
    void
    moveForwardSingleWord();

    /**
     * kills from current position to the end of line
     */
    void
    killCurrentPositionToEndLine();

    /**
     * delete the previous character
     */
    void
    deletePreviousChar();

    /**
     * deletes the current character
     */
    void
    deleteCurrentChar();

    /**
     * moves back through history
     */
    void
    moveBackHistory();

    /**
     * moves forward through history
     */
    void
    moveForwardHistory();

    /**
     * redraw line
     */
    void
    redrawLine();

    /**
     * kills last word
     */
    void
    killLastWord();

    /**
     * initialize new line
     */
    void
    newLine();

    /**
     * clear current line
     */
    void
    clearCurrentLine(bool withPrompt = true);

    /**
     * set prompt used by terminal
     */
    void
    setCurrentPrompt(const std::wstring& prompt);

    /**
     * get prompt used by terminal
     */
    std::wstring
    getCurrentPrompt();

    /**
     * display prompt
     */
    void
    displayPrompt();

    /**
     * refresh line
     */
    void
    refreshLine();

    /**
     * copy line on terminal
     */
    void
    copyLine(const std::wstring& line);

    /**
     * add character to current line
     * @param[in] character to add
     */
    void
    addCharacterCurrentLine(wchar_t ch);
};
