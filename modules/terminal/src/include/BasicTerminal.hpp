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
#include "nlsTerminal_exports.h"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSTERMINAL_IMPEXP BasicTerminal : public Interface
{
#define CMD_BUFFER_SIZE 4096 * 2
#define WIDTH 80
public:
    BasicTerminal();
    ~BasicTerminal() override;
    /**
     *  Get a line of input from the user with the
     *  given prompt.
     */
    std::wstring
    getLine(std::wstring prompt) override;
    std::string
    getLine(std::string prompt) override;
    std::wstring
    getInput(std::wstring prompt) override;
    /**
     *  Return the width of the current "terminal" in
     *  characters.
     */
    size_t
    getTerminalWidth() override;
    /**
     *  Output the following text message.
     */
    void
    outputMessage(std::wstring msg) override;
    void
    outputMessage(std::string msg) override;
    /**
     *  Output the following error message.
     */
    void
    errorMessage(std::wstring msg) override;
    void
    errorMessage(std::string msg) override;
    /**
     *  Output the following warning message.
     */
    void
    warningMessage(std::wstring msg) override;
    void
    warningMessage(std::string msg) override;

    void
    clearTerminal() override;
    bool
    isAtPrompt() override;

private:
    std::wstring
    getTextLine(std::wstring prompt, bool bIsInput);
    std::string
    getTextLine(std::string prompt, bool bIsInput);
    bool atPrompt;
};
//=============================================================================
