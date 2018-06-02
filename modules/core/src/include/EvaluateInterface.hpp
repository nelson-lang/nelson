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
#include "nlsCore_exports.h"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSCORE_IMPEXP EvaluateInterface : public Interface
{
#define WIDTH 80
public:
    EvaluateInterface();
    ~EvaluateInterface();
    std::wstring
    getLine(std::wstring prompt);
    std::string
    getLine(std::string prompt);
    std::wstring
    getInput(std::wstring prompt);
    size_t
    getTerminalWidth();
    void
    outputMessage(std::wstring msg);
    void
    outputMessage(std::string msg);
    void
    errorMessage(std::wstring msg);
    void
    errorMessage(std::string msg);
    void
    warningMessage(std::wstring msg);
    void
    warningMessage(std::string msg);
    void
    clearTerminal();
    bool
    isAtPrompt();
    std::wstring
    getOutputBuffer();

private:
    std::wstring outputBuffer;
};
//=============================================================================
