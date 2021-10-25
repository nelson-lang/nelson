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
    ~EvaluateInterface() override;
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::string
    getLine(const std::string& prompt) override;
    std::wstring
    getInput(const std::wstring& prompt) override;
    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight() override;
    void
    outputMessage(const std::wstring& msg) override;
    void
    outputMessage(const std::string& msg) override;
    void
    errorMessage(const std::wstring& msg) override;
    void
    errorMessage(const std::string& msg) override;
    void
    warningMessage(const std::wstring& msg) override;
    void
    warningMessage(const std::string& msg) override;
    void
    clearTerminal() override;
    bool
    isAtPrompt() override;
    std::wstring
    getOutputBuffer();
    void
    interruptGetLineByEvent();

private:
    std::wstring outputBuffer;
};
//=============================================================================
