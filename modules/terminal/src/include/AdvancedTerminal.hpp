//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "Interface.hpp"
#include "nlsTerminal_exports.h"
#include "linse.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSTERMINAL_IMPEXP AdvancedTerminal : public Interface
{
private:
    linse ls;

public:
    AdvancedTerminal();
    ~AdvancedTerminal();
    /**
     *  Get a line of input from the user with the
     *  given prompt.
     */
    std::wstring
    getLine(const std::wstring& prompt);
    std::string
    getLine(const std::string& prompt);
    std::wstring
    getInput(const std::wstring& prompt);
    /**
     *  Return the width of the current "terminal" in
     *  characters.
     */
    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight() override;
    /**
     *  Output the following text message.
     */
    void
    outputMessage(const std::wstring& msg);
    void
    outputMessage(const std::string& msg);
    /**
     *  Output the following error message.
     */
    void
    errorMessage(const std::wstring& msg);
    void
    errorMessage(const std::string& msg);
    /**
     *  Output the following warning message.
     */
    void
    warningMessage(const std::wstring& msg);
    void
    warningMessage(const std::string& msg);

    void
    clearTerminal();
    bool
    isAtPrompt();

    void
    interruptGetLineByEvent() override;

private:
    std::wstring
    getTextLine(const std::wstring& prompt, bool bIsInput);
    bool atPrompt;
};
//=============================================================================
