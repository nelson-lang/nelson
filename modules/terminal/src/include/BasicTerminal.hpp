//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
//=============================================================================
using namespace Nelson;
//=============================================================================
class NLSTERMINAL_IMPEXP BasicTerminal : public Interface
{
public:
    BasicTerminal();
    ~BasicTerminal() override;
    /**
     *  Get a line of input from the user with the
     *  given prompt.
     */
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::string
    getLine(const std::string& prompt) override;
    std::wstring
    getInput(const std::wstring& prompt) override;
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
    outputMessage(const std::wstring& msg) override;
    void
    outputMessage(const std::string& msg) override;
    /**
     *  Output the following error message.
     */
    void
    errorMessage(const std::wstring& msg) override;
    void
    errorMessage(const std::string& msg) override;
    /**
     *  Output the following warning message.
     */
    void
    warningMessage(const std::wstring& msg) override;
    void
    warningMessage(const std::string& msg) override;

    void
    clearTerminal() override;
    bool
    isAtPrompt() override;

    void
    interruptGetLineByEvent() override;

private:
    std::wstring
    getTextLine(const std::wstring& prompt, bool bIsInput);
    std::string
    getTextLine(const std::string& prompt, bool bIsInput);
    bool atPrompt;
};
//=============================================================================
