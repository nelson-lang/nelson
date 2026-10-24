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
    void
    outputMessage(const std::wstring& msg) override;
    void
    outputMessage(const std::string& msg) override;
    bool
    outputStyledText(const std::wstring& msg, const TextStyle& style) override;
    bool
    outputAnsiText(const std::wstring& msg, bool asError) override;
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

    void
    interruptGetLineByEvent() override;

private:
    std::wstring
    getTextLine(const std::wstring& prompt, bool bIsInput);
    std::string
    getTextLine(const std::string& prompt, bool bIsInput);
    void
    markPromptInterrupted();
    void
    redrawPromptIfNeeded();
    void
    clearPromptState();
    bool atPrompt;
    bool promptRedrawPending;
    bool promptRedrawNeedsNewline;
    std::string currentPrompt;
};
//=============================================================================
