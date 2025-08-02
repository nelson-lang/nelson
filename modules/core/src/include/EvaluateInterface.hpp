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
#include "nlsCore_exports.h"
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
    interruptGetLineByEvent() override;

private:
    std::wstring outputBuffer;
};
//=============================================================================
