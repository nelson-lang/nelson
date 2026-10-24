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
#include "nlsGui_exports.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtTerminal;
class QtMainWindow;
//=============================================================================
class NLSGUI_IMPEXP GuiTerminal : public Interface
{
public:
    GuiTerminal(void* qtMainW);
    ~GuiTerminal() override;

    std::string
    getLine(const std::string& prompt) override;
    std::wstring
    getLine(const std::wstring& prompt) override;
    std::wstring
    getInput(const std::wstring& prompt) override;

    size_t
    getTerminalWidth() override;
    size_t
    getTerminalHeight() override;
    void
    outputMessage(const std::string& msg) override;
    void
    outputMessage(const std::wstring& msg) override;
    bool
    outputStyledText(const std::wstring& msg, const TextStyle& style) override;
    bool
    outputAnsiText(const std::wstring& msg, bool asError) override;
    bool
    outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& plainFallback, const std::wstring& contextName = L"",
        long long contextValue = 0) override;
    bool
    supportsTruncatedMatrixDisplay() override;
    bool
    outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
        const std::wstring& expandedText, const std::wstring& variableName) override;
    // The Qt command window renders OSC 8 hyperlinks as clickable anchors that
    // open the editor at the error/warning file and line.
    bool
    supportsHyperlinks() override;
    void
    errorMessage(const std::string& msg) override;
    void
    errorMessage(const std::wstring& msg) override;
    void
    warningMessage(const std::string& msg) override;
    void
    warningMessage(const std::wstring& msg) override;

    void
    clearTerminal() override;

    void*
    getQtPointer();
    void
    banner();
    bool
    insertHtml(const std::wstring& msg) override;
    int
    getBufferScreenLine();
    void
    setBufferScreenLine(int newMax);
    bool
    isAtPrompt() override;
    void
    interruptGetLineByEvent() override;

private:
    std::wstring
    getTextLine(const std::wstring& prompt, bool bIsInput = false);

    QtTerminal* qtterm;
    QtMainWindow* qtMainWindow;
};
//=============================================================================
