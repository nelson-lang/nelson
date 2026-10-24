//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
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
#include "Diary.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_CONSOLE_MAX_LINE_VISIBLE 5500
#define DEFAULT_CONSOLE_WIDTH 80
#define DEFAULT_CONSOLE_HEIGHT 43
//=============================================================================
struct NLSSTREAM_MANAGER_IMPEXP TextStyle
{
    bool hasForeground = false;
    unsigned char red = 0;
    unsigned char green = 0;
    unsigned char blue = 0;
    bool hasBackground = false;
    unsigned char backgroundRed = 0;
    unsigned char backgroundGreen = 0;
    unsigned char backgroundBlue = 0;
    bool bold = false;
    bool italic = false;
    bool underline = false;
    bool strikethrough = false;

    bool
    isPlain() const
    {
        return !hasForeground && !hasBackground && !bold && !italic && !underline && !strikethrough;
    }
};
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP Interface
{
public:
    Diary diary;
    Interface();
    virtual ~Interface() = 0;
    virtual std::string
    getLine(const std::string& prompt)
        = 0;
    virtual std::wstring
    getLine(const std::wstring& prompt)
        = 0;
    virtual std::wstring
    getInput(const std::wstring& prompt)
        = 0;

    virtual size_t
    getTerminalWidth()
        = 0;

    virtual size_t
    getTerminalHeight()
        = 0;
    virtual void
    outputMessage(const std::string& msg)
        = 0;
    virtual void
    outputMessage(const std::wstring& msg)
        = 0;
    virtual bool
    outputExpandableText(const std::wstring& label, const std::wstring& expandedText,
        const std::wstring& plainFallback, const std::wstring& contextName = L"",
        long long contextValue = 0)
    {
        (void)label;
        (void)expandedText;
        (void)contextName;
        (void)contextValue;
        if (!plainFallback.empty()) {
            outputMessage(plainFallback);
        }
        return false;
    }
    virtual bool
    supportsTruncatedMatrixDisplay()
    {
        return false;
    }
    virtual bool
    outputTruncatedMatrix(const std::wstring& truncatedText, const std::wstring& label,
        const std::wstring& expandedText, const std::wstring& variableName)
    {
        (void)label;
        (void)expandedText;
        (void)variableName;
        outputMessage(truncatedText);
        return false;
    }
    virtual void
    errorMessage(const std::string& msg)
        = 0;
    virtual void
    errorMessage(const std::wstring& msg)
        = 0;
    virtual void
    warningMessage(const std::string& msg)
        = 0;
    virtual void
    warningMessage(const std::wstring& msg)
        = 0;

    virtual void
    clearTerminal()
        = 0;
    virtual bool
    isAtPrompt()
        = 0;
    virtual void
    interruptGetLineByEvent()
        = 0;
    virtual bool
    outputStyledText(const std::wstring& msg, const TextStyle& style)
    {
        (void)style;
        outputMessage(msg);
        return false;
    }
    virtual bool
    outputAnsiText(const std::wstring& msg, bool asError)
    {
        if (asError) {
            errorMessage(msg);
        } else {
            outputMessage(msg);
        }
        return false;
    }
    /**
     *  Returns false when the terminal cannot show markup, so the caller can
     *  report it instead of printing raw tags.
     */
    virtual bool
    insertHtml(const std::wstring& html)
    {
        (void)html;
        return false;
    }
    /**
     *  True when OSC 8 hyperlinks can be rendered (GUI and webview command
     *  windows). Declared last to keep the vtable layout stable.
     */
    virtual bool
    supportsHyperlinks()
    {
        return false;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
