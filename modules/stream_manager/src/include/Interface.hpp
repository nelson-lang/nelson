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
class NLSSTREAM_MANAGER_IMPEXP Interface
{
public:
    Diary diary;
    Interface();
    virtual ~Interface() = 0;
    /**
     *  Get a line of input from the user with the
     *  given prompt.
     */
    virtual std::string
    getLine(const std::string& prompt)
        = 0;
    virtual std::wstring
    getLine(const std::wstring& prompt)
        = 0;
    virtual std::wstring
    getInput(const std::wstring& prompt)
        = 0;

    /**
     *  Return the width, height of the current "terminal" in
     *  characters.
     */
    virtual size_t
    getTerminalWidth()
        = 0;

    virtual size_t
    getTerminalHeight()
        = 0;
    /**
     *  Output the following text message.
     */
    virtual void
    outputMessage(const std::string& msg)
        = 0;
    virtual void
    outputMessage(const std::wstring& msg)
        = 0;
    /**
     *  Output the following error message.
     */
    virtual void
    errorMessage(const std::string& msg)
        = 0;
    virtual void
    errorMessage(const std::wstring& msg)
        = 0;
    /**
     *  Output the following warning message.
     */
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
};
//=============================================================================
} // namespace Nelson
//=============================================================================
