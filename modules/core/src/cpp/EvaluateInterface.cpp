//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "EvaluateInterface.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "AnsiSgr.hpp"
//=============================================================================
EvaluateInterface::EvaluateInterface() { outputBuffer.clear(); }
//=============================================================================
EvaluateInterface::~EvaluateInterface() { outputBuffer.clear(); }
//=============================================================================
std::wstring
EvaluateInterface::getLine(const std::wstring& prompt)
{
    return L"";
}
//=============================================================================
std::string
EvaluateInterface::getLine(const std::string& prompt)
{
    return "";
}
//=============================================================================
std::wstring
EvaluateInterface::getInput(const std::wstring& prompt)
{
    Error(L"Nelson:evalc:inputNotAllowed", L"input function not allowed from evalc.");
    return L"";
}
//=============================================================================
size_t
EvaluateInterface::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
EvaluateInterface::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
EvaluateInterface::outputMessage(const std::wstring& msg)
{
    outputBuffer.append(msg);
}
//=============================================================================
void
EvaluateInterface::outputMessage(const std::string& msg)
{
    outputMessage(utf8_to_wstring(msg));
}
//=============================================================================
bool
EvaluateInterface::outputAnsiText(const std::wstring& msg, bool asError)
{
    (void)asError;
    outputMessage(stripAnsiSgr(msg));
    return containsAnsiEscape(msg);
}
//=============================================================================
void
EvaluateInterface::errorMessage(const std::wstring& msg)
{
    outputMessage(msg);
}
//=============================================================================
void
EvaluateInterface::errorMessage(const std::string& msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
EvaluateInterface::warningMessage(const std::wstring& msg)
{
    outputMessage(msg);
}
//=============================================================================
void
EvaluateInterface::warningMessage(const std::string& msg)
{
    warningMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
EvaluateInterface::clearTerminal()
{
    outputBuffer.clear();
}
//=============================================================================
bool
EvaluateInterface::isAtPrompt()
{
    return false;
}
//=============================================================================
std::wstring
EvaluateInterface::getOutputBuffer()
{
    return outputBuffer;
}
//=============================================================================
void
EvaluateInterface::interruptGetLineByEvent()
{
}
//=============================================================================
