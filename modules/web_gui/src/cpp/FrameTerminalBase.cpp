//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FrameTerminalBase.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
FrameTerminalBase::getLine(const std::string& /*prompt*/)
{
    return wstring_to_utf8(readLine());
}
//=============================================================================
std::wstring
FrameTerminalBase::getLine(const std::wstring& /*prompt*/)
{
    return readLine();
}
//=============================================================================
std::wstring
FrameTerminalBase::getInput(const std::wstring& prompt)
{
    return readInput(prompt);
}
//=============================================================================
std::wstring
FrameTerminalBase::readInput(const std::wstring& /*prompt*/)
{
    return L"";
}
//=============================================================================
void
FrameTerminalBase::onErrorLine(const std::wstring& /*msg*/)
{
}
//=============================================================================
size_t
FrameTerminalBase::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
FrameTerminalBase::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
FrameTerminalBase::outputMessage(const std::string& msg)
{
    emitText("stdout", utf8_to_wstring(msg));
}
//=============================================================================
void
FrameTerminalBase::outputMessage(const std::wstring& msg)
{
    emitText("stdout", msg);
}
//=============================================================================
void
FrameTerminalBase::errorMessage(const std::string& msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
FrameTerminalBase::errorMessage(const std::wstring& msg)
{
    onErrorLine(msg);
    emitText("stderr", msg);
}
//=============================================================================
void
FrameTerminalBase::warningMessage(const std::string& msg)
{
    emitText("stderr", utf8_to_wstring(msg));
}
//=============================================================================
void
FrameTerminalBase::warningMessage(const std::wstring& msg)
{
    emitText("stderr", msg);
}
//=============================================================================
void
FrameTerminalBase::clearTerminal()
{
}
//=============================================================================
bool
FrameTerminalBase::isAtPrompt()
{
    return interpreterIdle();
}
//=============================================================================
void
FrameTerminalBase::interruptGetLineByEvent()
{
    wakeInterpreter();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
