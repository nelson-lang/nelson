//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NotebookKernelTerminal.hpp"
#include "NotebookKernel.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NotebookKernelTerminal::NotebookKernelTerminal(NotebookKernel* kernel) : m_kernel(kernel) { }
//=============================================================================
NotebookKernelTerminal::~NotebookKernelTerminal() = default;
//=============================================================================
std::wstring
NotebookKernelTerminal::getLine(const std::wstring& /*prompt*/)
{
    // The prompt is never shown: a frame channel has no echo. Each call returns
    // the next cell's code (or a quit request on shutdown).
    return m_kernel->nextCellCode();
}
//=============================================================================
std::string
NotebookKernelTerminal::getLine(const std::string& /*prompt*/)
{
    return wstring_to_utf8(m_kernel->nextCellCode());
}
//=============================================================================
std::wstring
NotebookKernelTerminal::getInput(const std::wstring& /*prompt*/)
{
    // Interactive keyboard input() is not available over the frame channel.
    return L"";
}
//=============================================================================
size_t
NotebookKernelTerminal::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
NotebookKernelTerminal::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
NotebookKernelTerminal::outputMessage(const std::string& msg)
{
    m_kernel->emitOutput("stdout", utf8_to_wstring(msg));
}
//=============================================================================
void
NotebookKernelTerminal::outputMessage(const std::wstring& msg)
{
    m_kernel->emitOutput("stdout", msg);
}
//=============================================================================
void
NotebookKernelTerminal::errorMessage(const std::string& msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
NotebookKernelTerminal::errorMessage(const std::wstring& msg)
{
    m_kernel->markCellError(msg);
    m_kernel->emitOutput("stderr", msg);
}
//=============================================================================
void
NotebookKernelTerminal::warningMessage(const std::string& msg)
{
    m_kernel->emitOutput("stderr", utf8_to_wstring(msg));
}
//=============================================================================
void
NotebookKernelTerminal::warningMessage(const std::wstring& msg)
{
    m_kernel->emitOutput("stderr", msg);
}
//=============================================================================
void
NotebookKernelTerminal::clearTerminal()
{
}
//=============================================================================
bool
NotebookKernelTerminal::isAtPrompt()
{
    return m_kernel->isInterpreterIdle();
}
//=============================================================================
void
NotebookKernelTerminal::interruptGetLineByEvent()
{
    m_kernel->wake();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
