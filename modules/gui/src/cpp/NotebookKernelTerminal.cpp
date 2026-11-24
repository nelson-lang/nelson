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
//=============================================================================
namespace Nelson {
//=============================================================================
NotebookKernelTerminal::NotebookKernelTerminal(NotebookKernel* kernel) : m_kernel(kernel) { }
//=============================================================================
NotebookKernelTerminal::~NotebookKernelTerminal() = default;
//=============================================================================
std::wstring
NotebookKernelTerminal::readLine()
{
    // The prompt is never shown: a frame channel has no echo. Returns the next
    // cell's code (or a quit request on shutdown).
    return m_kernel->nextCellCode();
}
//=============================================================================
void
NotebookKernelTerminal::emitText(const std::string& stream, const std::wstring& msg)
{
    m_kernel->emitOutput(stream, msg);
}
//=============================================================================
std::wstring
NotebookKernelTerminal::readInput(const std::wstring& prompt)
{
    // Interactive input(): ask the client for a line over the frame channel and
    // block until it answers (or the cell is interrupted).
    return m_kernel->requestInput(prompt);
}
//=============================================================================
void
NotebookKernelTerminal::onErrorLine(const std::wstring& msg)
{
    m_kernel->markCellError(msg);
}
//=============================================================================
bool
NotebookKernelTerminal::interpreterIdle()
{
    return m_kernel->isInterpreterIdle();
}
//=============================================================================
void
NotebookKernelTerminal::wakeInterpreter()
{
    m_kernel->wake();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
