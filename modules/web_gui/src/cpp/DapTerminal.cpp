//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DapTerminal.hpp"
#include "DapServer.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
DapTerminal::DapTerminal(DapServer* server) : m_server(server) { }
//=============================================================================
DapTerminal::~DapTerminal() = default;
//=============================================================================
std::wstring
DapTerminal::getLine(const std::wstring& /*prompt*/)
{
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    return m_server->nextLine(eval);
}
//=============================================================================
std::string
DapTerminal::getLine(const std::string& prompt)
{
    return wstring_to_utf8(getLine(utf8_to_wstring(prompt)));
}
//=============================================================================
std::wstring
DapTerminal::getInput(const std::wstring& /*prompt*/)
{
    // Interactive keyboard input() is not available over the frame channel.
    return L"";
}
//=============================================================================
size_t
DapTerminal::getTerminalWidth()
{
    return DEFAULT_CONSOLE_WIDTH;
}
//=============================================================================
size_t
DapTerminal::getTerminalHeight()
{
    return DEFAULT_CONSOLE_HEIGHT;
}
//=============================================================================
void
DapTerminal::outputMessage(const std::string& msg)
{
    m_server->emitOutput("stdout", utf8_to_wstring(msg));
}
//=============================================================================
void
DapTerminal::outputMessage(const std::wstring& msg)
{
    m_server->emitOutput("stdout", msg);
}
//=============================================================================
void
DapTerminal::errorMessage(const std::string& msg)
{
    m_server->emitOutput("stderr", utf8_to_wstring(msg));
}
//=============================================================================
void
DapTerminal::errorMessage(const std::wstring& msg)
{
    m_server->emitOutput("stderr", msg);
}
//=============================================================================
void
DapTerminal::warningMessage(const std::string& msg)
{
    m_server->emitOutput("stderr", utf8_to_wstring(msg));
}
//=============================================================================
void
DapTerminal::warningMessage(const std::wstring& msg)
{
    m_server->emitOutput("stderr", msg);
}
//=============================================================================
void
DapTerminal::clearTerminal()
{
}
//=============================================================================
bool
DapTerminal::isAtPrompt()
{
    return m_server->isInterpreterIdle();
}
//=============================================================================
void
DapTerminal::interruptGetLineByEvent()
{
    m_server->wake();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
