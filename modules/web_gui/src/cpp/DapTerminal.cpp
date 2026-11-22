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
//=============================================================================
namespace Nelson {
//=============================================================================
DapTerminal::DapTerminal(DapServer* server) : m_server(server) { }
//=============================================================================
DapTerminal::~DapTerminal() = default;
//=============================================================================
std::wstring
DapTerminal::readLine()
{
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    return m_server->nextLine(eval);
}
//=============================================================================
void
DapTerminal::emitText(const std::string& stream, const std::wstring& msg)
{
    m_server->emitOutput(stream, msg);
}
//=============================================================================
bool
DapTerminal::interpreterIdle()
{
    return m_server->isInterpreterIdle();
}
//=============================================================================
void
DapTerminal::wakeInterpreter()
{
    m_server->wake();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
