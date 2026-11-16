//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "Evaluator.hpp"
#include "ai_interruptpendingBuiltin.hpp"
#include "ai_notifyBuiltin.hpp"
#include "ai_streamBuiltin.hpp"
#include "mcpcallBuiltin.hpp"
#include "mcpinfoBuiltin.hpp"
#include "mcpserverBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"ai";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__mcp_server__", (ptrBuiltin)Nelson::AiGateway::mcpserverBuiltin, 0, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__mcp_info__", (ptrBuiltin)Nelson::AiGateway::mcpinfoBuiltin, 1, 0 },
    { "__mcp_call__", (ptrBuiltin)Nelson::AiGateway::mcpcallBuiltin, 1, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__ai_notify__", (ptrBuiltin)Nelson::AiGateway::ai_notifyBuiltin, 0, -1 },
    { "__ai_stream__", (ptrBuiltin)Nelson::AiGateway::ai_streamBuiltin, 1, 7,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__ai_interrupt_pending__", (ptrBuiltin)Nelson::AiGateway::ai_interruptpendingBuiltin, 1,
        -1 },
};
//=============================================================================
// On module teardown (Nelson exit): stop every surface's poll timer and kill its
// claude-agent-acp agent, so a request left in "Thinking..." and its subprocess
// are not orphaned. The cleanup lives in nelson.ai.assistantShutdown (it holds
// the per-surface clients in a persistent).
static bool
finishAiModule(Nelson::Evaluator* eval)
{
    if (eval != nullptr) {
        try {
            // Wrapped in a Nelson try/catch too: at module teardown the nelson.ai
            // package path may already be gone, and the bare call would print an
            // "Undefined variable: nelson" error. The ProcessObject destructor
            // still terminates any surviving ACP child as a backstop.
            eval->evaluateString(L"try; nelson.ai.assistantShutdown(); catch; end", false);
        } catch (...) {
        }
    }
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishAiModule)
//=============================================================================
