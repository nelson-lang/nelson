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
#include <optional>
#include <string>
#include "ArrayOf.hpp"
#include "AiMcpServerPrivate.hpp"
#include "Evaluator.hpp"
#include "nlsAi_exports.h"
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
// Dispatch a tool by name on the live interpreter and return its result.
// Arguments are a Nelson struct; returns nullopt when the tool name is unknown.
// This is the in-process entry point shared by the stdio MCP server and any
// other caller that needs the Nelson tools without the JSON-RPC transport.
NLSAI_IMPEXP std::optional<ToolResult>
invokeTool(
    Evaluator* eval, const std::wstring& name, const ArrayOf& args, const McpOptions& options);
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
