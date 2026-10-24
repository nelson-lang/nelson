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
#include <nlohmann/json.hpp>
#include <string>
//=============================================================================
namespace Nelson::NFlowUiBindings {
//=============================================================================
std::string
generateCodeToDirRpc(const nlohmann::json& params);
std::string
generateBlockToDirRpc(const nlohmann::json& params);
// Replace every generated user.<id> library block with its packaged inner
// subsystem spec (recursing into nested specs). Shared by the directory
// generators and the code-view preview so all entry points see the same
// diagram. Returns true when at least one block was inlined.
bool
inlineUserBlocksForCodegen(nlohmann::json& diagram);
//=============================================================================
} // namespace Nelson::NFlowUiBindings
//=============================================================================
