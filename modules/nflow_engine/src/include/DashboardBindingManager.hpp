//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "SimEngineTypes.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
struct DashboardBindingDiagnostic
{
    std::string blockId;
    std::string message;
};
//=============================================================================
std::vector<DashboardBindingDiagnostic>
validateDashboardBindings(
    std::vector<Block>& blocks, std::vector<BlockState>& states, const ValMap& variables);
//=============================================================================
bool
applyDashboardBindingValue(std::vector<Block>& blocks, std::vector<BlockState>& states,
    ValMap& variables, const json& event, DashboardBindingDiagnostic& diagnostic);
//=============================================================================
void
flattenDashboardVariable(
    const std::string& name, const json& value, ValMap& variables, std::vector<size_t> path = {});
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
