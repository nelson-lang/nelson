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
#include "FmuComponent.hpp"
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson::NFlow {
bool
invokeWasmFmu(const nlohmann::json& request, nlohmann::json& response, SimDiagnostic& diag);
std::unique_ptr<IFmuComponent>
makeWasmFmuComponent(const std::string& path, SimDiagnostic& diag);
std::unique_ptr<IFmuMeComponent>
makeWasmFmuMeComponent(const std::string& path, SimDiagnostic& diag);
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
