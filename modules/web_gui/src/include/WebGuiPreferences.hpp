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
#include <cstddef>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
nlohmann::json
loadWebGuiPreferences();
void
saveWebGuiPreferences(const nlohmann::json& values);
void
applyWebGuiWindowGeometry(size_t window);
void
saveWebGuiWindowGeometry(size_t window);
//=============================================================================
} // namespace Nelson
//=============================================================================
