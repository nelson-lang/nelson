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
//=============================================================================
// Package manager window preferences (filter, sort, selection, window size),
// kept in <prefdir>/nmm_gui.json. Read and written from the WebUI thread, so
// saving never waits for a busy interpreter.
namespace Nelson::NmmGuiGateway {
//=============================================================================
nlohmann::json
readNmmGuiPrefs();
//=============================================================================
nlohmann::json
mergeNmmGuiPrefs(const nlohmann::json& changes);
//=============================================================================
// Window size to open with: the stored one when sensible, else the default.
void
nmmGuiWindowSize(int& width, int& height);
//=============================================================================
} // namespace Nelson::NmmGuiGateway
//=============================================================================
