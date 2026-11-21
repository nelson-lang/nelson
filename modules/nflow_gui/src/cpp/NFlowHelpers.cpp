//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "NFlowHelpers.hpp"
#include "ModulePath.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include <filesystem>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
getNFlowDistributionDirectory()
{
    // Desktop: nflow_gui is loaded, so ModulePath resolves its root. The stdio
    // kernel runs headless and does not load the module, so ModulePath raises;
    // fall back to the modules directory, which NelsonConfiguration resolves from
    // the Nelson root on every OS. The loaded-module path is returned unchanged.
    try {
        return ModulePath(L"nflow_gui", GET_ROOT_PATH) + L"/dist";
    } catch (...) {
    }
    const std::wstring modulesDir
        = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    if (!modulesDir.empty()) {
        const std::wstring candidate = modulesDir + L"/nflow_gui/dist";
        // Only hand back a path that exists on disk; otherwise empty, so the caller
        // keeps its own fallback rather than a wrong absolute path.
        std::error_code ec;
        if (std::filesystem::is_directory(std::filesystem::path(candidate), ec)) {
            return candidate;
        }
    }
    return std::wstring();
}
//=============================================================================
std::wstring
getNFlowPreferencesDirectory()
{

    return NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
