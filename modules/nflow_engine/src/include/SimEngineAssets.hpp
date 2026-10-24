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
#include <filesystem>
#include <map>
#include <string>
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
namespace NFlow {
    //=============================================================================
    // The packaged examples reference their assets as nflow-example:/<namespace>/...:
    // "fmi" is the nflow_fmi example tree, the other namespaces are nflow_blocks categories.
    inline std::map<std::string, std::string>
    packagedExampleRoots()
    {
        const std::filesystem::path modules(
            NelsonConfiguration::getInstance()->getNelsonModulesDirectory());
        std::map<std::string, std::string> roots;
        roots["fmi"] = (modules / "nflow_fmi" / "examples").u8string();
        roots[""] = (modules / "nflow_blocks" / "examples").u8string();
        return roots;
    }
    //=============================================================================
} // namespace NFlow
} // namespace Nelson
//=============================================================================
