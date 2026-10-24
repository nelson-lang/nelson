//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include <filesystem>
#include <set>
#include <string>
#include <vector>
#include "nlohmann/json_fwd.hpp"
//=============================================================================
namespace Nelson::Application {
struct InputSelection
{
    std::vector<std::filesystem::path> files;
    std::vector<std::filesystem::path> directories;
    std::vector<std::filesystem::path> roots;
    std::vector<std::filesystem::path> lookupPaths;
};
//=============================================================================
struct PackageSelection
{
    std::vector<std::filesystem::path> excludedRoots;
    std::set<std::string> excludedModules;
    std::vector<std::filesystem::path> lookupPaths;
};
//=============================================================================
PackageSelection
readPackageSelection(const std::wstring& encoded);
std::string
fileKey(const std::filesystem::path& path);
bool
within(const std::filesystem::path& path, const std::filesystem::path& root);
std::filesystem::path
functionSearchRoot(std::filesystem::path directory);
InputSelection
expandInputs(const std::vector<std::wstring>& inputs);
std::filesystem::path
inputRoot(const std::filesystem::path& path, const std::filesystem::path& applicationRoot,
    const InputSelection& inputs);
void
finishInputPaths(nlohmann::json& document, const std::filesystem::path& applicationRoot,
    const InputSelection& inputs);
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
