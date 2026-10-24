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
#include <set>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::Installer {
struct Options
{
    std::filesystem::path directory;
    std::filesystem::path runtimeDirectory;
    std::filesystem::path log;
    std::set<std::wstring> seen;
    bool agreed = false;
    bool desktop = false;
    bool startMenu = false;
    std::wstring native;
};
//=============================================================================
void
parseOptions(const std::vector<std::wstring>& arguments, Options& options);
void
validateOptions(const Options& options, bool runtime);
std::wstring
nativeArguments(const Options& options);
std::string
utf8(const std::wstring& value);
std::wstring
wide(const std::string& value);
//=============================================================================
} // namespace Nelson::Installer
//=============================================================================
