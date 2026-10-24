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
#include <filesystem>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class RecentDirectoriesStore
{
public:
    static constexpr size_t MAX_ENTRIES = 10;

    explicit RecentDirectoriesStore(const std::filesystem::path& storageFile);

    std::vector<std::string>
    directories();

    bool
    add(const std::string& directory);

private:
    std::filesystem::path m_storageFile;

    std::vector<std::filesystem::path>
    load(bool& changed) const;

    bool
    save(const std::vector<std::filesystem::path>& directories) const;

    static std::filesystem::path
    normalizeExistingDirectory(const std::filesystem::path& directory);

    static bool
    equivalent(const std::filesystem::path& left, const std::filesystem::path& right);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
