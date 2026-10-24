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
#include <map>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class DialogDirectoriesStore
{
public:
    explicit DialogDirectoriesStore(const std::filesystem::path& storageFile);

    std::string
    get(const std::string& key);

    bool
    set(const std::string& key, const std::string& directory);

    std::string
    resolve(
        const std::string& key, const std::string& explicitPath, const std::string& fallbackPath);

private:
    std::filesystem::path m_storageFile;

    static std::filesystem::path
    normalizeExistingDirectory(const std::filesystem::path& path, bool acceptFile);

    std::map<std::string, std::filesystem::path>
    load(bool& changed) const;

    bool
    save(const std::map<std::string, std::filesystem::path>& directories) const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
