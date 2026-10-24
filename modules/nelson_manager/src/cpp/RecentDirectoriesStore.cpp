//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RecentDirectoriesStore.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
RecentDirectoriesStore::RecentDirectoriesStore(const std::filesystem::path& storageFile)
    : m_storageFile(storageFile)
{
}
//=============================================================================
static std::string
pathToUtf8(const std::filesystem::path& path)
{
    return path.generic_u8string();
}
//=============================================================================
std::filesystem::path
RecentDirectoriesStore::normalizeExistingDirectory(const std::filesystem::path& directory)
{
    if (directory.empty()) {
        return {};
    }
    std::error_code ec;
    std::filesystem::path absolute = directory;
    if (!absolute.is_absolute()) {
        absolute = std::filesystem::absolute(absolute, ec);
    }
    if (ec || !std::filesystem::is_directory(absolute, ec) || ec) {
        return {};
    }
    std::filesystem::path normalized = std::filesystem::weakly_canonical(absolute, ec);
    return ec ? absolute.lexically_normal() : normalized;
}
//=============================================================================
bool
RecentDirectoriesStore::equivalent(
    const std::filesystem::path& left, const std::filesystem::path& right)
{
    std::error_code ec;
    const bool same = std::filesystem::equivalent(left, right, ec);
    return !ec && same;
}
//=============================================================================
std::vector<std::filesystem::path>
RecentDirectoriesStore::load(bool& changed) const
{
    changed = false;
    std::vector<std::filesystem::path> result;
    std::ifstream file(m_storageFile);
    if (!file.is_open()) {
        return result;
    }

    nlohmann::json data = nlohmann::json::parse(file, nullptr, false);
    if (data.is_discarded() || !data.is_object() || !data.contains("directories")
        || !data["directories"].is_array()) {
        return result;
    }

    const auto& stored = data["directories"];
    for (const auto& value : stored) {
        if (!value.is_string()) {
            changed = true;
            continue;
        }
        std::filesystem::path normalized
            = normalizeExistingDirectory(std::filesystem::u8path(value.get<std::string>()));
        if (normalized.empty()) {
            changed = true;
            continue;
        }
        const bool duplicate
            = std::any_of(result.begin(), result.end(), [&](const std::filesystem::path& existing) {
                  return equivalent(normalized, existing);
              });
        if (duplicate || result.size() == MAX_ENTRIES) {
            changed = true;
            continue;
        }
        result.emplace_back(std::move(normalized));
    }
    return result;
}
//=============================================================================
bool
RecentDirectoriesStore::save(const std::vector<std::filesystem::path>& directories) const
{
    std::error_code ec;
    if (!m_storageFile.parent_path().empty()) {
        std::filesystem::create_directories(m_storageFile.parent_path(), ec);
        if (ec) {
            return false;
        }
    }
    nlohmann::json data;
    data["directories"] = nlohmann::json::array();
    for (const auto& directory : directories) {
        data["directories"].push_back(pathToUtf8(directory));
    }
    std::ofstream file(m_storageFile, std::ios::trunc);
    if (!file.is_open()) {
        return false;
    }
    file << data.dump(2) << '\n';
    return file.good();
}
//=============================================================================
std::vector<std::string>
RecentDirectoriesStore::directories()
{
    bool changed = false;
    std::vector<std::filesystem::path> stored = load(changed);
    if (changed) {
        save(stored);
    }
    std::vector<std::string> result;
    result.reserve(stored.size());
    for (const auto& directory : stored) {
        result.emplace_back(pathToUtf8(directory));
    }
    return result;
}
//=============================================================================
bool
RecentDirectoriesStore::add(const std::string& directory)
{
    std::filesystem::path normalized
        = normalizeExistingDirectory(std::filesystem::u8path(directory));
    if (normalized.empty()) {
        return false;
    }
    bool changed = false;
    std::vector<std::filesystem::path> stored = load(changed);
    stored.erase(std::remove_if(stored.begin(), stored.end(),
                     [&](const std::filesystem::path& existing) {
                         return equivalent(normalized, existing);
                     }),
        stored.end());
    stored.insert(stored.begin(), std::move(normalized));
    if (stored.size() > MAX_ENTRIES) {
        stored.resize(MAX_ENTRIES);
    }
    return save(stored);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
