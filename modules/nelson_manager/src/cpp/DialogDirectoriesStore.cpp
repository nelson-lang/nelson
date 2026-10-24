//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DialogDirectoriesStore.hpp"
#include "nlohmann/json.hpp"
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
DialogDirectoriesStore::DialogDirectoriesStore(const std::filesystem::path& storageFile)
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
DialogDirectoriesStore::normalizeExistingDirectory(
    const std::filesystem::path& path, bool acceptFile)
{
    if (path.empty() || path == std::filesystem::path(".")) {
        return {};
    }
    std::error_code ec;
    std::filesystem::path candidate = path;
    if (!candidate.is_absolute()) {
        candidate = std::filesystem::absolute(candidate, ec);
    }
    if (ec || !std::filesystem::exists(candidate, ec) || ec) {
        return {};
    }
    if (!std::filesystem::is_directory(candidate, ec)) {
        if (!acceptFile || ec || !std::filesystem::is_regular_file(candidate, ec) || ec) {
            return {};
        }
        candidate = candidate.parent_path();
    }
    if (candidate.empty() || !std::filesystem::is_directory(candidate, ec) || ec) {
        return {};
    }
    std::filesystem::path normalized = std::filesystem::weakly_canonical(candidate, ec);
    return ec ? candidate.lexically_normal() : normalized;
}
//=============================================================================
std::map<std::string, std::filesystem::path>
DialogDirectoriesStore::load(bool& changed) const
{
    changed = false;
    std::map<std::string, std::filesystem::path> result;
    std::ifstream file(m_storageFile);
    if (!file.is_open()) {
        return result;
    }
    nlohmann::json data = nlohmann::json::parse(file, nullptr, false);
    if (data.is_discarded() || !data.is_object() || !data.contains("directories")
        || !data["directories"].is_object()) {
        return result;
    }
    for (const auto& item : data["directories"].items()) {
        if (item.key().empty() || !item.value().is_string()) {
            changed = true;
            continue;
        }
        std::filesystem::path directory = normalizeExistingDirectory(
            std::filesystem::u8path(item.value().get<std::string>()), false);
        if (directory.empty()) {
            changed = true;
            continue;
        }
        result[item.key()] = std::move(directory);
    }
    return result;
}
//=============================================================================
bool
DialogDirectoriesStore::save(const std::map<std::string, std::filesystem::path>& directories) const
{
    std::error_code ec;
    if (!m_storageFile.parent_path().empty()) {
        std::filesystem::create_directories(m_storageFile.parent_path(), ec);
        if (ec) {
            return false;
        }
    }
    nlohmann::json data;
    data["directories"] = nlohmann::json::object();
    for (const auto& item : directories) {
        data["directories"][item.first] = pathToUtf8(item.second);
    }
    std::ofstream file(m_storageFile, std::ios::trunc);
    if (!file.is_open()) {
        return false;
    }
    file << data.dump(2) << '\n';
    return file.good();
}
//=============================================================================
std::string
DialogDirectoriesStore::get(const std::string& key)
{
    if (key.empty()) {
        return {};
    }
    bool changed = false;
    std::map<std::string, std::filesystem::path> directories = load(changed);
    if (changed) {
        save(directories);
    }
    const auto found = directories.find(key);
    return found == directories.end() ? std::string() : pathToUtf8(found->second);
}
//=============================================================================
bool
DialogDirectoriesStore::set(const std::string& key, const std::string& directory)
{
    if (key.empty()) {
        return false;
    }
    std::filesystem::path normalized
        = normalizeExistingDirectory(std::filesystem::u8path(directory), false);
    if (normalized.empty()) {
        return false;
    }
    bool changed = false;
    std::map<std::string, std::filesystem::path> directories = load(changed);
    directories[key] = std::move(normalized);
    return save(directories);
}
//=============================================================================
std::string
DialogDirectoriesStore::resolve(
    const std::string& key, const std::string& explicitPath, const std::string& fallbackPath)
{
    std::filesystem::path explicitDirectory
        = normalizeExistingDirectory(std::filesystem::u8path(explicitPath), true);
    if (!explicitDirectory.empty()) {
        return pathToUtf8(explicitDirectory);
    }
    const std::string stored = get(key);
    if (!stored.empty()) {
        return stored;
    }
    return pathToUtf8(normalizeExistingDirectory(std::filesystem::u8path(fallbackPath), true));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
