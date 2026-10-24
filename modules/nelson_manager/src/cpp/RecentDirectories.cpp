//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RecentDirectories.hpp"
#include "RecentDirectoriesStore.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::mutex recentDirectoriesMutex;
//=============================================================================
static std::filesystem::path
recentDirectoriesFile()
{
    const std::wstring preferences
        = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
#ifdef _MSC_VER
    return std::filesystem::path(preferences) / L"recent_directories.json";
#else
    return std::filesystem::u8path(wstring_to_utf8(preferences)) / "recent_directories.json";
#endif
}
//=============================================================================
std::vector<std::wstring>
getRecentDirectories()
{
    std::lock_guard<std::mutex> lock(recentDirectoriesMutex);
    const std::vector<std::string> stored
        = RecentDirectoriesStore(recentDirectoriesFile()).directories();
    std::vector<std::wstring> result;
    result.reserve(stored.size());
    for (const std::string& directory : stored) {
        result.emplace_back(utf8_to_wstring(directory));
    }
    return result;
}
//=============================================================================
bool
addRecentDirectory(const std::wstring& directory)
{
    std::lock_guard<std::mutex> lock(recentDirectoriesMutex);
    return RecentDirectoriesStore(recentDirectoriesFile()).add(wstring_to_utf8(directory));
}
//=============================================================================
std::wstring
getFirstValidRecentDirectory()
{
    const std::vector<std::wstring> directories = getRecentDirectories();
    return directories.empty() ? std::wstring() : directories.front();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
