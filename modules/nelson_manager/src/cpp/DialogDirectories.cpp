//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DialogDirectories.hpp"
#include "DialogDirectoriesStore.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::mutex dialogDirectoriesMutex;
//=============================================================================
static std::filesystem::path
dialogDirectoriesFile()
{
    const std::wstring preferences
        = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
#ifdef _MSC_VER
    return std::filesystem::path(preferences) / L"dialog_directories.json";
#else
    return std::filesystem::u8path(wstring_to_utf8(preferences)) / "dialog_directories.json";
#endif
}
//=============================================================================
std::wstring
getDialogDirectory(const std::wstring& key)
{
    std::lock_guard<std::mutex> lock(dialogDirectoriesMutex);
    return utf8_to_wstring(
        DialogDirectoriesStore(dialogDirectoriesFile()).get(wstring_to_utf8(key)));
}
//=============================================================================
bool
setDialogDirectory(const std::wstring& key, const std::wstring& path)
{
    std::lock_guard<std::mutex> lock(dialogDirectoriesMutex);
    return DialogDirectoriesStore(dialogDirectoriesFile())
        .set(wstring_to_utf8(key), wstring_to_utf8(path));
}
//=============================================================================
std::wstring
resolveDialogDirectory(
    const std::wstring& key, const std::wstring& explicitPath, const std::wstring& fallbackPath)
{
    std::lock_guard<std::mutex> lock(dialogDirectoriesMutex);
    return utf8_to_wstring(DialogDirectoriesStore(dialogDirectoriesFile())
            .resolve(wstring_to_utf8(key), wstring_to_utf8(explicitPath),
                wstring_to_utf8(fallbackPath)));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
