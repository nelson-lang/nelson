//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <fstream>
#include <mutex>
#include <sstream>
#include "NmmGuiPrefs.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::NmmGuiGateway {
//=============================================================================
static std::filesystem::path
prefsPath();
static std::mutex&
prefsMutex();
//=============================================================================
constexpr int DEFAULT_WIDTH = 980;
constexpr int DEFAULT_HEIGHT = 720;
//=============================================================================
nlohmann::json
readNmmGuiPrefs()
{
    std::lock_guard<std::mutex> lock(prefsMutex());
    std::ifstream file(prefsPath());
    if (!file) {
        return nlohmann::json::object();
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    const nlohmann::json prefs = nlohmann::json::parse(buffer.str(), nullptr, false);
    return prefs.is_object() ? prefs : nlohmann::json::object();
}
//=============================================================================
nlohmann::json
mergeNmmGuiPrefs(const nlohmann::json& changes)
{
    nlohmann::json prefs = readNmmGuiPrefs();
    if (changes.is_object()) {
        prefs.update(changes);
    }
    std::lock_guard<std::mutex> lock(prefsMutex());
    std::ofstream file(prefsPath(), std::ios::trunc);
    if (file) {
        file << prefs.dump(2);
    }
    return prefs;
}
//=============================================================================
void
nmmGuiWindowSize(int& width, int& height)
{
    width = DEFAULT_WIDTH;
    height = DEFAULT_HEIGHT;
    const nlohmann::json prefs = readNmmGuiPrefs();
    const int w = prefs.value("width", 0);
    const int h = prefs.value("height", 0);
    // Ignore sizes a broken save or another screen could leave behind.
    if (w >= 640 && w <= 8000 && h >= 480 && h <= 8000) {
        width = w;
        height = h;
    }
}
//=============================================================================
static std::filesystem::path
prefsPath()
{
    return std::filesystem::path(
               NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory())
        / L"nmm_gui.json";
}
//=============================================================================
static std::mutex&
prefsMutex()
{
    static auto* mutex = new std::mutex;
    return *mutex;
}
//=============================================================================
} // namespace Nelson::NmmGuiGateway
//=============================================================================
