//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiPreferences.hpp"
#include "NelsonConfiguration.hpp"
#include <filesystem>
#include <fstream>
#include <mutex>
#include <stdexcept>
#include <string>
//=============================================================================
extern "C"
{
#include "webui.h"
}
#include "WebUiWindowGeometry.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr int PREFERENCES_SCHEMA = 1;
static constexpr std::size_t MAX_PREFERENCES_SIZE = 2 * 1024 * 1024;
static constexpr unsigned int DEFAULT_WINDOW_WIDTH = 1200;
static constexpr unsigned int DEFAULT_WINDOW_HEIGHT = 800;
static constexpr int MIN_WINDOW_WIDTH = 800;
static constexpr int MIN_WINDOW_HEIGHT = 600;
static constexpr const char* WINDOW_GEOMETRY_KEY = "nelson-web.window.geometry";
static constexpr const char* WINDOW_COORDINATE_SPACE = "native-window";
static std::mutex preferencesMutex;
//=============================================================================
static std::filesystem::path
preferencesPath()
{
    return std::filesystem::path(
               NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory())
        / L"web-gui-ui-prefs.json";
}
//=============================================================================
static nlohmann::json
emptyPreferences()
{
    return { { "schema", PREFERENCES_SCHEMA }, { "values", nlohmann::json::object() },
        { "path", preferencesPath().u8string() } };
}
//=============================================================================
static nlohmann::json
readPreferencesUnlocked()
{
    std::error_code ec;
    const std::filesystem::path path = preferencesPath();
    if (!std::filesystem::exists(path, ec) || ec) {
        return emptyPreferences();
    }
    const auto size = std::filesystem::file_size(path, ec);
    if (ec || size > MAX_PREFERENCES_SIZE) {
        return emptyPreferences();
    }
    std::ifstream input(path, std::ios::binary);
    nlohmann::json value = nlohmann::json::parse(input, nullptr, false);
    if (value.is_discarded() || !value.is_object() || !value.contains("values")
        || !value["values"].is_object()) {
        return emptyPreferences();
    }
    value["schema"] = PREFERENCES_SCHEMA;
    value["path"] = path.u8string();
    return value;
}
//=============================================================================
nlohmann::json
loadWebGuiPreferences()
{
    std::lock_guard<std::mutex> lock(preferencesMutex);
    return readPreferencesUnlocked();
}
//=============================================================================
static nlohmann::json
validatedValues(const nlohmann::json& values)
{
    if (!values.is_object() || values.size() > 64) {
        throw std::runtime_error("invalid desktop preferences");
    }
    nlohmann::json result = nlohmann::json::object();
    for (const auto& [key, value] : values.items()) {
        if (key.size() > 128 || !value.is_string()
            || value.get_ref<const std::string&>().size() > MAX_PREFERENCES_SIZE) {
            throw std::runtime_error("invalid desktop preference value");
        }
        result[key] = value;
    }
    return result;
}
//=============================================================================
static void
writePreferencesUnlocked(const nlohmann::json& values)
{
    const nlohmann::json document = { { "schema", PREFERENCES_SCHEMA }, { "values", values } };
    const std::string serialized = document.dump(2);
    if (serialized.size() > MAX_PREFERENCES_SIZE) {
        throw std::runtime_error("desktop preferences are too large");
    }

    const std::filesystem::path path = preferencesPath();
    std::error_code ec;
    std::filesystem::create_directories(path.parent_path(), ec);
    if (ec) {
        throw std::runtime_error("unable to create preferences directory");
    }
    std::filesystem::path temporary = path;
    temporary += L".tmp";
    {
        std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
        output << serialized;
        if (!output) {
            throw std::runtime_error("unable to write desktop preferences");
        }
    }
    std::filesystem::remove(path, ec);
    ec.clear();
    std::filesystem::rename(temporary, path, ec);
    if (ec) {
        std::filesystem::remove(temporary, ec);
        throw std::runtime_error("unable to replace desktop preferences");
    }
}
//=============================================================================
void
saveWebGuiPreferences(const nlohmann::json& values)
{
    nlohmann::json updatedValues = validatedValues(values);
    std::lock_guard<std::mutex> lock(preferencesMutex);
    const nlohmann::json current = readPreferencesUnlocked();
    const auto& currentValues = current["values"];
    const auto geometry = currentValues.find(WINDOW_GEOMETRY_KEY);
    if (geometry != currentValues.end() && geometry->is_string()) {
        updatedValues[WINDOW_GEOMETRY_KEY] = *geometry;
    }
    writePreferencesUnlocked(updatedValues);
}
//=============================================================================
static WebUiWindowGeometry
parseWindowGeometry(const nlohmann::json& values)
{
    WebUiWindowGeometry geometry;
    const auto iterator = values.find(WINDOW_GEOMETRY_KEY);
    if (iterator == values.end() || !iterator->is_string()) {
        return geometry;
    }
    const nlohmann::json value
        = nlohmann::json::parse(iterator->get<std::string>(), nullptr, false);
    if (value.is_discarded() || !value.is_object()
        || value.value("coordinateSpace", "") != WINDOW_COORDINATE_SPACE) {
        return geometry;
    }
    geometry.x = value.value("x", 0);
    geometry.y = value.value("y", 0);
    geometry.width = value.value("width", 0);
    geometry.height = value.value("height", 0);
    geometry.maximized = value.value("maximized", false);
    validateWebUiWindowGeometry(geometry, MIN_WINDOW_WIDTH, MIN_WINDOW_HEIGHT, 10000);
    return geometry;
}
//=============================================================================
void
applyWebGuiWindowGeometry(size_t window)
{
    webui_set_size(window, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);

    const nlohmann::json preferences = loadWebGuiPreferences();
    const WebUiWindowGeometry geometry = parseWindowGeometry(preferences["values"]);
    if (!geometry.valid) {
        return;
    }
    webui_set_size(window, static_cast<unsigned int>(geometry.width),
        static_cast<unsigned int>(geometry.height));
    if (geometry.x >= 0 && geometry.y >= 0) {
        webui_set_position(
            window, static_cast<unsigned int>(geometry.x), static_cast<unsigned int>(geometry.y));
    }
    restoreWebUiWindowGeometry(window, geometry);
}
//=============================================================================
void
saveWebGuiWindowGeometry(size_t window)
{
    const WebUiWindowGeometry geometry = captureWebUiWindowGeometry(window);
    if (!geometry.valid) {
        return;
    }
    const nlohmann::json value = { { "coordinateSpace", WINDOW_COORDINATE_SPACE },
        { "x", geometry.x }, { "y", geometry.y }, { "width", geometry.width },
        { "height", geometry.height }, { "maximized", geometry.maximized } };

    std::lock_guard<std::mutex> lock(preferencesMutex);
    nlohmann::json preferences = readPreferencesUnlocked();
    preferences["values"][WINDOW_GEOMETRY_KEY] = value.dump();
    writePreferencesUnlocked(preferences["values"]);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
