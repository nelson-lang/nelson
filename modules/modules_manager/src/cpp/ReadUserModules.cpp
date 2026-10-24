//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <fstream>
#include <nlohmann/json.hpp>
#include "ReadUserModules.hpp"
#include "GetExternalModulesPath.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ReadUserModules(
    std::vector<std::tuple<std::wstring, std::wstring, bool>>& userModules, bool reverse)
{
    std::wstring modulesJsonPath = GetExternalModulesPath() + std::wstring(L"modules.json");
#ifdef _MSC_VER
    std::ifstream jsonFile(modulesJsonPath);
#else
    std::ifstream jsonFile(wstring_to_utf8(modulesJsonPath));
#endif
    if (!jsonFile.is_open()) {
        return true;
    }
    nlohmann::json data;
    try {
        data = nlohmann::json::parse(jsonFile);
    } catch (const nlohmann::json::exception&) {
        return false;
    }
    jsonFile.close();
    if (data.is_array() && data.empty()) {
        return true;
    }
    if (!data.is_object()) {
        return false;
    }
    for (auto it = data.begin(); it != data.end(); ++it) {
        const auto& moduleData = it.value();
        if (!moduleData.is_object() || !moduleData.contains("path")
            || !moduleData["path"].is_string() || !moduleData.contains("load")
            || !moduleData["load"].is_boolean() || !moduleData.contains("version")
            || !moduleData["version"].is_string()) {
            return false;
        }
        std::tuple<std::wstring, std::wstring, bool> module = std::make_tuple(
            utf8_to_wstring(it.key()), utf8_to_wstring(moduleData["path"].get<std::string>()),
            moduleData["load"].get<bool>());
        userModules.push_back(module);
    }
    if (reverse) {
        if (!userModules.empty()) {
            std::reverse(userModules.begin(), userModules.end());
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
