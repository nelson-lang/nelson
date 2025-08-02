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
    if (jsonFile.is_open()) {
        nlohmann::json data;
        bool parsed = false;
        if (jsonFile.is_open()) {
            try {
                data = nlohmann::json::parse(jsonFile);
                parsed = true;
            } catch (const nlohmann::json::exception&) {
                parsed = false;
            }
            jsonFile.close();
        }
        if (parsed) {
            for (auto it = data.begin(); it != data.end(); ++it) {
                std::string module_name = it.key();
                std::string module_path = data[module_name]["path"];
                bool module_load = data[module_name]["load"];
                std::string module_version = data[module_name]["version"];
                std::tuple<std::wstring, std::wstring, bool> module = std::make_tuple(
                    utf8_to_wstring(module_name), utf8_to_wstring(module_path), module_load);
                userModules.push_back(module);
            }
        }
        if (reverse) {
            if (!userModules.empty()) {
                std::reverse(userModules.begin(), userModules.end());
            }
        }
    }
    return !userModules.empty();
}
//=============================================================================
}
//=============================================================================
