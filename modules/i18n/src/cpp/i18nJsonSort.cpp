//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <nlohmann/json.hpp>
#include <fstream>
#include <sstream>
#include "i18nJsonSort.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
i18nJsonSort(
    const std::wstring& jsonSource, const std::wstring& jsonDestination, std::wstring& errorMessage)
{
    try {
        nlohmann::ordered_json sourceJson;
        nlohmann::ordered_json destJson;

        {
#ifdef _MSC_VER
            std::wifstream sourceFile(jsonSource);
            if (!sourceFile.is_open()) {
                errorMessage = L"Unable to open source JSON file.";
                return false;
            }
            std::wstringstream wss;
            wss << sourceFile.rdbuf();
            sourceFile.close();
            std::string utf8Content = wstring_to_utf8(wss.str());
            sourceJson = nlohmann::json::parse(utf8Content);
#else
            std::ifstream sourceFile(wstring_to_utf8(jsonSource));
            if (!sourceFile.is_open()) {
                errorMessage = L"Unable to open source JSON file.";
                return false;
            }
            sourceFile >> sourceJson;
#endif
        }

        std::function<nlohmann::ordered_json(const nlohmann::ordered_json&)> sortJson
            = [&sortJson](const nlohmann::ordered_json& j) -> nlohmann::ordered_json {
            if (j.is_object()) {
                nlohmann::ordered_json sorted;
                std::vector<std::string> keys;
                for (const auto& item : j.items()) {
                    keys.push_back(item.key());
                }
                std::sort(keys.begin(), keys.end());

                for (const auto& key : keys) {
                    sorted[key] = sortJson(j[key]);
                }
                return sorted;
            } else if (j.is_array()) {
                nlohmann::ordered_json sorted = nlohmann::ordered_json::array();
                for (const auto& element : j) {
                    sorted.push_back(sortJson(element));
                }
                return sorted;
            }
            return j;
        };

        destJson = sortJson(sourceJson);

        {
#ifdef _MSC_VER
            std::wofstream destFile(jsonDestination);
#else
            std::ofstream destFile(wstring_to_utf8(jsonDestination));
#endif
            if (!destFile.is_open()) {
                errorMessage = L"Unable to open destination JSON file.";
                return false;
            }
#ifdef _MSC_VER
            destFile << utf8_to_wstring(destJson.dump(2));
#else
            destFile << destJson.dump(2);

#endif
        }

        return true;
    } catch (const nlohmann::json::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
