//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <nlohmann/json.hpp>
#include <fstream>
#include <sstream>
#include "i18nJsonMerger.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
i18nJsonMerger(
    const std::wstring& jsonSource, const std::wstring& jsonDestination, std::wstring& errorMessage)
{
    try {
        // Create JSON objects for source and destination
        nlohmann::ordered_json sourceJson;
        nlohmann::ordered_json destJson;

        // Read source JSON
        {
#ifdef _MSC_VER
            std::wifstream sourceFile(wstring_to_utf8(jsonSource));
            if (!sourceFile.is_open()) {
                errorMessage = _W("Unable to open source JSON file.");
                return false;
            }
            std::wstringstream wss;
            wss << sourceFile.rdbuf();
            sourceFile.close();
            // Convert wide string to UTF-8
            std::string utf8Content = wstring_to_utf8(wss.str());
            sourceJson = nlohmann::json::parse(utf8Content);
#else
            std::ifstream sourceFile(wstring_to_utf8(jsonSource));
            if (!sourceFile.is_open()) {
                errorMessage = _W("Unable to open source JSON file.");
                return false;
            }
            sourceFile >> sourceJson;
#endif
        }

        // Read destination JSON
        {
#ifdef _MSC_VER
            std::wifstream destFile(wstring_to_utf8(jsonDestination));
            if (!destFile.is_open()) {
                errorMessage = _W("Unable to open destination JSON file.");
                return false;
            }
            std::wstringstream wss;
            wss << destFile.rdbuf();
            destFile.close();
            // Convert wide string to UTF-8
            std::string utf8Content = wstring_to_utf8(wss.str());
            destJson = nlohmann::json::parse(utf8Content);
#else
            std::ifstream destFile(wstring_to_utf8(jsonDestination));
            if (!destFile.is_open()) {
                errorMessage = _W("Unable to open destination JSON file.");
                return false;
            }
            destFile >> destJson;
#endif
        }

        // Create new JSON object for merged result
        nlohmann::ordered_json mergedJson = nlohmann::json::object();

        // Iterate through source JSON
        for (auto it = sourceJson.begin(); it != sourceJson.end(); ++it) {
            const std::string& key = it.key();
            if (!key.empty()) {
                // If key exists in destination, use destination's value
                if (destJson.contains(key)) {
                    mergedJson[key] = destJson[key];
                }
                // If key only exists in source, add it with empty string
                else {
                    mergedJson[key] = "";
                }
            }
        }

        // Write the merged result back to destination file
        {
#ifdef _MSC_VER
            std::wofstream destFile(jsonDestination);
#else
            std::ofstream destFile(wstring_to_utf8(jsonDestination));
#endif
            if (!destFile.is_open()) {
                errorMessage = _W("Unable to write to destination JSON file.");
                return false;
            }
#ifdef _MSC_VER
            destFile << utf8_to_wstring(mergedJson.dump(2));
            destFile << L"\n";
#else
            destFile << mergedJson.dump(2);
            destFile << "\n";
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
}
//=============================================================================
