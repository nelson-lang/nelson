//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <unordered_map>
#include <nlohmann/json.hpp>
#include "TranslationManager.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct TranslationManager::TranslationManagerData
{
    std::unordered_map<std::wstring, std::wstring> uiTranslationData;
    std::unordered_map<std::wstring, std::wstring> errorsTranslationData;
};
//=============================================================================
namespace {
    //=============================================================================
    bool
    loadTranslationFile(const std::wstring& filePath,
        std::unordered_map<std::wstring, std::wstring>& translationData, bool overwriteExisting)
    {
#ifdef _MSC_VER
        std::ifstream jsonFile(filePath);
#else
        std::ifstream jsonFile(wstring_to_utf8(filePath));
#endif

        if (!jsonFile.is_open()) {
            return false;
        }

        try {
            nlohmann::json jsonData = nlohmann::json::parse(jsonFile);
            for (const auto& [key, value] : jsonData.items()) {
                if (value.is_string()) {
                    std::wstring wkey = utf8_to_wstring(key);
                    std::wstring wvalue = utf8_to_wstring(value.get<std::string>());
                    if (overwriteExisting) {
                        translationData[wkey] = std::move(wvalue);
                    } else {
                        translationData.try_emplace(std::move(wkey), std::move(wvalue));
                    }
                }
            }
        } catch (const nlohmann::json::exception&) {
            fprintf(stderr, "Error parsing JSON file: %s\n", wstring_to_utf8(filePath).c_str());
            return false;
        }

        return true;
    }
    //=============================================================================
    std::wstring
    getTranslationPath(
        const std::wstring& modulePath, const std::wstring& locale, const std::wstring& fileName)
    {
        std::wstring localizedPath = modulePath + L"/i18n/" + locale + L"/" + fileName;
        if (FileSystemWrapper::Path(localizedPath).is_regular_file()) {
            return localizedPath;
        }
        return L"";
    }
    //=============================================================================
}
//=============================================================================
TranslationManager::TranslationManager() : data(new TranslationManagerData()) { }
//=============================================================================
TranslationManager::~TranslationManager()
{
    delete data;
    data = nullptr;
}
//=============================================================================
bool
TranslationManager::loadModuleTranslation(
    const std::wstring& moduleName, const std::wstring& modulePath, const std::wstring& locale)
{
    std::wstring errorsEnPath = getTranslationPath(modulePath, L"en_US", L"errors.json");
    std::wstring uiEnPath = getTranslationPath(modulePath, L"en_US", L"messages.json");
    std::wstring errorsLocalePath = getTranslationPath(modulePath, locale, L"errors.json");
    std::wstring uiLocalePath = getTranslationPath(modulePath, locale, L"messages.json");

    if (errorsEnPath.empty() || uiEnPath.empty()) {
        return false;
    }

    bool errorsLoaded = loadTranslationFile(errorsEnPath, data->errorsTranslationData, false);
    bool uiLoaded = loadTranslationFile(uiEnPath, data->uiTranslationData, false);
    if (locale == L"en_US") {
        return errorsLoaded || uiLoaded;
    }

    if (!errorsLocalePath.empty()) {
        errorsLoaded = loadTranslationFile(errorsLocalePath, data->errorsTranslationData, true)
            || errorsLoaded;
    }
    if (!uiLocalePath.empty()) {
        uiLoaded = loadTranslationFile(uiLocalePath, data->uiTranslationData, true) || uiLoaded;
    }

    return errorsLoaded || uiLoaded;
}
//=============================================================================
std::wstring
TranslationManager::getMessage(const std::wstring& key) const
{
    auto it = data->uiTranslationData.find(key);
    return (it != data->uiTranslationData.end()) ? it->second : key;
}
//=============================================================================
std::string
TranslationManager::getMessage(const std::string& key) const
{
    auto it = data->uiTranslationData.find(utf8_to_wstring(key));
    return (it != data->uiTranslationData.end()) ? wstring_to_utf8(it->second) : key;
}
//=============================================================================
std::wstring
TranslationManager::getError(const std::wstring& key) const
{
    auto it = data->errorsTranslationData.find(key);
    return (it != data->errorsTranslationData.end()) ? it->second : key;
}
//=============================================================================
std::string
TranslationManager::getError(const std::string& key) const
{
    auto it = data->errorsTranslationData.find(utf8_to_wstring(key));
    return (it != data->errorsTranslationData.end()) ? wstring_to_utf8(it->second) : key;
}
//=============================================================================
void
TranslationManager::reset()
{
    data->uiTranslationData.clear();
    data->errorsTranslationData.clear();
}
//=============================================================================
}
//=============================================================================
