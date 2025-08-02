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
#include "I18nManager.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
I18nManager* I18nManager::instance = nullptr;
//=============================================================================
I18nManager::I18nManager() { loadTranslations(); }
//=============================================================================
bool
I18nManager::loadTranslations()
{
    auto config = NelsonConfiguration::getInstance();
    std::wstring localesPath = config->getNelsonRootDirectory() + L"/locale/";
    std::wstring currentLocale = config->getCurrentLocale();
    std::wstring fullPath = localesPath + L"nelson-" + currentLocale + L".json";

#ifdef _MSC_VER
    std::ifstream jsonFile(fullPath);
#else
    std::ifstream jsonFile(wstring_to_utf8(fullPath));
#endif

    if (!jsonFile.is_open()) {
        return false;
    }

    try {
        translationData = nlohmann::json::parse(jsonFile);
        return true;
    } catch (const nlohmann::json::exception&) {
        return false;
    }
    return true;
}
//=============================================================================
std::string
I18nManager::convertToUtf8(const std::string& str)
{
    return str;
}
//=============================================================================
std::string
I18nManager::convertToUtf8(const std::wstring& wstr)
{
    return wstring_to_utf8(wstr);
}
//=============================================================================
std::wstring
I18nManager::convertFromUtf8(const std::string& str, const std::wstring&)
{
    return utf8_to_wstring(str);
}
//=============================================================================
std::string
I18nManager::convertFromUtf8(const std::string& str, const std::string&)
{
    return str;
}
//=============================================================================
void
I18nManager::forceToUpdateLocaleData()
{
    loadTranslations();
}
//=============================================================================
I18nManager*
I18nManager::getInstance()
{
    if (!instance) {
        instance = new I18nManager();
    }
    return instance;
}
//=============================================================================
std::wstring
I18nManager::getTextW(const std::wstring& key)
{
    if (key.empty()) {
        return key;
    }
    return getTranslationInternal(key);
}
//=============================================================================
std::wstring
I18nManager::getTextW(const std::string& key)
{
    if (key.empty()) {
        return L"";
    }
    return getTranslationInternal(utf8_to_wstring(key));
}
//=============================================================================
std::string
I18nManager::getText(const std::string& key)
{
    if (key.empty()) {
        return key;
    }
    return getTranslationInternal(key);
}
//=============================================================================
I18nManager::~I18nManager() { translationData.clear(); }
//=============================================================================
};
//=============================================================================
