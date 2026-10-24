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
I18nManager::loadJsonFile(const std::wstring& fullPath, nlohmann::json& target)
{
#ifdef _MSC_VER
    std::ifstream jsonFile(fullPath);
#else
    std::ifstream jsonFile(wstring_to_utf8(fullPath));
#endif
    if (!jsonFile.is_open()) {
        target = nlohmann::json::object();
        return false;
    }
    try {
        target = nlohmann::json::parse(jsonFile);
        return true;
    } catch (const nlohmann::json::exception&) {
        target = nlohmann::json::object();
        return false;
    }
}
//=============================================================================
bool
I18nManager::loadTranslations()
{
    auto config = NelsonConfiguration::getInstance();
    std::wstring localesPath = config->getNelsonRootDirectory() + L"/locale/";
    std::wstring currentLocale = config->getCurrentLocale();
    std::wstring defaultLocale = config->getDefaultLocale();

    bool uiLoaded
        = loadJsonFile(localesPath + L"nelson-ui-" + currentLocale + L".json", translationData);

    loadJsonFile(localesPath + L"nelson-errors-" + currentLocale + L".json", errorData);
    if (currentLocale != defaultLocale) {
        loadJsonFile(localesPath + L"nelson-errors-" + defaultLocale + L".json", errorDataDefault);
    } else {
        errorDataDefault = nlohmann::json::object();
    }

    // Layer the opt-in external-module catalogs on top of the core ones, so a
    // locale change re-applies them too.
    for (const auto& entry : moduleCatalogs) {
        mergeModuleCatalog(entry.first, entry.second, currentLocale);
    }
    return uiLoaded;
}
//=============================================================================
void
I18nManager::mergeModuleCatalog(
    const std::wstring& moduleName, const std::wstring& moduleRoot, const std::wstring& locale)
{
    std::wstring base = moduleRoot + L"/locale/" + moduleName + L"-";
    // UI catalog (keyed by source text): a module entry only overrides its own
    // source strings, so a plain assignment is enough. Empty values are skipped
    // so the runtime keeps falling back to the English key.
    nlohmann::json moduleUi;
    if (loadJsonFile(base + L"ui-" + locale + L".json", moduleUi) && moduleUi.is_object()) {
        for (auto it = moduleUi.begin(); it != moduleUi.end(); ++it) {
            if (it.value().is_string() && !it.value().get<std::string>().empty()) {
                translationData[it.key()] = it.value();
            }
        }
    }
    nlohmann::json moduleErrors;
    if (loadJsonFile(base + L"errors-" + locale + L".json", moduleErrors)
        && moduleErrors.is_object()) {
        for (auto it = moduleErrors.begin(); it != moduleErrors.end(); ++it) {
            if (it.value().is_string() && !it.value().get<std::string>().empty()) {
                errorData[it.key()] = it.value();
            }
        }
    }
    // English (default-locale) errors, merged into the fallback catalog so that
    // error('Nelson:<module>:id', 'english template') still resolves the English
    // template by id when the current locale is not the default, and gets
    // localized against errorData.
    std::wstring defaultLocale = NelsonConfiguration::getInstance()->getDefaultLocale();
    if (locale != defaultLocale) {
        nlohmann::json moduleErrorsDefault;
        if (loadJsonFile(base + L"errors-" + defaultLocale + L".json", moduleErrorsDefault)
            && moduleErrorsDefault.is_object()) {
            for (auto it = moduleErrorsDefault.begin(); it != moduleErrorsDefault.end(); ++it) {
                if (it.value().is_string() && !it.value().get<std::string>().empty()) {
                    errorDataDefault[it.key()] = it.value();
                }
            }
        }
    }
}
//=============================================================================
void
I18nManager::registerModuleCatalog(const std::wstring& moduleName, const std::wstring& moduleRoot)
{
    for (auto& entry : moduleCatalogs) {
        if (entry.first == moduleName) {
            entry.second = moduleRoot;
            mergeModuleCatalog(
                moduleName, moduleRoot, NelsonConfiguration::getInstance()->getCurrentLocale());
            return;
        }
    }
    moduleCatalogs.emplace_back(moduleName, moduleRoot);
    mergeModuleCatalog(
        moduleName, moduleRoot, NelsonConfiguration::getInstance()->getCurrentLocale());
}
//=============================================================================
void
I18nManager::unregisterModuleCatalog(const std::wstring& moduleName)
{
    bool removed = false;
    for (auto it = moduleCatalogs.begin(); it != moduleCatalogs.end(); ++it) {
        if (it->first == moduleName) {
            moduleCatalogs.erase(it);
            removed = true;
            break;
        }
    }
    if (removed) {
        // Rebuild from scratch so the removed module's strings drop out.
        loadTranslations();
    }
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
std::wstring
I18nManager::getTemplateById(const std::wstring& id)
{
    if (id.empty()) {
        return L"";
    }
    try {
        std::string idStr = wstring_to_utf8(id);
        if (errorData.contains(idStr) && errorData[idStr].is_string()) {
            std::string tmpl = errorData[idStr].get<std::string>();
            if (!tmpl.empty()) {
                return utf8_to_wstring(tmpl);
            }
        }
        if (errorDataDefault.contains(idStr) && errorDataDefault[idStr].is_string()) {
            std::string tmpl = errorDataDefault[idStr].get<std::string>();
            if (!tmpl.empty()) {
                return utf8_to_wstring(tmpl);
            }
        }
    } catch (const nlohmann::json::exception&) {
    }
    return L"";
}
//=============================================================================
std::wstring
I18nManager::getEnglishTemplateById(const std::wstring& id)
{
    if (id.empty()) {
        return L"";
    }
    try {
        std::string idStr = wstring_to_utf8(id);
        // errorDataDefault holds the English catalog when the current locale is not the default;
        // when it is the default, errorData itself is the English catalog.
        if (errorDataDefault.contains(idStr) && errorDataDefault[idStr].is_string()) {
            std::string tmpl = errorDataDefault[idStr].get<std::string>();
            if (!tmpl.empty()) {
                return utf8_to_wstring(tmpl);
            }
        }
        if (errorData.contains(idStr) && errorData[idStr].is_string()) {
            std::string tmpl = errorData[idStr].get<std::string>();
            if (!tmpl.empty()) {
                return utf8_to_wstring(tmpl);
            }
        }
    } catch (const nlohmann::json::exception&) {
    }
    return L"";
}
//=============================================================================
I18nManager::~I18nManager()
{
    translationData.clear();
    errorData.clear();
    errorDataDefault.clear();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
