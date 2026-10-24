//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <utility>
#include <vector>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
class I18nManager
{
private:
    static I18nManager* instance;
    // User-interface catalog: keyed by the English source text (fallback to the key).
    nlohmann::json translationData;
    // Error/warning catalog: keyed by the message identifier ("Nelson:module:mnemonic"),
    // value is the (translated) template with %s/%d placeholders.
    nlohmann::json errorData;
    // English error/warning catalog, loaded as a fallback when the current locale is not
    // the default one, for identifiers not yet translated in the current locale.
    nlohmann::json errorDataDefault;
    // External-module catalogs registered at runtime, as (name, root) pairs, kept to
    // re-merge them on a locale change.
    std::vector<std::pair<std::wstring, std::wstring>> moduleCatalogs;
    //=============================================================================
    I18nManager();
    //=============================================================================
    bool
    loadTranslations();
    //=============================================================================
    // Merge one registered module's catalogs (<root>/locale/<name>-ui-<locale>.json
    // into the UI catalog, <name>-errors-<locale>.json into the errors catalog)
    // for the given locale. Missing files are silently ignored.
    void
    mergeModuleCatalog(
        const std::wstring& moduleName, const std::wstring& moduleRoot, const std::wstring& locale);
    //=============================================================================
    static bool
    loadJsonFile(const std::wstring& fullPath, nlohmann::json& target);
    //=============================================================================
    static std::string
    convertToUtf8(const std::string& str);
    //=============================================================================
    static std::string
    convertToUtf8(const std::wstring& wstr);
    //=============================================================================
    static std::wstring
    convertFromUtf8(const std::string& str, const std::wstring&);
    //=============================================================================
    static std::string
    convertFromUtf8(const std::string& str, const std::string&);
    //=============================================================================
    template <typename T>
    T
    getTranslationInternal(const T& key) const
    {
        try {
            std::string keyStr = convertToUtf8(key);
            if (translationData.contains(keyStr) && translationData[keyStr].is_string()) {
                std::string translated = translationData[keyStr].get<std::string>();
                if (!translated.empty()) {
                    return convertFromUtf8(translated, key);
                }
            }
        } catch (const nlohmann::json::exception&) {
        }
        return key;
    }
    //=============================================================================
public:
    //=============================================================================
    void
    forceToUpdateLocaleData();
    //=============================================================================
    static I18nManager*
    getInstance();
    //=============================================================================
    std::wstring
    getTextW(const std::wstring& key);
    //=============================================================================
    std::wstring
    getTextW(const std::string& key);
    //=============================================================================
    std::string
    getText(const std::string& key);
    //=============================================================================
    // Resolve an error/warning message template from its identifier. Returns the template for the
    // current locale, falling back to the English catalog, then to an empty string when the
    // identifier is unknown (callers of ErrorID then use the inline English template).
    std::wstring
    getTemplateById(const std::wstring& id);
    //=============================================================================
    // The English (default-locale) template for an identifier, regardless of the current locale.
    // Used to decide whether an inline message equals the catalog template before localizing it.
    std::wstring
    getEnglishTemplateById(const std::wstring& id);
    //=============================================================================
    // Registers an external module's locale catalog (typically from the module's
    // etc/startup.m). It is merged for the current locale immediately and re-merged on
    // every locale change. Registering the same name again refreshes its root.
    void
    registerModuleCatalog(const std::wstring& moduleName, const std::wstring& moduleRoot);
    //=============================================================================
    // Drop a previously registered module catalog and rebuild the in-memory
    // catalogs from scratch (typically from the module's etc/finish.m).
    void
    unregisterModuleCatalog(const std::wstring& moduleName);
    //=============================================================================
    ~I18nManager();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
