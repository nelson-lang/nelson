//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
class I18nManager
{
private:
    static I18nManager* instance;
    nlohmann::json translationData;
    //=============================================================================
    I18nManager();
    //=============================================================================
    bool
    loadTranslations();
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
    ~I18nManager();
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
