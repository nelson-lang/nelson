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
#include <boost/function.hpp>
#include <boost/locale.hpp>
#include <boost/locale/generator.hpp>
#include <clocale>
#include <fstream>
#include "Localization.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Localization* Localization::m_pInstance = nullptr;
//=============================================================================
Localization::Localization() { initLanguageSupported(); }
//=============================================================================
Localization*
Localization::Instance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new Localization();
    }
    return m_pInstance;
}
//=============================================================================
void
Localization::destroy()
{
    if (m_pInstance) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
void
Localization::setLanguageEnvironment(const std::wstring& lang)
{
    if (isSupportedLanguage(lang)) {
        std::wstring localesPath
            = NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/locale/";
        boost::locale::generator gen;
        try {
            gen.add_messages_path(wstring_to_utf8(localesPath));
            gen.add_messages_domain("nelson");
            std::string effectiveLang = wstring_to_utf8(lang);
            const std::string langDesired = effectiveLang + std::string(".UTF-8");
            std::locale::global(gen(langDesired));
        } catch (const std::exception&) { //-V565
        }
        setlocale(LC_NUMERIC, "C");
    }
}
//=============================================================================
void
Localization::initLanguageSupported()
{
    if (LanguageSupported.empty()) {
        std::wstring langsconf
            = NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/etc/languages.conf";
        std::string jsonString;
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(langsconf);
#else
        std::ifstream jsonFile(wstring_to_utf8(langsconf));
#endif
        if (!jsonFile.is_open()) {
            LanguageSupported.push_back(std::wstring(L"en_US"));
            return;
        }
        nlohmann::json data;
        try {
            data = nlohmann::json::parse(jsonFile);
        } catch (const nlohmann::json::exception&) {
            jsonFile.close();
            LanguageSupported.push_back(std::wstring(L"en_US"));
            return;
        }

        jsonFile.close();

        nlohmann::json languages;
        try {
            languages = data["supported_languages"];
        } catch (const nlohmann::json::exception&) {
            LanguageSupported.push_back(std::wstring(L"en_US"));
            return;
        }
        if (!languages.is_null() && languages.is_array()) {
            try {
                for (nlohmann::json::iterator it = languages.begin(); it != languages.end(); ++it) {
                    std::string value = *it;
                    LanguageSupported.push_back(utf8_to_wstring(value));
                }
            } catch (const nlohmann::json::exception&) {
                LanguageSupported.clear();
                LanguageSupported.push_back(std::wstring(L"en_US"));
                return;
            }
        } else {
            LanguageSupported.push_back(std::wstring(L"en_US"));
        }
    }
}
//=============================================================================
std::wstring
Localization::getCurrentLanguage()
{
    return currentLanguage;
}
//=============================================================================
std::wstring
Localization::getDefaultLanguage()
{
    return L"en_US";
}
//=============================================================================
bool
Localization::setLanguage(const std::wstring& lang, bool save)
{
    if (isSupportedLanguage(lang)) {
        setLanguageEnvironment(lang);
        currentLanguage = lang;
        if (!save) {
            return true;
        }
        std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
        std::wstring prefFile = prefDir + L"/nelson.conf";
        std::string jsonString;
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(prefFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(prefFile));
#endif
        nlohmann::json data;
        if (jsonFile.is_open()) {
            try {
                data = nlohmann::json::parse(jsonFile);
            } catch (const nlohmann::json::exception&) {
                jsonFile.close();
                return false;
            }
            jsonFile.close();
        }
        data["language"] = wstring_to_utf8(lang);
#ifdef _MSC_VER
        std::ofstream out(prefFile);
#else
        std::ofstream out(wstring_to_utf8(prefFile));
#endif
        if (out.is_open()) {
            out << data.dump(4);
            out.close();
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Localization::getManagedLanguages(wstringVector& langs)
{
    langs.clear();
    for (const auto& k : LanguageSupported) {
        langs.push_back(k);
    }
    return true;
}
//=============================================================================
std::wstring
Localization::initializeLocalization(const std::wstring& lang)
{
    std::wstring effectiveLang = L"en_US";
    std::wstring _lang(lang);

    initLanguageSupported();
    if (lang.empty()) {
        std::wstring language_saved = L"";
        std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
        std::wstring prefFile = prefDir + L"/nelson.conf";
#ifdef _MSC_VER
        std::ifstream jsonFile(prefFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(prefFile));
#endif
        if (jsonFile.is_open()) {
            try {
                nlohmann::json data = nlohmann::json::parse(jsonFile);
                if (data["language"].is_string()) {
                    std::string value = data["language"];
                    language_saved = utf8_to_wstring(value);
                }
            } catch (const nlohmann::json::exception&) {
                language_saved.clear();
            }
            jsonFile.close();
        }
        _lang = language_saved;
    }
    if (isSupportedLanguage(_lang)) {
        effectiveLang.assign(_lang);
    } else {
        effectiveLang.assign(L"en_US");
    }
    setLanguageEnvironment(effectiveLang);
    return effectiveLang;
}
//=============================================================================
bool
Localization::isSupportedLanguage(const std::wstring& lang)
{
    for (auto& k : LanguageSupported) {
        if (wcscmp(lang.c_str(), k.c_str()) == 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
