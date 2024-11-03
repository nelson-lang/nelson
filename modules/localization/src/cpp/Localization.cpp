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
#include <clocale>
#include <fstream>
#include <regex>
#include "Localization.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
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
        NelsonConfiguration::getInstance()->setCurrentLocale(lang);
        forceRefreshLocale();
        try {
            const std::string defaultLang
                = wstring_to_utf8(NelsonConfiguration::getInstance()->getDefaultLocale())
                + std::string(".UTF-8");

            std::locale base = std::locale::classic();

            std::locale us_locale(defaultLang);
            std::locale mixed_locale(us_locale, base, std::locale::numeric);
            std::locale::global(mixed_locale);

            setlocale(LC_NUMERIC, "C");

        } catch (const std::exception&) {
            std::locale::global(std::locale::classic());
            setlocale(LC_ALL, "C");
        }
    }
}
//=============================================================================
void
Localization::initLanguageSupported()
{
    if (LanguageSupported.empty()) {
        auto config = NelsonConfiguration::getInstance();
        std::wstring localesPath = config->getNelsonRootDirectory() + L"/locale/";
        std::filesystem::path dir = localesPath;
        std::filesystem::path r = dir.root_path();
        if (std::filesystem::is_directory(dir.wstring())) {
            std::wregex localeRegex(L"nelson-([a-z]{2}_[A-Z]{2})\\.json");
            try {
                for (std::filesystem::directory_iterator p(dir.native()), end; p != end; ++p) {

                    if (std::filesystem::is_regular_file(p->path())) {
                        std::wstring filename = p->path().filename().wstring();
                        std::wsmatch match;
                        if (std::regex_match(filename, match, localeRegex)) {
                            std::wstring localeCode = match[1];
                            LanguageSupported.push_back(localeCode);
                        }
                    }
                }
            } catch (const std::filesystem::filesystem_error&) {
                LanguageSupported.clear();
                LanguageSupported.push_back(NelsonConfiguration::getInstance()->getDefaultLocale());
            }
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
    return NelsonConfiguration::getInstance()->getDefaultLocale();
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
    std::wstring effectiveLang = NelsonConfiguration::getInstance()->getDefaultLocale();
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
        effectiveLang.assign(NelsonConfiguration::getInstance()->getDefaultLocale());
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
