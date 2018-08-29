//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Localization.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include <boost/foreach.hpp>
#include <boost/function.hpp>
#include <boost/locale.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <clocale>
//=============================================================================
namespace Nelson {
//=============================================================================
Localization* Localization::m_pInstance = nullptr;
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
Localization::Localization()
{
    nlsCoreDynamicLibrary = nullptr;
    bFirstDynamicLibraryCall = true;
    initCoreDynamicLibrary();
    initLanguageSupported();
}
//=============================================================================
Localization*
Localization::Instance()
{
    if (m_pInstance == NULL) {
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
Localization::initCoreDynamicLibrary(void)
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathCoreSharedLibrary = "libnlsCore" + get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf = new char[MAX_PATH];
        if (buf) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet) {
                fullpathCoreSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathCoreSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = std::getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathCoreSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathCoreSharedLibrary;
        }
#endif
        nlsCoreDynamicLibrary = load_dynamic_library(fullpathCoreSharedLibrary);
        if (nlsCoreDynamicLibrary) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
std::wstring
Localization::getPreferencesPathDynamic()
{
    typedef std::wstring (*PROC_GetPreferencesPath)();
    static PROC_GetPreferencesPath GetPreferencesPathPtr = nullptr;
    initCoreDynamicLibrary();
    if (!GetPreferencesPathPtr) {
        GetPreferencesPathPtr = reinterpret_cast<PROC_GetPreferencesPath>(
            get_function(nlsCoreDynamicLibrary, "GetNelsonPreferencesPath"));
        if (!GetPreferencesPathPtr) {
            Error(L"Core Function not loaded.");
        }
    }
    return GetPreferencesPathPtr();
}
//=============================================================================
std::wstring
Localization::getNelsonPathDynamic()
{
    typedef std::wstring (*PROC_GetNelsonPath)();
    static PROC_GetNelsonPath GetNelsonPathPtr = nullptr;
    initCoreDynamicLibrary();
    if (!GetNelsonPathPtr) {
        GetNelsonPathPtr = reinterpret_cast<PROC_GetNelsonPath>(
            get_function(nlsCoreDynamicLibrary, "GetNelsonPath"));
        if (!GetNelsonPathPtr) {
            Error(_W("Core Function not loaded."));
        }
    }
    return GetNelsonPathPtr();
}
//=============================================================================
void
Localization::setLanguageEnvironment(const std::wstring lang)
{
    if (isSupportedLanguage(lang)) {
        std::wstring localesPath = getNelsonPathDynamic() + L"/locale/";
        boost::locale::generator gen;
        try {
            gen.add_messages_path(wstring_to_utf8(localesPath).c_str());
            gen.add_messages_domain("nelson");
            std::string effectiveLang = wstring_to_utf8(lang);
            const std::string langDesired = effectiveLang + std::string(".UTF-8");
            std::locale::global(gen(langDesired));
        } catch (const std::exception&) {
        }
        setlocale(LC_NUMERIC, "C");
    }
}
//=============================================================================
void
Localization::initLanguageSupported(void)
{
    if (LanguageSupported.empty()) {
        std::wstring langsconf = getNelsonPathDynamic() + L"/etc/languages.conf";
        std::string jsonString = "";
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(langsconf);
#else
        std::ifstream jsonFile(wstring_to_utf8(langsconf));
#endif
        if (jsonFile.is_open()) {
            while (safegetline(jsonFile, tmpline)) {
                jsonString += tmpline + '\n';
            }
            jsonFile.close();
        }
        boost::property_tree::ptree pt2;
        std::istringstream is(jsonString);
        LanguageSupported.clear();
        try {
            boost::property_tree::read_json(is, pt2);
            BOOST_FOREACH (
                boost::property_tree::ptree::value_type& v, pt2.get_child("supported_languages")) {
                LanguageSupported.push_back(utf8_to_wstring(v.second.data()));
            }
        } catch (const boost::property_tree::json_parser::json_parser_error& je) {
            je.message();
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
Localization::setLanguage(std::wstring lang, bool save)
{
    if (isSupportedLanguage(lang)) {
        setLanguageEnvironment(lang);
        currentLanguage = lang;
        if (!save) {
            return true;
        }
        std::wstring prefDir = getPreferencesPathDynamic();
        std::wstring prefFile = prefDir + L"/nelson.conf";
        std::string jsonString = "";
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(prefFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(prefFile));
#endif
        if (jsonFile.is_open()) {
            while (safegetline(jsonFile, tmpline)) {
                jsonString += tmpline + '\n';
            }
            jsonFile.close();
            boost::property_tree::ptree pt;
            std::istringstream is(jsonString);
            try {
                boost::property_tree::read_json(is, pt);
                pt.put("language", wstring_to_utf8(lang));
                std::ostringstream buf;
                boost::property_tree::write_json(buf, pt, false);
                std::string json = buf.str();
#ifdef _MSC_VER
                std::ofstream out(prefFile);
#else
                std::ofstream out(wstring_to_utf8(prefFile));
#endif
                out << json;
                out.close();
                return true;
            } catch (const boost::property_tree::json_parser::json_parser_error& je) {
                je.message();
                return false;
            }
        } else {
            boost::property_tree::ptree pt;
            pt.put("language", wstring_to_utf8(lang));
            std::ostringstream buf;
            boost::property_tree::write_json(buf, pt, false);
            std::string json = buf.str();
#ifdef _MSC_VER
            std::ofstream out(prefFile);
#else
            std::ofstream out(wstring_to_utf8(prefFile));
#endif
            out << json;
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
    for (size_t k = 0; k < LanguageSupported.size(); k++) {
        langs.push_back(LanguageSupported[k]);
    }
    return true;
}
//=============================================================================
std::wstring
Localization::initializeLocalization(std::wstring lang)
{
    std::wstring effectiveLang = L"en_US";
    initLanguageSupported();
    if (lang.empty()) {
        std::wstring language_saved = L"";
        std::wstring prefDir = getPreferencesPathDynamic();
        std::wstring prefFile = prefDir + L"/nelson.conf";
        std::string jsonString = "";
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(prefFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(prefFile));
#endif
        if (jsonFile.is_open()) {
            while (safegetline(jsonFile, tmpline)) {
                jsonString += tmpline + '\n';
            }
            jsonFile.close();
            boost::property_tree::ptree pt;
            std::istringstream is(jsonString);
            try {
                boost::property_tree::read_json(is, pt);
                language_saved = utf8_to_wstring(pt.get<std::string>("language"));
            } catch (const boost::property_tree::json_parser::json_parser_error& je) {
                je.message();
                language_saved = L"";
            }
        } else {
            language_saved = L"";
        }
        lang = language_saved;
    }
    if (isSupportedLanguage(lang)) {
        effectiveLang = lang;
    } else {
        effectiveLang = L"en_US";
    }
    setLanguageEnvironment(effectiveLang);
    return effectiveLang;
}
//=============================================================================
bool
Localization::isSupportedLanguage(std::wstring lang)
{
    for (size_t k = 0; k < LanguageSupported.size(); k++) {
        if (lang == LanguageSupported[k]) {
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
