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
#include <map>
#include <set>
#include <string>
#include <nlohmann/json.hpp>
#include "i18nHelpersBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18nExtract.hpp"
#include "i18nJsonMerger.hpp"
#include "i18nJsonSort.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
loadJsonObject(const std::wstring& path, nlohmann::json& j)
{
#ifdef _MSC_VER
    std::ifstream f(path);
#else
    std::ifstream f(wstring_to_utf8(path));
#endif
    if (!f.is_open()) {
        j = nlohmann::json::object();
        return false;
    }
    try {
        j = nlohmann::json::parse(f);
    } catch (const nlohmann::json::exception&) {
        j = nlohmann::json::object();
        return false;
    }
    if (!j.is_object()) {
        j = nlohmann::json::object();
    }
    return true;
}
//=============================================================================
// Builds the identifier-keyed French error catalog. An already translated value
// is kept, otherwise the legacy text-keyed French translation of the same English
// template is reused. A changed template stays empty and falls back to English.
static bool
partitionErrorsFr(const std::wstring& enFile, const std::wstring& frFile,
    const std::wstring& legacyFrFile, std::wstring& errorMessage)
{
    nlohmann::json en;
    nlohmann::json legacyFr;
    nlohmann::json existingFr;
    loadJsonObject(enFile, en);
    loadJsonObject(legacyFrFile, legacyFr);
    loadJsonObject(frFile, existingFr);

    nlohmann::ordered_json out;
    for (auto it = en.begin(); it != en.end(); ++it) {
        const std::string& id = it.key();
        std::string frValue;
        if (existingFr.contains(id) && existingFr[id].is_string()
            && !existingFr[id].get<std::string>().empty()) {
            frValue = existingFr[id].get<std::string>();
        } else if (it.value().is_string()) {
            const std::string enTemplate = it.value().get<std::string>();
            if (legacyFr.contains(enTemplate) && legacyFr[enTemplate].is_string()) {
                frValue = legacyFr[enTemplate].get<std::string>();
            }
        }
        out[id] = frValue;
    }

#ifdef _MSC_VER
    std::wofstream outFile(frFile);
#else
    std::ofstream outFile(wstring_to_utf8(frFile));
#endif
    if (!outFile) {
        errorMessage = resolveMessageTemplate(
                           L"Nelson:i18n:cannotOpenOutputFile", L"Cannot open output file: ")
            + frFile;
        return false;
    }
#ifdef _MSC_VER
    outFile << utf8_to_wstring(out.dump(2)) << L"\n";
#else
    outFile << out.dump(2) << "\n";
#endif
    outFile.close();
    return true;
}
//=============================================================================
// Module segment of an identifier "Nelson:module:mnemonic"; empty when it has
// fewer than three segments.
static std::string
moduleOfIdentifier(const std::string& id)
{
    std::string::size_type first = id.find(':');
    if (first == std::string::npos) {
        return std::string();
    }
    std::string::size_type second = id.find(':', first + 1);
    if (second == std::string::npos) {
        return std::string();
    }
    return id.substr(first + 1, second - first - 1);
}
//=============================================================================
// Audit helper: lists the groups of identifiers that share the same English
// template within one module. Most are intentional, but a real duplicate hides in
// one. Manual command, not run during regeneration. Returns the group count.
static size_t
reportDuplicateTemplates(
    const std::wstring& errorsJsonFile, const std::wstring& reportFile, std::wstring& errorMessage)
{
    nlohmann::json errorsJson;
    loadJsonObject(errorsJsonFile, errorsJson);

    // template -> module -> sorted identifiers
    std::map<std::string, std::map<std::string, std::set<std::string>>> byTemplateModule;
    for (auto it = errorsJson.begin(); it != errorsJson.end(); ++it) {
        if (!it.value().is_string()) {
            continue;
        }
        const std::string tmpl = it.value().get<std::string>();
        if (tmpl.empty()) {
            continue;
        }
        const std::string module = moduleOfIdentifier(it.key());
        if (module.empty()) {
            continue;
        }
        byTemplateModule[tmpl][module].insert(it.key());
    }

    // Keep groups with more than one identifier, ordered by module then template.
    std::map<std::string, std::map<std::string, std::set<std::string>>> groupsByModule;
    size_t groupCount = 0;
    for (const auto& tmplEntry : byTemplateModule) {
        for (const auto& moduleEntry : tmplEntry.second) {
            if (moduleEntry.second.size() >= 2) {
                groupsByModule[moduleEntry.first][tmplEntry.first] = moduleEntry.second;
                ++groupCount;
            }
        }
    }

#ifdef _MSC_VER
    std::wofstream outFile(reportFile);
#else
    std::ofstream outFile(wstring_to_utf8(reportFile));
#endif
    if (!outFile) {
        errorMessage = resolveMessageTemplate(
                           L"Nelson:i18n:cannotOpenOutputFile", L"Cannot open output file: ")
            + reportFile;
        return 0;
    }
    std::string header = "Same-module error identifiers sharing one English template: "
        + std::to_string(groupCount) + " group(s).\n\n";
    for (const auto& moduleEntry : groupsByModule) {
        for (const auto& tmplEntry : moduleEntry.second) {
            header += "MODULE " + moduleEntry.first + " | TEMPLATE " + tmplEntry.first + "\n";
            for (const std::string& id : tmplEntry.second) {
                header += "    " + id + "\n";
            }
        }
    }
#ifdef _MSC_VER
    outFile << utf8_to_wstring(header);
#else
    outFile << header;
#endif
    outFile.close();
    return groupCount;
}
//=============================================================================
ArrayOfVector
Nelson::I18nGateway::i18nHelpersBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 2, 4);
    std::wstring helpersName = argIn[0].asWideString();
    if (helpersName == L"template") {
        nargincheck(argIn, 2, 2);
        std::wstring id = argIn[1].asWideString();
        retval << ArrayOf::characterArray(getMessageTemplateById(id));
        return retval;
    } else if (helpersName == L"partition") {
        nargincheck(argIn, 4, 4);
        std::wstring enFile = argIn[1].asWideString();
        std::wstring frFile = argIn[2].asWideString();
        std::wstring legacyFrFile = argIn[3].asWideString();
        std::wstring errorMessage;
        if (!partitionErrorsFr(enFile, frFile, legacyFrFile, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
    } else if (helpersName == L"extractUI") {
        nargincheck(argIn, 3, 3);
        std::wstring sourceRoot = argIn[1].asWideString();
        std::wstring jsonFile = argIn[2].asWideString();
        std::wstring errorMessage;
        if (!i18nExtractUi(sourceRoot, jsonFile, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
    } else if (helpersName == L"extractErrors") {
        nargincheck(argIn, 3, 3);
        std::wstring sourceRoot = argIn[1].asWideString();
        std::wstring jsonFile = argIn[2].asWideString();
        std::wstring errorMessage;
        if (!i18nExtractErrors(sourceRoot, jsonFile, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
    } else if (helpersName == L"merge") {
        std::wstring jsonSource = argIn[1].asWideString();
        std::wstring jsonDestination = argIn[2].asWideString();
        std::wstring errorMessage;
        if (!i18nJsonMerger(jsonSource, jsonDestination, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
    } else if (helpersName == L"sort") {
        std::wstring jsonSource = argIn[1].asWideString();
        std::wstring jsonDestination = argIn[2].asWideString();
        std::wstring errorMessage;
        if (!i18nJsonSort(jsonSource, jsonDestination, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
    } else if (helpersName == L"noindex") {
        nargincheck(argIn, 3, 3);
        std::wstring errorsJsonFile = argIn[1].asWideString();
        std::wstring sourceRoot = argIn[2].asWideString();
        // Identifiers raised with a run-time (non-literal) message must not carry a fixed
        // template: the cascade would resolve it and override the run-time message.
        std::set<std::string> runtimeIds = collectRuntimeMessageIds(sourceRoot);
        nlohmann::ordered_json errorsJson;
#ifdef _MSC_VER
        std::ifstream inFile(errorsJsonFile);
#else
        std::ifstream inFile(wstring_to_utf8(errorsJsonFile));
#endif
        if (inFile.is_open()) {
            try {
                errorsJson = nlohmann::ordered_json::parse(inFile);
            } catch (const nlohmann::json::exception&) {
                errorsJson = nlohmann::ordered_json::object();
            }
        }
        for (const std::string& id : runtimeIds) {
            errorsJson.erase(id);
        }
#ifdef _MSC_VER
        std::wofstream outFile(errorsJsonFile);
        outFile << utf8_to_wstring(errorsJson.dump(2)) << L"\n";
#else
        std::ofstream outFile(wstring_to_utf8(errorsJsonFile));
        outFile << errorsJson.dump(2) << "\n";
#endif
    } else if (helpersName == L"dedup") {
        nargincheck(argIn, 4, 4);
        std::wstring uiJsonFile = argIn[1].asWideString();
        std::wstring errorsJsonFile = argIn[2].asWideString();
        std::wstring sourceRoot = argIn[3].asWideString();
        // A UI (text-keyed) entry is redundant when its text is already an identifier-keyed
        // error template: such messages resolve through the identifier cascade
        // (getMessageTemplateById), so the UI copy is never consulted. The French is already
        // in the error catalog, so dropping it is lossless. A UI entry also used as a
        // genuine (non-error) label is kept.
        nlohmann::json errorsJson;
        loadJsonObject(errorsJsonFile, errorsJson);
        std::set<std::string> errorTemplates;
        for (auto it = errorsJson.begin(); it != errorsJson.end(); ++it) {
            if (it.value().is_string()) {
                std::string v = it.value().get<std::string>();
                if (!v.empty()) {
                    errorTemplates.insert(v);
                }
            }
        }
        // Genuine UI text: literals that are not themselves an error/warning argument.
        std::set<std::string> errorMessages = collectErrorWarningMessages(sourceRoot);
        std::set<std::string> textKeyed = collectTextKeyedLiterals(sourceRoot);
        // Only single-template identifiers are dropped. Templates of overloaded
        // identifiers are not in errorTemplates and stay in the UI catalog, which still
        // translates them. An error template wrapped in _W(...) inside Error() does not
        // count as a text-keyed use.
        std::set<std::string> genuineTextKeyed;
        for (const std::string& t : textKeyed) {
            if (errorMessages.count(t) == 0) {
                genuineTextKeyed.insert(t);
            }
        }
        textKeyed.swap(genuineTextKeyed);
        nlohmann::ordered_json uiJson;
#ifdef _MSC_VER
        std::ifstream inFile(uiJsonFile);
#else
        std::ifstream inFile(wstring_to_utf8(uiJsonFile));
#endif
        if (inFile.is_open()) {
            try {
                uiJson = nlohmann::ordered_json::parse(inFile);
            } catch (const nlohmann::json::exception&) {
                uiJson = nlohmann::ordered_json::object();
            }
        }
        nlohmann::ordered_json kept;
        for (auto it = uiJson.begin(); it != uiJson.end(); ++it) {
            const std::string& key = it.key();
            if (errorTemplates.count(key) != 0 && textKeyed.count(key) == 0) {
                continue; // redundant: resolved by the identifier cascade, not text-keyed
            }
            kept[key] = it.value();
        }
#ifdef _MSC_VER
        std::wofstream outFile(uiJsonFile);
        outFile << utf8_to_wstring(kept.dump(2)) << L"\n";
#else
        std::ofstream outFile(wstring_to_utf8(uiJsonFile));
        outFile << kept.dump(2) << "\n";
#endif
    } else if (helpersName == L"reportDuplicates") {
        nargincheck(argIn, 3, 3);
        std::wstring errorsJsonFile = argIn[1].asWideString();
        std::wstring reportFile = argIn[2].asWideString();
        std::wstring errorMessage;
        size_t groupCount = reportDuplicateTemplates(errorsJsonFile, reportFile, errorMessage);
        if (!errorMessage.empty()) {
            RaiseError(errorMessage, L"Nelson:i18n:operationFailed");
        }
        retval << ArrayOf::doubleScalar(static_cast<double>(groupCount));
    } else if (helpersName == L"register") {
        // Runtime registration of an external module's locale catalog, called from the
        // module's etc/startup.m:
        //   i18nHelpers('register', '<moduleName>', modulepath('<moduleName>'))
        // With one or two outputs it returns [status, message] instead of raising.
        nargincheck(argIn, 3, 3);
        std::wstring moduleName = argIn[1].asWideString();
        std::wstring moduleRoot = argIn[2].asWideString();
        registerModuleCatalog(moduleName, moduleRoot);
        if (nLhs >= 1) {
            retval << ArrayOf::logicalScalar(true);
            if (nLhs >= 2) {
                retval << ArrayOf::characterArray(std::string());
            }
        }
    } else if (helpersName == L"unregister") {
        // Drops a module's catalog, called from etc/finish.m:
        //   i18nHelpers('unregister', '<moduleName>')
        // With one or two outputs it returns [status, message] instead of raising.
        nargincheck(argIn, 2, 2);
        std::wstring moduleName = argIn[1].asWideString();
        unregisterModuleCatalog(moduleName);
        if (nLhs >= 1) {
            retval << ArrayOf::logicalScalar(true);
            if (nLhs >= 2) {
                retval << ArrayOf::characterArray(std::string());
            }
        }
    } else if (helpersName == L"namespace") {
        // Keeps only the keys of a JSON catalog that start with a prefix (in place), to
        // scope a module errors catalog to its own identifiers:
        //   i18nHelpers('namespace', jsonFile, 'Nelson:cserial:')
        nargincheck(argIn, 3, 3);
        std::wstring jsonFile = argIn[1].asWideString();
        std::string prefix = argIn[2].asUtf8String();
        std::ifstream in(wstring_to_utf8(jsonFile));
        if (!in.is_open()) {
            RaiseError(L"Unable to open JSON file.", L"Nelson:i18n:operationFailed");
        }
        nlohmann::ordered_json data;
        try {
            in >> data;
        } catch (const nlohmann::json::exception&) {
            RaiseError(L"Invalid JSON file.", L"Nelson:i18n:operationFailed");
        }
        in.close();
        nlohmann::ordered_json filtered = nlohmann::ordered_json::object();
        for (auto it = data.begin(); it != data.end(); ++it) {
            if (it.key().rfind(prefix, 0) == 0) {
                filtered[it.key()] = it.value();
            }
        }
        std::ofstream out(wstring_to_utf8(jsonFile));
        if (!out.is_open()) {
            RaiseError(L"Unable to write JSON file.", L"Nelson:i18n:operationFailed");
        }
        out << filtered.dump(2) << "\n";
        out.close();
    } else {
        Error("Nelson:i18n:unsupportedCase", "case not supported.");
    }
    return retval;
}
//=============================================================================
