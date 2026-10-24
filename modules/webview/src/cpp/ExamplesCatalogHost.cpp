//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExamplesCatalogHost.hpp"
#include "DynamicLibrary.hpp"
#include "ModulesManager.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include <nlohmann/json.hpp>
#include <map>
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using json = nlohmann::json;
    using ProviderSink = void (*)(void* context, const char* groupsJson);
    using ProviderFunction = int (*)(const char* locale, ProviderSink sink, void* context);
    //=============================================================================
    struct ExamplesProvider
    {
        std::string symbol;
        library_handle handle = nullptr;
    };
    //=============================================================================
    struct HostState
    {
        std::map<std::wstring, ExamplesProvider> providers;
    };
    //=============================================================================
    std::mutex&
    stateMutex()
    {
        static auto* mutex = new std::mutex;
        return *mutex;
    }
    //=============================================================================
    HostState&
    hostState()
    {
        static auto* value = new HostState;
        return *value;
    }
    //=============================================================================
    void
    collectGroups(void* context, const char* groupsJson)
    {
        if (context != nullptr && groupsJson != nullptr) {
            static_cast<std::string*>(context)->assign(groupsJson);
        }
    }
    //=============================================================================
    bool
    callProvider(const std::wstring& libraryPath, ExamplesProvider& provider,
        const std::string& locale, std::string& groupsJson, std::string& error)
    {
        if (provider.handle == nullptr) {
            provider.handle = load_dynamic_libraryW(libraryPath);
        }
        if (provider.handle == nullptr) {
            error = "cannot load examples provider library";
            return false;
        }
        auto function
            = reinterpret_cast<ProviderFunction>(get_function(provider.handle, provider.symbol));
        if (function == nullptr) {
            error = "examples provider symbol not found: " + provider.symbol;
            return false;
        }
        if (function(locale.c_str(), collectGroups, &groupsJson) == 0) {
            error = "examples provider failed";
            return false;
        }
        return true;
    }
    //=============================================================================
    void
    appendDiagnostic(
        ExamplesCatalog& catalog, const std::string& module, const std::string& message)
    {
        json parsed = json::parse(catalog.json);
        parsed["diagnostics"].push_back({ { "module", module }, { "message", message } });
        catalog.json = parsed.dump();
    }
    //=============================================================================
} // namespace
//=============================================================================
void
RegisterExamplesProvider(const std::wstring& libraryPath, const std::string& symbol)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    ExamplesProvider& provider = hostState().providers[libraryPath];
    if (provider.symbol != symbol && provider.handle != nullptr) {
        close_dynamic_library(provider.handle);
        provider.handle = nullptr;
    }
    provider.symbol = symbol;
}
//=============================================================================
bool
UnregisterExamplesProvider(const std::wstring& libraryPath)
{
    std::lock_guard<std::mutex> lock(stateMutex());
    auto& providers = hostState().providers;
    auto found = providers.find(libraryPath);
    if (found == providers.end()) {
        return false;
    }
    if (found->second.handle != nullptr) {
        close_dynamic_library(found->second.handle);
    }
    providers.erase(found);
    return true;
}
//=============================================================================
std::string
CurrentExamplesLocale()
{
    const std::string locale
        = wstring_to_utf8(NelsonConfiguration::getInstance()->getCurrentLocale());
    return locale.rfind("fr", 0) == 0 ? "fr_FR" : "en_US";
}
//=============================================================================
ExamplesCatalog
BuildLoadedModulesExamplesCatalog()
{
    std::vector<ExamplesModule> modules;
    for (const module& loaded : GetModules()) {
        modules.push_back({ loaded.modulename, loaded.modulepath, !loaded.isprotected });
    }
    const std::string locale = CurrentExamplesLocale();
    std::vector<std::string> providerGroups;
    std::vector<std::pair<std::string, std::string>> failures;
    {
        std::lock_guard<std::mutex> lock(stateMutex());
        for (auto& [libraryPath, provider] : hostState().providers) {
            std::string groupsJson;
            std::string error;
            if (callProvider(libraryPath, provider, locale, groupsJson, error)) {
                providerGroups.push_back(groupsJson);
            } else {
                failures.emplace_back(wstring_to_utf8(libraryPath), error);
            }
        }
    }
    const std::wstring requirementVocabularyPath
        = NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/etc/gallery_requirements.json";
    ExamplesCatalog catalog
        = BuildExamplesCatalog(modules, locale, providerGroups, requirementVocabularyPath);
    for (const auto& [library, error] : failures) {
        appendDiagnostic(catalog, library, error);
    }
    return catalog;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
