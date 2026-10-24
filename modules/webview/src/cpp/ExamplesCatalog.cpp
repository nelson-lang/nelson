//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExamplesCatalog.hpp"
#include <nlohmann/json.hpp>
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <set>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    namespace fs = std::filesystem;
    using json = nlohmann::json;
    //=============================================================================
    // The requirement vocabulary comes from etc/gallery_requirements.json, shared
    // with the test helpers and the WebAssembly packaging. When the file cannot
    // be read the set stays empty and requirement validation is skipped (the test
    // helper still enforces it in CI).
    std::set<std::string>
    loadRequirementVocabulary(const fs::path& path)
    {
        if (path.empty()) {
            return {};
        }
        try {
            std::ifstream stream(path);
            if (!stream.good()) {
                return {};
            }
            const json manifest = json::parse(stream);
            if (!manifest.is_object() || !manifest.contains("requirements")
                || !manifest.at("requirements").is_array()) {
                return {};
            }
            std::set<std::string> names;
            for (const auto& item : manifest.at("requirements")) {
                if (item.is_object() && item.contains("name") && item.at("name").is_string()) {
                    names.insert(item.at("name").get<std::string>());
                }
            }
            return names;
        } catch (...) {
            return {};
        }
    }
    //=============================================================================
    const std::set<std::string> kinds { "script", "command", "model" };
    const std::set<std::string> sources { "nelson", "external", "nflow" };
    //=============================================================================
    std::string
    toUtf8(const std::wstring& text)
    {
        return fs::path(text).u8string();
    }
    //=============================================================================
    std::wstring
    fromUtf8(const std::string& text)
    {
        return fs::u8path(text).wstring();
    }
    //=============================================================================
    std::string
    pathUtf8(const fs::path& path)
    {
        return toUtf8(path.wstring());
    }
    //=============================================================================
    bool
    isInside(const fs::path& path, const fs::path& root)
    {
        auto pathIt = path.begin();
        for (auto rootIt = root.begin(); rootIt != root.end(); ++rootIt, ++pathIt) {
            if (pathIt == path.end() || *pathIt != *rootIt) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    std::string
    localizedText(const json& value, const std::string& locale, const std::string& field)
    {
        if (value.is_string() && !value.get<std::string>().empty()) {
            return value.get<std::string>();
        }
        if (!value.is_object()) {
            throw std::runtime_error(field + " must be a non-empty string or localized object");
        }
        for (auto it = value.begin(); it != value.end(); ++it) {
            if ((it.key() != "en_US" && it.key() != "fr_FR") || !it.value().is_string()
                || it.value().get<std::string>().empty()) {
                throw std::runtime_error(
                    field + " translations must be non-empty en_US or fr_FR strings");
            }
        }
        for (const std::string& key : { locale, std::string("en_US"), std::string("fr_FR") }) {
            auto it = value.find(key);
            if (it != value.end() && it->is_string() && !it->get<std::string>().empty()) {
                return it->get<std::string>();
            }
        }
        throw std::runtime_error(field + " has no usable localization");
    }
    //=============================================================================
    std::string
    moduleTitle(const ExamplesModule& module, const std::string& locale)
    {
        if (!module.external) {
            return toUtf8(module.name);
        }
        std::ifstream stream(fs::path(module.path) / "module.json");
        if (!stream.good()) {
            return toUtf8(module.name);
        }
        try {
            const json metadata = json::parse(stream);
            if (metadata.contains("title")) {
                return localizedText(metadata.at("title"), locale, "title");
            }
        } catch (...) {
        }
        return toUtf8(module.name);
    }
    //=============================================================================
    std::vector<std::string>
    stringArray(const json& object, const std::string& field,
        const std::set<std::string>* requirementVocabulary)
    {
        if (!object.contains(field)) {
            return {};
        }
        const json& values = object.at(field);
        if (!values.is_array()) {
            throw std::runtime_error(field + " must be an array");
        }
        std::vector<std::string> result;
        for (const auto& value : values) {
            if (!value.is_string() || value.get<std::string>().empty()) {
                throw std::runtime_error(field + " must contain non-empty strings");
            }
            const std::string text = value.get<std::string>();
            if (requirementVocabulary != nullptr && !requirementVocabulary->empty()
                && requirementVocabulary->find(text) == requirementVocabulary->end()) {
                throw std::runtime_error("unknown requirement: " + text);
            }
            result.push_back(text);
        }
        return result;
    }
    //=============================================================================
    void
    fillFileEntry(const json& item, const fs::path& root, json& entry,
        std::vector<std::wstring>& allowedPaths)
    {
        if (!item.at("file").is_string()) {
            throw std::runtime_error("example file must be a relative .m path");
        }
        const fs::path relative = fs::path(fromUtf8(item.at("file").get<std::string>()));
        if (relative.empty() || relative.is_absolute() || relative.extension() != ".m") {
            throw std::runtime_error("example file must be a relative .m path");
        }
        if (std::find(relative.begin(), relative.end(), fs::path("..")) != relative.end()) {
            throw std::runtime_error("example file path must not contain ..");
        }
        const fs::path candidate = fs::weakly_canonical(root / relative);
        if (!isInside(candidate, root) || !fs::is_regular_file(candidate)) {
            throw std::runtime_error("example file is missing or outside the module");
        }
        entry["file"] = pathUtf8(candidate);
        entry["relativeFile"] = pathUtf8(relative.generic_wstring());
        entry["kind"] = "script";
        allowedPaths.push_back(candidate.wstring());
    }
    //=============================================================================
    void
    fillCommandEntry(const json& item, json& entry, std::vector<std::wstring>& allowedCommands)
    {
        if (!item.at("command").is_string() || item.at("command").get<std::string>().empty()) {
            throw std::runtime_error("example command must be a non-empty string");
        }
        const std::string commandText = item.at("command").get<std::string>();
        entry["command"] = commandText;
        entry["kind"] = "command";
        allowedCommands.push_back(fromUtf8(commandText));
    }
    //=============================================================================
    json
    parseManifest(const ExamplesModule& module, const fs::path& manifestPath,
        const std::string& locale, const std::set<std::string>& requirementVocabulary,
        const std::string& moduleName, json& diagnostics, std::vector<std::wstring>& allowedPaths,
        std::vector<std::wstring>& allowedCommands)
    {
        std::ifstream stream(manifestPath);
        if (!stream.good()) {
            throw std::runtime_error("cannot read manifest");
        }
        const json manifest = json::parse(stream);
        if (!manifest.is_object() || !manifest.contains("schemaVersion")
            || !manifest.at("schemaVersion").is_number_integer()
            || manifest.at("schemaVersion").get<int>() != 1 || !manifest.contains("examples")
            || !manifest.at("examples").is_array()) {
            throw std::runtime_error("schemaVersion 1 and an examples array are required");
        }
        const fs::path root = fs::weakly_canonical(fs::path(module.path) / "examples");
        std::set<fs::path> uniquePaths;
        json entries = json::array();
        int index = 0;
        // A malformed example is reported and skipped, not the whole module. The
        // allowlists and the duplicate set are updated only after full validation.
        for (const auto& item : manifest.at("examples")) {
            ++index;
            try {
                if (!item.is_object() || !item.contains("title") || !item.contains("description")) {
                    throw std::runtime_error("each example requires title and description");
                }
                const bool hasFile = item.contains("file");
                const bool hasCommand = item.contains("command");
                if (hasFile == hasCommand) {
                    throw std::runtime_error(
                        "each example requires exactly one of file or command");
                }
                json entry;
                std::vector<std::wstring> entryPaths;
                std::vector<std::wstring> entryCommands;
                if (hasCommand) {
                    fillCommandEntry(item, entry, entryCommands);
                } else {
                    fillFileEntry(item, root, entry, entryPaths);
                }
                entry["title"] = localizedText(item.at("title"), locale, "title");
                entry["description"] = localizedText(item.at("description"), locale, "description");
                entry["tags"] = stringArray(item, "tags", nullptr);
                // In-tree modules are held to the shared vocabulary (CI enforces it
                // through validateExampleModule.m). An external toolbox is maintained
                // outside this repository, so an unknown token is kept as a label
                // instead of dropping the toolbox from the gallery.
                entry["requirements"] = stringArray(
                    item, "requirements", module.external ? nullptr : &requirementVocabulary);
                if (!entryPaths.empty()
                    && !uniquePaths.insert(fs::path(entryPaths.front())).second) {
                    throw std::runtime_error("duplicate example file");
                }
                entries.push_back(std::move(entry));
                allowedPaths.insert(allowedPaths.end(), entryPaths.begin(), entryPaths.end());
                allowedCommands.insert(
                    allowedCommands.end(), entryCommands.begin(), entryCommands.end());
            } catch (const std::exception& exception) {
                diagnostics.push_back({ { "module", moduleName },
                    { "message", "example #" + std::to_string(index) + ": " + exception.what() } });
            }
        }
        return entries;
    }
    //=============================================================================
    // A provider entry is already in the catalog schema; this checks it and
    // resolves its file so the gallery can only launch cataloged documents.
    json
    providerExample(const json& item, const std::set<std::string>& requirementVocabulary,
        std::vector<std::wstring>& allowedPaths, std::vector<std::wstring>& allowedCommands)
    {
        if (!item.is_object() || !item.contains("title") || !item.at("title").is_string()
            || item.at("title").get<std::string>().empty()) {
            throw std::runtime_error("each provider example requires a title");
        }
        const std::string kind = item.value("kind", std::string("script"));
        if (kinds.find(kind) == kinds.end()) {
            throw std::runtime_error("unknown example kind: " + kind);
        }
        json entry = item;
        entry["kind"] = kind;
        entry["description"] = item.value("description", std::string());
        entry["tags"] = stringArray(item, "tags", nullptr);
        entry["requirements"] = stringArray(item, "requirements", &requirementVocabulary);
        if (kind == "command") {
            if (!item.contains("command") || !item.at("command").is_string()
                || item.at("command").get<std::string>().empty()) {
                throw std::runtime_error("command example requires a command");
            }
            entry.erase("file");
            allowedCommands.push_back(fromUtf8(item.at("command").get<std::string>()));
            return entry;
        }
        if (!item.contains("file") || !item.at("file").is_string()) {
            throw std::runtime_error("example requires a file");
        }
        const fs::path file = fromUtf8(item.at("file").get<std::string>());
        std::error_code error;
        const fs::path canonical = fs::weakly_canonical(file, error);
        if (error || !fs::is_regular_file(canonical)) {
            throw std::runtime_error("example file is unavailable");
        }
        entry.erase("command");
        entry["file"] = pathUtf8(canonical);
        allowedPaths.push_back(canonical.wstring());
        return entry;
    }
    //=============================================================================
    void
    appendProviderGroups(const std::string& groupsJson,
        const std::set<std::string>& requirementVocabulary, json& groups, json& diagnostics,
        ExamplesCatalog& result)
    {
        json parsed;
        try {
            parsed = json::parse(groupsJson);
        } catch (const std::exception& exception) {
            diagnostics.push_back({ { "module", "provider" }, { "message", exception.what() } });
            return;
        }
        if (!parsed.is_array()) {
            diagnostics.push_back(
                { { "module", "provider" }, { "message", "provider groups must be an array" } });
            return;
        }
        for (const auto& group : parsed) {
            const std::string name = group.is_object() ? group.value("name", "") : "";
            try {
                if (name.empty() || !group.contains("title") || !group.at("title").is_string()
                    || !group.contains("examples") || !group.at("examples").is_array()) {
                    throw std::runtime_error("each group requires name, title and examples");
                }
                const std::string source = group.value("source", std::string("external"));
                if (sources.find(source) == sources.end()) {
                    throw std::runtime_error("unknown group source: " + source);
                }
                std::vector<std::wstring> groupPaths;
                std::vector<std::wstring> groupCommands;
                json examples = json::array();
                for (const auto& item : group.at("examples")) {
                    examples.push_back(
                        providerExample(item, requirementVocabulary, groupPaths, groupCommands));
                }
                groups.push_back({ { "name", name }, { "title", group.at("title") },
                    { "source", source }, { "examples", std::move(examples) } });
                result.allowedPaths.insert(
                    result.allowedPaths.end(), groupPaths.begin(), groupPaths.end());
                result.allowedCommands.insert(
                    result.allowedCommands.end(), groupCommands.begin(), groupCommands.end());
            } catch (const std::exception& exception) {
                diagnostics.push_back({ { "module", name.empty() ? "provider" : name },
                    { "message", exception.what() } });
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
ExamplesCatalog
BuildExamplesCatalog(const std::vector<ExamplesModule>& modules, const std::string& locale,
    const std::vector<std::string>& providerGroupsJson,
    const std::wstring& requirementVocabularyPath)
{
    json groups = json::array();
    json diagnostics = json::array();
    ExamplesCatalog result;
    const std::set<std::string> requirementVocabulary
        = loadRequirementVocabulary(fs::path(requirementVocabularyPath));
    for (const auto& module : modules) {
        const std::string name = toUtf8(module.name);
        if (name.rfind("nflow_", 0) == 0) {
            continue;
        }
        const fs::path manifestPath = fs::path(module.path) / "examples" / "index.json";
        std::error_code error;
        if (!fs::exists(manifestPath, error)) {
            continue;
        }
        try {
            std::vector<std::wstring> moduleAllowedPaths;
            std::vector<std::wstring> moduleAllowedCommands;
            json examples = parseManifest(module, manifestPath, locale, requirementVocabulary, name,
                diagnostics, moduleAllowedPaths, moduleAllowedCommands);
            // A module whose every example was rejected leaves no group, like a
            // module without a manifest: only its per-example diagnostics remain.
            if (!examples.empty()) {
                groups.push_back({ { "name", name }, { "title", moduleTitle(module, locale) },
                    { "source", module.external ? "external" : "nelson" },
                    { "examples", std::move(examples) } });
                result.allowedPaths.insert(result.allowedPaths.end(), moduleAllowedPaths.begin(),
                    moduleAllowedPaths.end());
                result.allowedCommands.insert(result.allowedCommands.end(),
                    moduleAllowedCommands.begin(), moduleAllowedCommands.end());
            }
        } catch (const std::exception& exception) {
            diagnostics.push_back({ { "module", name }, { "message", exception.what() } });
        }
    }
    for (const std::string& groupsJson : providerGroupsJson) {
        appendProviderGroups(groupsJson, requirementVocabulary, groups, diagnostics, result);
    }
    result.json = json { { "groups", groups }, { "diagnostics", diagnostics } }.dump();
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
