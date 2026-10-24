//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerConfig.hpp"
#include "CodeAnalyzerRules.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <set>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    //=============================================================================
    bool
    readTextFile(const fs::path& path, std::string& content)
    {
        std::ifstream stream(path, std::ios::binary);
        if (!stream.good()) {
            return false;
        }
        std::ostringstream buffer;
        buffer << stream.rdbuf();
        content = buffer.str();
        return true;
    }
    //=============================================================================
    bool
    isValidSeverity(const std::string& severity)
    {
        return severity == "allow" || severity == "info" || severity == "warning"
            || severity == "error";
    }
    //=============================================================================
    bool
    isValidFailOn(const std::string& severity)
    {
        return severity == "none" || severity == "info" || severity == "warning"
            || severity == "error";
    }
    //=============================================================================
    void
    mergeConfigJson(nlohmann::json& base, const nlohmann::json& child)
    {
        for (auto it = child.begin(); it != child.end(); ++it) {
            if (it.key() == "extends") {
                continue;
            }
            if (base.contains(it.key()) && base[it.key()].is_object() && it.value().is_object()) {
                mergeConfigJson(base[it.key()], it.value());
            } else if (base.contains(it.key()) && base[it.key()].is_array()
                && it.value().is_array()) {
                for (const auto& item : it.value()) {
                    base[it.key()].push_back(item);
                }
            } else {
                base[it.key()] = it.value();
            }
        }
    }
    //=============================================================================
    bool
    loadConfigJson(const fs::path& configPath, std::set<fs::path>& loading, nlohmann::json& merged,
        std::string& error)
    {
        fs::path absolutePath = fs::absolute(configPath).lexically_normal();
        if (loading.find(absolutePath) != loading.end()) {
            error = "Code analyzer configuration has an extends cycle.";
            return false;
        }
        std::string content;
        if (!readTextFile(absolutePath, content)) {
            error = "Code analyzer configuration cannot be read.";
            return false;
        }
        nlohmann::json current;
        try {
            current = nlohmann::json::parse(content);
        } catch (const std::exception& e) {
            error = std::string("Code analyzer configuration is not valid JSON: ") + e.what();
            return false;
        }
        if (!current.is_object()) {
            error = "Code analyzer configuration root must be an object.";
            return false;
        }
        static const std::set<std::string> allowedKeys
            = { "version", "extends", "files", "rules", "ci" };
        for (auto it = current.begin(); it != current.end(); ++it) {
            if (allowedKeys.find(it.key()) == allowedKeys.end()) {
                error = "Unsupported code analyzer configuration key: " + it.key();
                return false;
            }
        }
        loading.insert(absolutePath);
        nlohmann::json result = nlohmann::json::object();
        if (current.contains("extends")) {
            if (!current["extends"].is_array()) {
                error = "Code analyzer configuration 'extends' must be an array.";
                return false;
            }
            for (const auto& item : current["extends"]) {
                if (!item.is_string()) {
                    error = "Code analyzer configuration 'extends' entries must be strings.";
                    return false;
                }
                nlohmann::json parent = nlohmann::json::object();
                fs::path parentPath = absolutePath.parent_path() / item.get<std::string>();
                if (!loadConfigJson(parentPath, loading, parent, error)) {
                    return false;
                }
                mergeConfigJson(result, parent);
            }
        }
        mergeConfigJson(result, current);
        loading.erase(absolutePath);
        merged = result;
        return true;
    }
    //=============================================================================
    bool
    readStringArray(const nlohmann::json& value, wstringVector& destination, std::string& error,
        const std::string& key)
    {
        if (!value.is_array()) {
            error = "Code analyzer configuration '" + key + "' must be an array.";
            return false;
        }
        for (const auto& item : value) {
            if (!item.is_string()) {
                error = "Code analyzer configuration '" + key + "' entries must be strings.";
                return false;
            }
            destination.push_back(utf8_to_wstring(item.get<std::string>()));
        }
        return true;
    }
    //=============================================================================
    bool
    readFilesConfig(const nlohmann::json& config, CodeAnalyzerOptions& options)
    {
        if (!config.contains("files")) {
            return true;
        }
        const auto& files = config["files"];
        if (!files.is_object()) {
            options.configError = "Code analyzer configuration 'files' must be an object.";
            return false;
        }
        if (files.contains("exclude")
            && !readStringArray(
                files["exclude"], options.exclusions, options.configError, "files.exclude")) {
            return false;
        }
        if (files.contains("include")
            && !readStringArray(
                files["include"], options.inclusions, options.configError, "files.include")) {
            return false;
        }
        return true;
    }
    //=============================================================================
    bool
    readRuleOptions(
        const std::string& id, const nlohmann::json& value, CodeAnalyzerOptions& options)
    {
        if (!value.contains("options")) {
            return true;
        }
        const auto& ruleOptions = value["options"];
        if (!ruleOptions.is_object()) {
            options.configError = "Code analyzer rule options must be an object.";
            return false;
        }
        if (id == "NLS0012" && ruleOptions.contains("maxLineLength")) {
            options.maxLineLength = ruleOptions["maxLineLength"].get<int>();
        } else if (id == "NLS0020" && ruleOptions.contains("maxFileLines")) {
            options.maxFileLines = ruleOptions["maxFileLines"].get<int>();
        } else if (id == "NLS0021" && ruleOptions.contains("maxFunctionLines")) {
            options.maxFunctionLines = ruleOptions["maxFunctionLines"].get<int>();
        } else if ((id == "NLS0014" || id == "NLS0015" || id == "NLS0026")
            && (ruleOptions.contains("warning") || ruleOptions.contains("error"))) {
            if (ruleOptions.contains("warning")) {
                options.cyclomaticWarningThreshold = ruleOptions["warning"].get<int>();
            }
            if (ruleOptions.contains("error")) {
                options.cyclomaticErrorThreshold = ruleOptions["error"].get<int>();
            }
        }
        return true;
    }
    //=============================================================================
    bool
    readRulesConfig(const nlohmann::json& config, CodeAnalyzerOptions& options)
    {
        if (!config.contains("rules")) {
            return true;
        }
        if (!config["rules"].is_object()) {
            options.configError = "Code analyzer configuration 'rules' must be an object.";
            return false;
        }
        for (auto it = config["rules"].begin(); it != config["rules"].end(); ++it) {
            std::string id = it.key();
            std::transform(id.begin(), id.end(), id.begin(),
                [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
            if (codeAnalyzerRuleInfo(id) == nullptr) {
                options.configError = "Unknown code analyzer rule: " + id;
                return false;
            }
            if (!it.value().is_object()) {
                options.configError = "Code analyzer rule configuration must be an object.";
                return false;
            }
            if (it.value().contains("level")) {
                if (!it.value()["level"].is_string()) {
                    options.configError = "Code analyzer rule level must be a string.";
                    return false;
                }
                std::string level = it.value()["level"].get<std::string>();
                if (!isValidSeverity(level)) {
                    options.configError = "Code analyzer rule level is invalid.";
                    return false;
                }
                options.ruleSeverity[id] = level;
            }
            if (!readRuleOptions(id, it.value(), options)) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    readCiConfig(const nlohmann::json& config, CodeAnalyzerOptions& options)
    {
        if (!config.contains("ci")) {
            return true;
        }
        if (!config["ci"].is_object()) {
            options.configError = "Code analyzer configuration 'ci' must be an object.";
            return false;
        }
        if (!config["ci"].contains("failOn")) {
            return true;
        }
        if (!config["ci"]["failOn"].is_string()) {
            options.configError = "Code analyzer configuration 'ci.failOn' must be a string.";
            return false;
        }
        std::string failOn = config["ci"]["failOn"].get<std::string>();
        if (!isValidFailOn(failOn)) {
            options.configError = "Code analyzer configuration 'ci.failOn' is invalid.";
            return false;
        }
        options.failOn = failOn;
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
readCodeAnalyzerConfig(CodeAnalyzerOptions& options)
{
    if (options.configFile.empty()) {
        return;
    }
    if (!options.configError.empty()) {
        return;
    }
    nlohmann::json config;
    std::set<fs::path> loading;
    if (!loadConfigJson(fs::path(options.configFile), loading, config, options.configError)) {
        return;
    }
    try {
        if (!config.contains("version") || !config["version"].is_number_integer()
            || config["version"].get<int>() != 2) {
            options.configError = "Code analyzer configuration must declare version 2.";
            return;
        }
        readFilesConfig(config, options) && readRulesConfig(config, options)
            && readCiConfig(config, options);
    } catch (const std::exception& e) {
        options.configError
            = std::string("Code analyzer configuration contains an invalid value: ") + e.what();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
