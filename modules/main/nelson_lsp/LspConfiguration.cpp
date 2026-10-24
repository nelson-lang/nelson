//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LspConfiguration.hpp"
#include "CodeAnalyzerRules.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace Lsp {
    //=============================================================================
    namespace {
        //=============================================================================
        bool
        isValidSeverity(const std::string& severity)
        {
            return severity == "allow" || severity == "info" || severity == "warning"
                || severity == "error";
        }
        //=============================================================================
        std::string
        upperAscii(std::string value)
        {
            std::transform(value.begin(), value.end(), value.begin(),
                [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
            return value;
        }
        //=============================================================================
        void
        readStringArray(const nlohmann::json& value, wstringVector& destination)
        {
            if (!value.is_array()) {
                return;
            }
            destination.clear();
            for (const auto& item : value) {
                if (item.is_string()) {
                    destination.push_back(utf8_to_wstring(item.get<std::string>()));
                }
            }
        }
        //=============================================================================
        void
        applyFileSettings(CodeAnalyzerOptions& options, const nlohmann::json& settings)
        {
            if (!settings.contains("files") || !settings["files"].is_object()) {
                return;
            }
            const auto& files = settings["files"];
            if (files.contains("include")) {
                readStringArray(files["include"], options.inclusions);
            }
            if (files.contains("exclude")) {
                readStringArray(files["exclude"], options.exclusions);
            }
        }
        //=============================================================================
        void
        applyRuleSettings(CodeAnalyzerOptions& options, const nlohmann::json& settings)
        {
            if (!settings.contains("rules") || !settings["rules"].is_object()) {
                return;
            }
            for (auto it = settings["rules"].begin(); it != settings["rules"].end(); ++it) {
                const std::string id = upperAscii(it.key());
                std::string level;
                if (it.value().is_string()) {
                    level = it.value().get<std::string>();
                } else if (it.value().is_object() && it.value().contains("level")
                    && it.value()["level"].is_string()) {
                    level = it.value()["level"].get<std::string>();
                }
                if (codeAnalyzerRuleInfo(id) != nullptr && isValidSeverity(level)) {
                    options.ruleSeverity[id] = level;
                }
            }
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    nlohmann::json
    lintSettingsFromChangeConfigurationParams(const nlohmann::json& params)
    {
        const nlohmann::json settings = params.value("settings", nlohmann::json::object());
        if (!settings.is_object()) {
            return nlohmann::json::object();
        }
        if (settings.contains("nelson") && settings["nelson"].is_object()
            && settings["nelson"].contains("lint") && settings["nelson"]["lint"].is_object()) {
            return settings["nelson"]["lint"];
        }
        for (const std::string& key : { "nelsonLint", "nelson-lint", "lint" }) {
            if (settings.contains(key) && settings[key].is_object()) {
                return settings[key];
            }
        }
        if (settings.contains("rules") || settings.contains("files")) {
            return settings;
        }
        return nlohmann::json::object();
    }
    //=============================================================================
    void
    applyLspLintSettings(CodeAnalyzerOptions& options, const nlohmann::json& settings)
    {
        if (!settings.is_object()) {
            return;
        }
        applyFileSettings(options, settings);
        applyRuleSettings(options, settings);
    }
    //=============================================================================
} // namespace Lsp
//=============================================================================
} // namespace Nelson
//=============================================================================
