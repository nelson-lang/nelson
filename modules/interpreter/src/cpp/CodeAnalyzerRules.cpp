//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerRules.hpp"
#include "CodeAnalyzerRulesJson.hpp"
#include "nlohmann/json.hpp"
#include <algorithm>
#include <set>
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    requiredString(const nlohmann::json& rule, const std::string& key)
    {
        if (!rule.contains(key) || !rule.at(key).is_string()) {
            throw std::runtime_error("Invalid code analyzer rule registry field: " + key);
        }
        return rule.at(key).get<std::string>();
    }
    //=============================================================================
    bool
    validSeverity(const std::string& severity)
    {
        return severity == "allow" || severity == "info" || severity == "warning"
            || severity == "error";
    }
    //=============================================================================
    bool
    validFixability(const std::string& fixability)
    {
        return fixability == "none" || fixability == "trivial" || fixability == "safe"
            || fixability == "unsafe";
    }
    //=============================================================================
    CodeAnalyzerRuleInfo
    ruleInfoFromJson(const nlohmann::json& rule)
    {
        if (!rule.is_object()) {
            throw std::runtime_error("Code analyzer rule registry entries must be objects.");
        }
        CodeAnalyzerRuleInfo info;
        info.id = requiredString(rule, "id");
        info.name = requiredString(rule, "name");
        info.defaultSeverity = requiredString(rule, "defaultSeverity");
        info.category = requiredString(rule, "category");
        info.fixability = requiredString(rule, "fixability");
        info.description = requiredString(rule, "description");
        info.helpUri = requiredString(rule, "helpUri");
        if (!validSeverity(info.defaultSeverity)) {
            throw std::runtime_error("Invalid code analyzer rule severity: " + info.id);
        }
        if (!validFixability(info.fixability)) {
            throw std::runtime_error("Invalid code analyzer rule fixability: " + info.id);
        }
        return info;
    }
    //=============================================================================
    void
    appendRule(std::vector<CodeAnalyzerRuleInfo>& rules, std::set<std::string>& ids,
        std::set<std::string>& names, const CodeAnalyzerRuleInfo& rule)
    {
        if (!ids.insert(rule.id).second) {
            throw std::runtime_error("Duplicate code analyzer rule id: " + rule.id);
        }
        if (!names.insert(rule.name).second) {
            throw std::runtime_error("Duplicate code analyzer rule name: " + rule.name);
        }
        rules.push_back(rule);
    }
    //=============================================================================
    std::vector<CodeAnalyzerRuleInfo>
    loadRuleTable()
    {
        std::vector<CodeAnalyzerRuleInfo> rules;
        const nlohmann::json ruleJson
            = nlohmann::json::parse(CodeAnalyzerRuleRegistry::RULE_TABLE_JSON);
        if (!ruleJson.is_array()) {
            throw std::runtime_error("Code analyzer rule registry must be a JSON array.");
        }
        std::set<std::string> ids;
        std::set<std::string> names;
        for (const auto& rule : ruleJson) {
            appendRule(rules, ids, names, ruleInfoFromJson(rule));
        }
        return rules;
    }
    //=============================================================================
    const std::vector<CodeAnalyzerRuleInfo>&
    ruleTable()
    {
        static const std::vector<CodeAnalyzerRuleInfo> rules = loadRuleTable();
        return rules;
    }
    //=============================================================================
} // namespace
//=============================================================================
const CodeAnalyzerRuleInfo*
codeAnalyzerRuleInfo(const std::string& id)
{
    const auto& rules = ruleTable();
    auto found = std::find_if(rules.begin(), rules.end(),
        [&](const CodeAnalyzerRuleInfo& rule) { return rule.id == id; });
    return found == rules.end() ? nullptr : &(*found);
}
//=============================================================================
std::vector<CodeAnalyzerRuleInfo>
codeAnalyzerRules()
{
    return ruleTable();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
