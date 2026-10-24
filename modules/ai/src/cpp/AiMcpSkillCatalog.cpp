//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AiMcpServerPrivate.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <fstream>
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
const std::vector<SkillDefinition>&
skillDefinitions()
{
    static const std::vector<SkillDefinition> skills = {
        { "skills://nelson-core/module-work", "Nelson Core module work",
            "Skill for scoped Nelson Core module changes.", L"Nelson Core Module Work",
            L"modules/ai/skills-catalog/nelson-core/module-work/SKILL.md" },
        { "skills://nelson-core/debugging", "Nelson Core debugging",
            "Skill for debugging Nelson failures and unstable CLI behavior.",
            L"Nelson Core Debugging", L"modules/ai/skills-catalog/nelson-core/debugging/SKILL.md" },
        { "skills://nelson-core/code-review", "Nelson Core code review",
            "Skill for reviewing Nelson changes.", L"Nelson Core Code Review",
            L"modules/ai/skills-catalog/nelson-core/code-review/SKILL.md" },
        { "skills://nelson-core/testing", "Nelson Core testing",
            "Skill for writing and running focused Nelson tests.", L"Nelson Core Testing",
            L"modules/ai/skills-catalog/nelson-core/testing/SKILL.md" },
        { "skills://nelson-core/native-builtins", "Nelson Core native builtins",
            "Skill for C++ builtin implementation work.", L"Nelson Core Native Builtins",
            L"modules/ai/skills-catalog/nelson-core/native-builtins/SKILL.md" },
        { "skills://nelson-core/help-authoring", "Nelson Core help authoring",
            "Skill for Nelson XML help updates.", L"Nelson Core Help Authoring",
            L"modules/ai/skills-catalog/nelson-core/help-authoring/SKILL.md" },
        { "skills://nelson-core/performance", "Nelson Core performance",
            "Skill for improving runtime and output cost.", L"Nelson Core Performance",
            L"modules/ai/skills-catalog/nelson-core/performance/SKILL.md" },
        { "skills://nelson-core/packaging", "Nelson Core packaging",
            "Skill for installed data and platform packaging.", L"Nelson Core Packaging",
            L"modules/ai/skills-catalog/nelson-core/packaging/SKILL.md" },
        { "skills://nelson-data/analysis-workflows", "Nelson data analysis workflows",
            "Skill for reproducible Nelson data analysis.", L"Nelson Data Analysis Workflows",
            L"modules/ai/skills-catalog/nelson-data/analysis-workflows/SKILL.md" },
        { "skills://nelson-companion/mcp-workflows", "Nelson MCP workflows",
            "Skill for MCP stdio workflows and tool usage.", L"Nelson MCP Workflows",
            L"modules/ai/skills-catalog/nelson-companion/mcp-workflows/SKILL.md" },
        { "skills://nflow/create-block", "NFlow create block",
            "Skill for complete NFlow block implementation and validation.",
            L"Create an NFlow Block", L"modules/ai/skills-catalog/nflow/create-block/SKILL.md" },
        { "skills://nflow/create-library", "NFlow create library",
            "Skill for NFlow library manifests, renderers, plugins, and packaging.",
            L"Create an NFlow Library",
            L"modules/ai/skills-catalog/nflow/create-library/SKILL.md" },
        { "skills://nflow/create-model", "NFlow create model",
            "Skill for authoring and validating runnable NFlow models.", L"Create an NFlow Model",
            L"modules/ai/skills-catalog/nflow/create-model/SKILL.md" },
    };
    return skills;
}
//=============================================================================
static std::wstring
readLocalDocument(const std::wstring& relativePath, size_t maximumCharacters)
{
    std::filesystem::path path = std::filesystem::path(getNelsonRoot()) / relativePath;
    std::ifstream input(path, std::ios::binary);
    if (!input.good()) {
        return L"";
    }
    std::string content((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    for (char& ch : content) {
        if (static_cast<unsigned char>(ch) >= 128) {
            ch = ' ';
        }
    }
    std::wstring text = utf8_to_wstring(content);
    if (text.size() > maximumCharacters) {
        text = text.substr(0, maximumCharacters);
        text += L"\n\n...";
    }
    return text;
}
//=============================================================================
std::wstring
localDocumentExcerpt(const std::wstring& title, const std::wstring& relativePath)
{
    std::wstring text = readLocalDocument(relativePath, 3200);
    if (text.empty()) {
        return L"";
    }
    return L"\n\n## " + title + L"\nSource: " + relativePath + L"\n\n" + text;
}
//=============================================================================
std::wstring
skillCatalogText()
{
    std::wstring text = L"# Nelson Skill Catalog\n";
    for (const auto& skill : skillDefinitions()) {
        text += L"- " + utf8_to_wstring(skill.uri) + L"\n";
    }
    text += L"\nEach resource mirrors a local SKILL.md file under "
            L"modules/ai/skills-catalog.";
    return text;
}
//=============================================================================
std::wstring
skillResourceText(const std::wstring& uri)
{
    for (const auto& skill : skillDefinitions()) {
        if (uri != utf8_to_wstring(skill.uri)) {
            continue;
        }
        std::wstring text = readLocalDocument(skill.relativePath, 16000);
        if (text.empty()) {
            return L"";
        }
        return L"# " + std::wstring(skill.title) + L"\nSource: " + skill.relativePath + L"\n\n"
            + text;
    }
    return L"";
}
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
