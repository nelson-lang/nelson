//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include <filesystem>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
inline constexpr const char* MCP_PROTOCOL_VERSION = "2025-06-18";
inline constexpr const char* MCP_SERVER_NAME = "nelson";
inline constexpr const char* MCP_SERVER_VERSION = "0.7.0";
inline constexpr size_t MCP_DEFAULT_MAX_OUTPUT_CHARACTERS = 200000;
//=============================================================================
struct McpOptions
{
    std::wstring initialWorkingFolder;
    std::string displayMode = "adv-cli";
    std::string logLevel = "error";
    std::wstring logFolder;
    std::wstring workspaceRoot;
    size_t maxOutputCharacters = MCP_DEFAULT_MAX_OUTPUT_CHARACTERS;
    bool initializeNelsonOnStartup = false;
    bool allowExecution = true;
    bool allowFormat = true;
};
//=============================================================================
struct ToolResult
{
    bool success = true;
    std::wstring text;
    std::wstring error;
    std::wstring image;
    std::string dataJson;
    double duration = 0;
    bool outputTruncated = false;
    bool errorTruncated = false;
};
//=============================================================================
struct ToolDefinition
{
    const char* name;
    const char* title;
    const char* description;
    const char* inputSchema;
    bool readOnly;
    bool destructive;
    bool openWorld;
};
//=============================================================================
struct SkillDefinition
{
    const char* uri;
    const char* name;
    const char* description;
    const wchar_t* title;
    const wchar_t* relativePath;
};
//=============================================================================
const std::vector<ToolDefinition>&
toolDefinitions();
//=============================================================================
const std::vector<SkillDefinition>&
skillDefinitions();
//=============================================================================
std::wstring
skillCatalogText();
//=============================================================================
std::wstring
skillResourceText(const std::wstring& uri);
//=============================================================================
std::wstring
localDocumentExcerpt(const std::wstring& title, const std::wstring& relativePath);
//=============================================================================
std::wstring
getNelsonRoot();
//=============================================================================
std::wstring
getNelsonModulesRoot();
//=============================================================================
std::wstring
getNelsonBinary();
//=============================================================================
std::string
jsonString(const std::wstring& value);
//=============================================================================
std::string
jsonString(const std::string& value);
//=============================================================================
std::wstring
nelsonStringLiteral(const std::wstring& value);
//=============================================================================
bool
hasField(const ArrayOf& value, const std::string& field);
//=============================================================================
ArrayOf
fieldOrEmpty(const ArrayOf& value, const std::string& field);
//=============================================================================
std::wstring
fieldAsString(
    const ArrayOf& value, const std::string& field, const std::wstring& defaultValue = L"");
//=============================================================================
ToolResult
evaluate(Evaluator* eval, const std::wstring& command);
//=============================================================================
ToolResult
detectNelsonModules(const ArrayOf& args);
//=============================================================================
ToolResult
findNelsonSymbol(const ArrayOf& args);
//=============================================================================
ToolResult
lintNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
checkNelsonFormat(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
getNelsonFunctionInfo(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
formatNelsonFile(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
getNelsonHelp(Evaluator* eval, const ArrayOf& args);
//=============================================================================
ToolResult
checkNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
runNelsonFile(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
runNelsonTest(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
evaluateNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
ToolResult
getNelsonVersion(Evaluator* eval, const McpOptions& options);
//=============================================================================
ToolResult
getNelsonWorkspace(const McpOptions& options);
//=============================================================================
ToolResult
createNelsonPlot(Evaluator* eval, const ArrayOf& args, const McpOptions& options);
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
