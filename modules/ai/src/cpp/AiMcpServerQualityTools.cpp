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
#include "AiMcpSymbolSearch.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cwctype>
#include <exception>
#include <fstream>
#include <iterator>
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
namespace {
    //=============================================================================
    std::filesystem::path
    toPathLocal(const std::wstring& value)
    {
        return std::filesystem::path(value);
    }
    //=============================================================================
    std::wstring
    normalizePathForNelsonLocal(const std::filesystem::path& path)
    {
        std::wstring value = path.wstring();
        std::replace(value.begin(), value.end(), L'\\', L'/');
        return value;
    }
    //=============================================================================
    std::filesystem::path
    canonicalPathLocal(const std::filesystem::path& path)
    {
        return std::filesystem::weakly_canonical(path);
    }
    //=============================================================================
    std::filesystem::path
    workspaceRootPathLocal(const McpOptions& options)
    {
        std::filesystem::path root
            = toPathLocal(options.workspaceRoot.empty() ? getNelsonRoot() : options.workspaceRoot);
        return canonicalPathLocal(root);
    }
    //=============================================================================
    std::filesystem::path
    resolvePathLocal(const std::wstring& input, const std::filesystem::path& base)
    {
        std::filesystem::path path = toPathLocal(input);
        if (path.is_relative()) {
            path = base / path;
        }
        return canonicalPathLocal(path);
    }
    //=============================================================================
    std::filesystem::path
    resolveWorkspacePathLocal(const std::wstring& input, const McpOptions& options)
    {
        return resolvePathLocal(input, workspaceRootPathLocal(options));
    }
    //=============================================================================
    bool
    isInsideLocal(const std::filesystem::path& child, const std::filesystem::path& parent)
    {
        auto childPath = std::filesystem::weakly_canonical(child);
        auto parentPath = std::filesystem::weakly_canonical(parent);
        auto childIt = childPath.begin();
        auto parentIt = parentPath.begin();
        for (; parentIt != parentPath.end(); ++parentIt, ++childIt) {
            if (childIt == childPath.end() || *childIt != *parentIt) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    isInsideWorkspaceLocal(const std::filesystem::path& child, const McpOptions& options)
    {
        return isInsideLocal(child, workspaceRootPathLocal(options));
    }
    //=============================================================================
    ToolResult
    outsideWorkspaceResultLocal(const std::wstring& toolName)
    {
        ToolResult result;
        result.success = false;
        result.error = toolName + L" refuses paths outside workspace root.";
        return result;
    }
    //=============================================================================
    ToolResult
    pathOutsideWorkspaceResult(const std::wstring& pathName, const std::wstring& toolName)
    {
        ToolResult result;
        result.success = false;
        result.error = toolName + L" refuses " + pathName + L" outside workspace root.";
        return result;
    }
    //=============================================================================
    bool
    fieldAsBoolLocal(const ArrayOf& value, const std::string& field, bool defaultValue)
    {
        ArrayOf fieldValue = fieldOrEmpty(value, field);
        if (fieldValue.isEmpty()) {
            return defaultValue;
        }
        if (fieldValue.isLogical() && fieldValue.isScalar()) {
            return fieldValue.asLogicalScalar();
        }
        if (fieldValue.isNumeric() && fieldValue.isScalar()) {
            return fieldValue.asDoubleScalar() != 0;
        }
        if (fieldValue.isRowVectorCharacterArray() || fieldValue.isScalarStringArray()) {
            std::wstring text = fieldValue.asWideString();
            std::transform(text.begin(), text.end(), text.begin(),
                [](wchar_t ch) { return static_cast<wchar_t>(std::towlower(ch)); });
            if (text == L"true" || text == L"1") {
                return true;
            }
            if (text == L"false" || text == L"0") {
                return false;
            }
        }
        return defaultValue;
    }
    //=============================================================================
    std::filesystem::path
    nelsonLintExecutable()
    {
        std::filesystem::path executable = toPathLocal(getNelsonBinary());
#ifdef _WIN32
        executable /= L"nelson-lint.exe";
#else
        executable /= L"nelson-lint";
#endif
        return executable;
    }
    //=============================================================================
    std::filesystem::path
    nelsonFormatExecutable()
    {
        std::filesystem::path executable = toPathLocal(getNelsonBinary());
#ifdef _WIN32
        executable /= L"nelson-format.exe";
#else
        executable /= L"nelson-format";
#endif
        return executable;
    }
    //=============================================================================
    std::wstring
    shellQuotedPathLocal(const std::filesystem::path& path)
    {
        std::wstring value = normalizePathForNelsonLocal(path);
        std::wstring quoted;
        quoted.reserve(value.size() + 2);
        quoted.push_back(L'"');
        for (wchar_t ch : value) {
            if (ch == L'"') {
                quoted.push_back(L'\\');
            }
            quoted.push_back(ch);
        }
        quoted.push_back(L'"');
        return quoted;
    }
    //=============================================================================
    bool
    isIgnoredByNelsonFormatLocal(const std::filesystem::path& file)
    {
        std::filesystem::path ignorePath
            = toPathLocal(getNelsonRoot()) / "nelson-format-ignore.json";
        std::ifstream input(ignorePath, std::ios::binary);
        if (!input.good()) {
            return false;
        }
        std::string content(
            (std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
        std::wstring relative = normalizePathForNelsonLocal(
            std::filesystem::relative(file, toPathLocal(getNelsonRoot())));
        std::string relativeUtf8 = wstring_to_utf8(relative);
        if (content.find(relativeUtf8) != std::string::npos
            || content.find(file.filename().string()) != std::string::npos) {
            return true;
        }
        size_t pos = 0;
        while ((pos = content.find('"', pos)) != std::string::npos) {
            size_t end = content.find('"', pos + 1);
            if (end == std::string::npos) {
                break;
            }
            std::string pattern = content.substr(pos + 1, end - pos - 1);
            std::replace(pattern.begin(), pattern.end(), '\\', '/');
            if (pattern.rfind("**/", 0) == 0) {
                pattern = pattern.substr(3);
            }
            if (!pattern.empty() && relativeUtf8.find(pattern) != std::string::npos) {
                return true;
            }
            pos = end + 1;
        }
        return false;
    }
    //=============================================================================
    bool
    extractSystemStatus(std::wstring& text, int& status)
    {
        const std::wstring prefix = L"__NELSON_MCP_STATUS__:";
        size_t pos = text.find(prefix);
        if (pos == std::wstring::npos) {
            return false;
        }
        size_t valueStart = pos + prefix.size();
        size_t valueEnd = text.find_first_of(L"\r\n", valueStart);
        std::wstring value = text.substr(valueStart, valueEnd - valueStart);
        try {
            status = std::stoi(value);
        } catch (const std::exception&) {
            return false;
        }
        size_t eraseEnd = valueEnd;
        if (eraseEnd == std::wstring::npos) {
            eraseEnd = text.size();
        } else {
            while (eraseEnd < text.size() && (text[eraseEnd] == L'\r' || text[eraseEnd] == L'\n')) {
                ++eraseEnd;
            }
        }
        text.erase(pos, eraseEnd - pos);
        return true;
    }
    //=============================================================================
    ToolResult
    evaluateSystemCommand(Evaluator* eval, const std::wstring& commandLine, int& status)
    {
        ToolResult result = evaluate(eval,
            L"[__mcp_status, __mcp_output] = system(" + nelsonStringLiteral(commandLine)
                + L"); fprintf('__NELSON_MCP_STATUS__:%d\\n', __mcp_status); "
                  L"if ~isempty(__mcp_output), disp(__mcp_output); end;");
        status = -1;
        if (!result.success) {
            return result;
        }
        if (!extractSystemStatus(result.text, status)) {
            result.success = false;
            result.error = L"Could not read command status.";
        }
        return result;
    }
    //=============================================================================
    std::wstring
    trimWideString(const std::wstring& text)
    {
        size_t first = text.find_first_not_of(L" \t\r\n");
        if (first == std::wstring::npos) {
            return L"";
        }
        size_t last = text.find_last_not_of(L" \t\r\n");
        return text.substr(first, last - first + 1);
    }
    //=============================================================================
    std::string
    jsonNumberOrNull(const std::wstring& text)
    {
        std::wstring value = trimWideString(text);
        if (value.empty()) {
            return "null";
        }
        size_t start = value[0] == L'-' ? 1 : 0;
        if (start == value.size()) {
            return "null";
        }
        for (size_t k = start; k < value.size(); ++k) {
            if (!std::iswdigit(value[k])) {
                return "null";
            }
        }
        return wstring_to_utf8(value);
    }
    //=============================================================================
    bool
    isValidLintFormat(const std::string& format)
    {
        return format == "json" || format == "text" || format == "sarif";
    }
    //=============================================================================
    ToolResult
    missingExecutableResult(const std::wstring& executableName)
    {
        ToolResult result;
        result.success = false;
        result.error = executableName + L" executable is not available.";
        return result;
    }
    //=============================================================================
    std::string
    symbolMatchesJson(const std::wstring& symbol, bool& found, bool& hasTests, bool& hasHelp)
    {
        SymbolSearchResult result = searchNelsonSymbolMatches(symbol);
        found = result.found;
        hasTests = result.hasTests;
        hasHelp = result.hasHelp;
        return result.matchesJson;
    }
    //=============================================================================
    std::wstring
    evaluateText(Evaluator* eval, const std::wstring& command)
    {
        ToolResult result = evaluate(eval, command);
        if (!result.success) {
            return L"";
        }
        return trimWideString(result.text);
    }
    //=============================================================================
} // namespace
//=============================================================================
ToolResult
lintNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    ToolResult result;
    std::wstring requestedPath = fieldAsString(args, "path");
    if (requestedPath.empty()) {
        result.success = false;
        result.error = L"path is required.";
        return result;
    }
    std::filesystem::path target = resolveWorkspacePathLocal(requestedPath, options);
    if (!isInsideWorkspaceLocal(target, options)) {
        return outsideWorkspaceResultLocal(L"lint_nelson_code");
    }
    if (!std::filesystem::exists(target)) {
        result.success = false;
        result.error = L"path does not exist.";
        return result;
    }
    std::wstring formatWide = fieldAsString(args, "format", L"json");
    std::string format = wstring_to_utf8(formatWide);
    if (!isValidLintFormat(format)) {
        result.success = false;
        result.error = L"lint_nelson_code format must be json, text, or sarif.";
        return result;
    }
    std::filesystem::path linter = nelsonLintExecutable();
    if (!std::filesystem::is_regular_file(linter)) {
        return missingExecutableResult(L"nelson-lint");
    }
    std::wstring commandLine = shellQuotedPathLocal(linter) + L" --format "
        + utf8_to_wstring(format) + L" --include-subfolders "
        + std::wstring(fieldAsBoolLocal(args, "include_subfolders", true) ? L"true" : L"false");
    std::wstring configPath = fieldAsString(args, "config_path");
    if (!configPath.empty()) {
        std::filesystem::path config = resolveWorkspacePathLocal(configPath, options);
        if (!isInsideWorkspaceLocal(config, options)) {
            return pathOutsideWorkspaceResult(L"config_path", L"lint_nelson_code");
        }
        if (!std::filesystem::is_regular_file(config)) {
            result.success = false;
            result.error = L"config_path does not exist.";
            return result;
        }
        commandLine += L" --config " + shellQuotedPathLocal(config);
    }
    commandLine += L" " + shellQuotedPathLocal(target);
    int status = -1;
    result = evaluateSystemCommand(eval, commandLine, status);
    if (result.success && status > 1) {
        result.success = false;
        result.error = result.text.empty() ? L"nelson-lint failed." : result.text;
    }
    result.dataJson = "{\"path\":" + jsonString(normalizePathForNelsonLocal(target))
        + ",\"format\":" + jsonString(format) + ",\"status\":" + std::to_string(status)
        + ",\"has_diagnostics\":" + std::string(status == 1 ? "true" : "false") + "}";
    return result;
}
//=============================================================================
ToolResult
checkNelsonFormat(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    ToolResult result;
    std::wstring requestedPath = fieldAsString(args, "path");
    if (requestedPath.empty()) {
        result.success = false;
        result.error = L"path is required.";
        return result;
    }
    std::filesystem::path target = resolveWorkspacePathLocal(requestedPath, options);
    if (!isInsideWorkspaceLocal(target, options)) {
        return outsideWorkspaceResultLocal(L"check_nelson_format");
    }
    if (!std::filesystem::exists(target)) {
        result.success = false;
        result.error = L"path does not exist.";
        return result;
    }
    if (std::filesystem::is_regular_file(target) && target.extension() != ".m") {
        result.success = false;
        result.error = L"check_nelson_format expects a .m file or folder.";
        return result;
    }
    std::filesystem::path root = toPathLocal(getNelsonRoot());
    if (std::filesystem::is_regular_file(target) && isInsideLocal(target, root)
        && isIgnoredByNelsonFormatLocal(target)) {
        result.success = false;
        result.error = L"File is listed in nelson-format-ignore.json.";
        return result;
    }
    std::filesystem::path formatter = nelsonFormatExecutable();
    if (!std::filesystem::is_regular_file(formatter)) {
        return missingExecutableResult(L"nelson-format");
    }
    std::wstring commandLine = shellQuotedPathLocal(formatter) + L" --check";
    std::wstring configPath = fieldAsString(args, "config_path");
    if (!configPath.empty()) {
        std::filesystem::path config = resolveWorkspacePathLocal(configPath, options);
        if (!isInsideWorkspaceLocal(config, options)) {
            return pathOutsideWorkspaceResult(L"config_path", L"check_nelson_format");
        }
        if (!std::filesystem::is_regular_file(config)) {
            result.success = false;
            result.error = L"config_path does not exist.";
            return result;
        }
        commandLine += L" --config " + shellQuotedPathLocal(config);
    }
    commandLine += L" " + shellQuotedPathLocal(target);
    int status = -1;
    result = evaluateSystemCommand(eval, commandLine, status);
    if (result.success && status > 1) {
        result.success = false;
        result.error = result.text.empty() ? L"nelson-format --check failed." : result.text;
    }
    result.dataJson = "{\"path\":" + jsonString(normalizePathForNelsonLocal(target))
        + ",\"status\":" + std::to_string(status)
        + ",\"would_format\":" + std::string(status == 1 ? "true" : "false") + "}";
    return result;
}
//=============================================================================
ToolResult
getNelsonFunctionInfo(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    ToolResult result;
    std::wstring symbol = fieldAsString(args, "symbol");
    if (symbol.empty()) {
        result.success = false;
        result.error = L"symbol is required.";
        return result;
    }
    std::wstring literal = nelsonStringLiteral(symbol);
    std::wstring whichText = evaluateText(eval, L"disp(which(" + literal + L"))");
    std::wstring narginText
        = evaluateText(eval, L"try fprintf('%d', nargin(" + literal + L")); catch; end");
    std::wstring nargoutText
        = evaluateText(eval, L"try fprintf('%d', nargout(" + literal + L")); catch; end");
    bool found = false;
    bool hasTests = false;
    bool hasHelp = false;
    std::string matches = symbolMatchesJson(symbol, found, hasTests, hasHelp);
    if (whichText.empty() && !found) {
        result.success = false;
        result.error = L"symbol not found.";
    }
    std::string json = "{";
    json += "\"symbol\":" + jsonString(symbol);
    json += ",\"which\":" + jsonString(whichText);
    json += ",\"nargin\":" + jsonNumberOrNull(narginText);
    json += ",\"nargout\":" + jsonNumberOrNull(nargoutText);
    json += ",\"matches\":" + matches;
    json += ",\"has_tests\":" + std::string(hasTests ? "true" : "false");
    json += ",\"has_help\":" + std::string(hasHelp ? "true" : "false");
    json += ",\"workspace_root\":"
        + jsonString(normalizePathForNelsonLocal(workspaceRootPathLocal(options)));
    json += "}";
    result.text = utf8_to_wstring(json);
    result.dataJson = json;
    return result;
}
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
