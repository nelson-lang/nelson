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
#include "EvaluateCommand.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <regex>
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
const std::vector<ToolDefinition>&
toolDefinitions()
{
    static const std::vector<ToolDefinition> tools = {
        { "detect_nelson_modules", "Detect Nelson Modules",
            "List Nelson modules and their builtin, function, test, and help files.",
            R"({"type":"object","properties":{"module":{"type":"string","description":"Optional module name to inspect."}}})",
            true, false, false },
        { "check_nelson_code", "Check Nelson Code",
            "Run Nelson static code checks on a .m file and return diagnostics.",
            R"({"type":"object","properties":{"script_path":{"type":"string","description":"Absolute or repository-relative .m file path."}},"required":["script_path"]})",
            true, false, false },
        { "lint_nelson_code", "Lint Nelson Code",
            "Run nelson-lint on a Nelson file or folder and return diagnostics.",
            R"({"type":"object","properties":{"path":{"type":"string","description":"Absolute or repository-relative .m file or folder path."},"format":{"type":"string","enum":["json","text","sarif"],"description":"Diagnostic output format. Defaults to json."},"include_subfolders":{"type":"boolean","description":"Include subfolders when path is a folder. Defaults to true."},"config_path":{"type":"string","description":"Optional absolute or repository-relative nelson-lint configuration file."}},"required":["path"]})",
            true, false, false },
        { "check_nelson_format", "Check Nelson Format",
            "Check whether Nelson .m files would be changed by nelson-format without editing them.",
            R"({"type":"object","properties":{"path":{"type":"string","description":"Absolute or repository-relative .m file or folder path."},"config_path":{"type":"string","description":"Optional absolute or repository-relative nelson-format configuration file."}},"required":["path"]})",
            true, false, false },
        { "evaluate_nelson_code", "Evaluate Nelson Code",
            "Evaluate Nelson code and return captured command output.",
            R"({"type":"object","properties":{"code":{"type":"string","description":"Nelson code to evaluate."},"project_path":{"type":"string","description":"Optional working folder."}},"required":["code"]})",
            false, true, true },
        { "run_nelson_file", "Run Nelson File",
            "Run a Nelson .m file and return captured command output.",
            R"({"type":"object","properties":{"script_path":{"type":"string","description":"Absolute or repository-relative .m file path."},"project_path":{"type":"string","description":"Optional working folder."}},"required":["script_path"]})",
            false, true, true },
        { "run_nelson_test_file", "Run Nelson Test",
            "Run a Nelson test file or module with test_run/nelson.unittest.",
            R"({"type":"object","properties":{"target":{"type":"string","description":"Module name or .m test file path."}},"required":["target"]})",
            false, true, true },
        { "format_nelson_file", "Format Nelson File",
            "Format a Nelson .m file with nelson-format, respecting nelson-format.json and "
            "nelson-format-ignore.json.",
            R"({"type":"object","properties":{"script_path":{"type":"string","description":"Absolute or repository-relative .m file path."}},"required":["script_path"]})",
            false, true, false },
        { "get_nelson_help", "Get Nelson Help", "Return local Nelson help for a symbol.",
            R"({"type":"object","properties":{"symbol":{"type":"string","description":"Nelson symbol or function name."}},"required":["symbol"]})",
            true, false, false },
        { "find_nelson_symbol", "Find Nelson Symbol",
            "Find where a Nelson symbol is defined, registered, tested, and documented.",
            R"({"type":"object","properties":{"symbol":{"type":"string","description":"Nelson symbol or function name."}},"required":["symbol"]})",
            true, false, false },
        { "get_nelson_function_info", "Get Nelson Function Info",
            "Return which/nargin/nargout and local source, test, and help matches for a symbol.",
            R"({"type":"object","properties":{"symbol":{"type":"string","description":"Nelson symbol or function name."}},"required":["symbol"]})",
            true, false, false },
        { "get_nelson_workspace", "Get Nelson Workspace",
            "Return Nelson root, workspace root, temp folder, binary folder, and server policy.",
            R"({"type":"object","properties":{}})", true, false, false },
        { "get_nelson_version", "Get Nelson Version",
            "Return Nelson version, binary, root, mode, and MCP server information.",
            R"({"type":"object","properties":{}})", true, false, false },
        { "create_nelson_plot", "Create Nelson Plot",
            "Evaluate Nelson plotting code and return a generated PNG file path.",
            R"({"type":"object","properties":{"code":{"type":"string","description":"Nelson plotting code to evaluate."},"project_path":{"type":"string","description":"Optional working folder."}},"required":["code"]})",
            false, true, true },
    };
    return tools;
}
//=============================================================================
std::wstring
getNelsonRoot()
{
    return NelsonConfiguration::getInstance()->getNelsonRootDirectory();
}
//=============================================================================
std::wstring
getNelsonModulesRoot()
{
    std::wstring modules = NelsonConfiguration::getInstance()->getNelsonModulesDirectory();
    if (!modules.empty()) {
        return modules;
    }
    return getNelsonRoot() + L"/modules";
}
//=============================================================================
std::wstring
getNelsonBinary()
{
    return NelsonConfiguration::getInstance()->getNelsonBinaryDirectory();
}
//=============================================================================
std::wstring
jsonEscape(const std::wstring& input)
{
    std::wstring out;
    out.reserve(input.size() + 8);
    for (wchar_t ch : input) {
        switch (ch) {
        case L'"':
            out += L"\\\"";
            break;
        case L'\\':
            out += L"\\\\";
            break;
        case L'\b':
            out += L"\\b";
            break;
        case L'\f':
            out += L"\\f";
            break;
        case L'\n':
            out += L"\\n";
            break;
        case L'\r':
            out += L"\\r";
            break;
        case L'\t':
            out += L"\\t";
            break;
        default:
            if (ch >= 0 && ch < 32) {
                wchar_t buffer[7];
#ifdef _MSC_VER
                swprintf_s(buffer, L"\\u%04x", static_cast<unsigned int>(ch));
#else
                swprintf(buffer, 7, L"\\u%04x", static_cast<unsigned int>(ch));
#endif
                out += buffer;
            } else {
                out.push_back(ch);
            }
        }
    }
    return out;
}
//=============================================================================
std::string
jsonString(const std::wstring& value)
{
    return "\"" + wstring_to_utf8(jsonEscape(value)) + "\"";
}
//=============================================================================
std::string
jsonString(const std::string& value)
{
    return jsonString(utf8_to_wstring(value));
}
//=============================================================================
std::wstring
nelsonStringLiteral(const std::wstring& value)
{
    std::wstring escaped;
    escaped.reserve(value.size() + 2);
    escaped.push_back(L'\'');
    for (wchar_t ch : value) {
        escaped.push_back(ch);
        if (ch == L'\'') {
            escaped.push_back(ch);
        }
    }
    escaped.push_back(L'\'');
    return escaped;
}
//=============================================================================
bool
hasField(const ArrayOf& value, const std::string& field)
{
    return (value.isStruct() && value.isScalar() && value.isField(field));
}
//=============================================================================
ArrayOf
fieldOrEmpty(const ArrayOf& value, const std::string& field)
{
    if (!hasField(value, field)) {
        return ArrayOf::empty();
    }
    return value.field(field);
}
//=============================================================================
std::wstring
fieldAsString(const ArrayOf& value, const std::string& field, const std::wstring& defaultValue)
{
    ArrayOf fieldValue = fieldOrEmpty(value, field);
    if (fieldValue.isEmpty()) {
        return defaultValue;
    }
    if (fieldValue.isRowVectorCharacterArray() || fieldValue.isScalarStringArray()) {
        return fieldValue.asWideString();
    }
    return defaultValue;
}
//=============================================================================
std::filesystem::path
toPath(const std::wstring& value)
{
    return std::filesystem::path(value);
}
//=============================================================================
std::filesystem::path
nelsonFormatExecutable()
{
    std::filesystem::path executable = toPath(getNelsonBinary());
#ifdef _WIN32
    executable /= L"nelson-format.exe";
#else
    executable /= L"nelson-format";
#endif
    return executable;
}
//=============================================================================
std::wstring
normalizePathForNelson(const std::filesystem::path& path)
{
    std::wstring value = path.wstring();
    std::replace(value.begin(), value.end(), L'\\', L'/');
    return value;
}
//=============================================================================
std::filesystem::path
canonicalPath(const std::filesystem::path& path)
{
    return std::filesystem::weakly_canonical(path);
}
//=============================================================================
std::filesystem::path
workspaceRootPath(const McpOptions& options)
{
    std::filesystem::path root
        = toPath(options.workspaceRoot.empty() ? getNelsonRoot() : options.workspaceRoot);
    return canonicalPath(root);
}
//=============================================================================
std::filesystem::path
resolvePath(const std::wstring& input, const std::filesystem::path& base)
{
    std::filesystem::path path = toPath(input);
    if (path.is_relative()) {
        path = base / path;
    }
    return canonicalPath(path);
}
//=============================================================================
std::filesystem::path
resolveWorkspacePath(const std::wstring& input, const McpOptions& options)
{
    return resolvePath(input, workspaceRootPath(options));
}
//=============================================================================
bool
isInside(const std::filesystem::path& child, const std::filesystem::path& parent)
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
isInsideWorkspace(const std::filesystem::path& child, const McpOptions& options)
{
    return isInside(child, workspaceRootPath(options));
}
//=============================================================================
ToolResult
outsideWorkspaceResult(const std::wstring& toolName)
{
    ToolResult result;
    result.success = false;
    result.error = toolName + L" refuses paths outside workspace root.";
    return result;
}
//=============================================================================
ToolResult
evaluate(Evaluator* eval, const std::wstring& command)
{
    auto start = std::chrono::steady_clock::now();
    ToolResult result;
    result.success = EvaluateConsoleCommandToString(eval, command, result.text);
    if (!result.success) {
        result.error = result.text;
    }
    auto stop = std::chrono::steady_clock::now();
    result.duration = std::chrono::duration<double>(stop - start).count();
    return result;
}
//=============================================================================
std::wstring
withWorkingFolder(const std::wstring& command, const std::filesystem::path& folder)
{
    std::wstring normalized = normalizePathForNelson(folder);
    return L"__mcp_pwd = pwd(); __mcp_cleanup = onCleanup(@() cd(__mcp_pwd)); cd("
        + nelsonStringLiteral(normalized) + L"); " + command;
}
//=============================================================================
ToolResult
evaluateWithWorkingFolder(Evaluator* eval, const std::wstring& command,
    const std::wstring& projectPath, const McpOptions& options, const std::wstring& toolName)
{
    if (projectPath.empty()) {
        return evaluate(eval, command);
    }
    std::filesystem::path path = resolveWorkspacePath(projectPath, options);
    if (!isInsideWorkspace(path, options)) {
        return outsideWorkspaceResult(toolName);
    }
    return evaluate(eval, withWorkingFolder(command, path));
}
//=============================================================================
std::string
listDirectoryFilesJson(const std::filesystem::path& directory, const std::string& extension)
{
    std::string json = "[";
    bool first = true;
    if (std::filesystem::is_directory(directory)) {
        for (const auto& entry : std::filesystem::directory_iterator(directory)) {
            if (!entry.is_regular_file()) {
                continue;
            }
            if (!extension.empty() && entry.path().extension().string() != extension) {
                continue;
            }
            if (!first) {
                json += ",";
            }
            first = false;
            json += jsonString(normalizePathForNelson(entry.path()));
        }
    }
    json += "]";
    return json;
}
//=============================================================================
std::string
moduleInfoJson(const std::filesystem::path& modulePath)
{
    std::string json = "{";
    json += "\"name\":" + jsonString(modulePath.filename().wstring());
    json += ",\"path\":" + jsonString(normalizePathForNelson(modulePath));
    json += ",\"builtins\":" + listDirectoryFilesJson(modulePath / "builtin" / "cpp", ".cpp");
    json += ",\"functions\":" + listDirectoryFilesJson(modulePath / "functions", ".m");
    json += ",\"tests\":" + listDirectoryFilesJson(modulePath / "tests", ".m");
    json += ",\"help\":" + listDirectoryFilesJson(modulePath / "help" / "en_US" / "xml", ".xml");
    json += "}";
    return json;
}
//=============================================================================
ToolResult
detectNelsonModules(const ArrayOf& args)
{
    ToolResult result;
    std::wstring requested = fieldAsString(args, "module");
    std::filesystem::path modulesRoot = toPath(getNelsonModulesRoot());
    std::string json = "[";
    bool first = true;
    if (std::filesystem::is_directory(modulesRoot)) {
        for (const auto& entry : std::filesystem::directory_iterator(modulesRoot)) {
            if (!entry.is_directory()) {
                continue;
            }
            if (!requested.empty() && entry.path().filename().wstring() != requested) {
                continue;
            }
            if (!first) {
                json += ",";
            }
            first = false;
            json += moduleInfoJson(entry.path());
        }
    }
    json += "]";
    result.text = utf8_to_wstring(json);
    result.dataJson = "{\"modules\":" + json + "}";
    return result;
}
//=============================================================================
ToolResult
findNelsonSymbol(const ArrayOf& args)
{
    ToolResult result;
    std::wstring symbol = fieldAsString(args, "symbol");
    if (symbol.empty()) {
        result.success = false;
        result.error = L"symbol is required.";
        return result;
    }
    SymbolSearchResult searchResult = searchNelsonSymbolMatches(symbol);
    result.text = utf8_to_wstring(searchResult.matchesJson);
    result.dataJson = "{\"matches\":" + searchResult.matchesJson + "}";
    return result;
}
//=============================================================================
bool
isIgnoredByNelsonFormat(const std::filesystem::path& file)
{
    std::filesystem::path ignorePath = toPath(getNelsonRoot()) / "nelson-format-ignore.json";
    std::ifstream input(ignorePath, std::ios::binary);
    if (!input.good()) {
        return false;
    }
    std::string content((std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
    std::wstring relative
        = normalizePathForNelson(std::filesystem::relative(file, toPath(getNelsonRoot())));
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
std::wstring
shellQuotedPath(const std::filesystem::path& path)
{
    std::wstring value = normalizePathForNelson(path);
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
ToolResult
formatNelsonFile(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    ToolResult result;
    std::wstring scriptPath = fieldAsString(args, "script_path");
    if (scriptPath.empty()) {
        result.success = false;
        result.error = L"script_path is required.";
        return result;
    }
    std::filesystem::path file = resolveWorkspacePath(scriptPath, options);
    if (!isInsideWorkspace(file, options)) {
        return outsideWorkspaceResult(L"format_nelson_file");
    }
    std::filesystem::path root = toPath(getNelsonRoot());
    if (!isInside(file, root)) {
        result.success = false;
        result.error = L"format_nelson_file refuses paths outside nelsonroot.";
        return result;
    }
    if (file.extension() != ".m") {
        result.success = false;
        result.error = L"format_nelson_file expects a .m file.";
        return result;
    }
    if (isIgnoredByNelsonFormat(file)) {
        result.success = false;
        result.error = L"File is listed in nelson-format-ignore.json.";
        return result;
    }
    std::filesystem::path formatter = nelsonFormatExecutable();
    if (!std::filesystem::is_regular_file(formatter)) {
        result.success = false;
        result.error = L"nelson-format executable is not available.";
        return result;
    }
    std::wstring commandLine = shellQuotedPath(formatter) + L" " + shellQuotedPath(file);
    return evaluate(eval,
        L"[__mcp_status, __mcp_output] = system(" + nelsonStringLiteral(commandLine)
            + L"); if __mcp_status ~= 0, error(__mcp_output); end; "
              L"if ~isempty(__mcp_output), disp(__mcp_output); end;");
}
//=============================================================================
ToolResult
getNelsonHelp(Evaluator* eval, const ArrayOf& args)
{
    std::wstring symbol = fieldAsString(args, "symbol");
    if (symbol.empty()) {
        return { false, L"", L"symbol is required.", L"", "", 0 };
    }
    return evaluate(eval, L"help(" + nelsonStringLiteral(symbol) + L")");
}
//=============================================================================
ToolResult
checkNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    std::wstring scriptPath = fieldAsString(args, "script_path");
    if (scriptPath.empty()) {
        return { false, L"", L"script_path is required.", L"", "", 0 };
    }
    std::filesystem::path file = resolveWorkspacePath(scriptPath, options);
    if (!isInsideWorkspace(file, options)) {
        return outsideWorkspaceResult(L"check_nelson_code");
    }
    if (file.extension() != ".m") {
        return { false, L"", L"check_nelson_code expects a .m file.", L"", "", 0 };
    }
    return evaluate(eval, L"checkcode(" + nelsonStringLiteral(normalizePathForNelson(file)) + L")");
}
//=============================================================================
ToolResult
runNelsonFile(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    std::wstring scriptPath = fieldAsString(args, "script_path");
    std::wstring projectPath = fieldAsString(args, "project_path");
    if (scriptPath.empty()) {
        return { false, L"", L"script_path is required.", L"", "", 0 };
    }
    std::filesystem::path file = resolveWorkspacePath(scriptPath, options);
    if (!isInsideWorkspace(file, options)) {
        return outsideWorkspaceResult(L"run_nelson_file");
    }
    if (file.extension() != ".m") {
        return { false, L"", L"run_nelson_file expects a .m file.", L"", "", 0 };
    }
    std::wstring command = L"run(" + nelsonStringLiteral(normalizePathForNelson(file)) + L")";
    return evaluateWithWorkingFolder(eval, command, projectPath, options, L"run_nelson_file");
}
//=============================================================================
int
extractSummaryCount(const std::wstring& text, const std::wstring& label)
{
    std::wregex pattern(label + L"\\s*:\\s*([0-9]+)");
    std::wsmatch match;
    if (std::regex_search(text, match, pattern) && match.size() > 1) {
        return std::stoi(match[1].str());
    }
    return -1;
}
//=============================================================================
ToolResult
runNelsonTest(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    std::wstring target = fieldAsString(args, "target");
    if (target.empty()) {
        return { false, L"", L"target is required.", L"", "", 0 };
    }
    std::filesystem::path possibleFile = resolveWorkspacePath(target, options);
    std::wstring command;
    if (std::filesystem::is_regular_file(possibleFile) || possibleFile.extension() == ".m") {
        if (!isInsideWorkspace(possibleFile, options)) {
            return outsideWorkspaceResult(L"run_nelson_test_file");
        }
        command = L"r = nelson.unittest.run("
            + nelsonStringLiteral(normalizePathForNelson(possibleFile))
            + L"); fprintf('Tests : %d\\nPassed : %d\\nFailed : %d\\nSkipped : %d\\n', "
              L"r.summary.tests, r.summary.passed, r.summary.failed, r.summary.skipped); "
              L"if ~r.status, error('Nelson:tests_manager:testFailed', 'Test failed.'); end";
    } else {
        command = L"test_run(" + nelsonStringLiteral(target) + L")";
    }
    ToolResult result = evaluate(eval, command);
    int tests = extractSummaryCount(result.text, L"Tests");
    int passed = extractSummaryCount(result.text, L"Passed");
    int failed = extractSummaryCount(result.text, L"Failed");
    int skipped = extractSummaryCount(result.text, L"Skipped");
    if (result.success) {
        if (failed < 0) {
            failed = 0;
        }
        if (skipped < 0) {
            skipped = 0;
        }
        if (passed < 0) {
            passed = 1;
        }
        if (tests < 0) {
            tests = passed + failed + skipped;
        }
    }
    result.dataJson = "{\"target\":" + jsonString(target) + ",\"tests\":" + std::to_string(tests)
        + ",\"passed\":" + std::to_string(passed) + ",\"failed\":" + std::to_string(failed)
        + ",\"skipped\":" + std::to_string(skipped) + "}";
    return result;
}
//=============================================================================
ToolResult
evaluateNelsonCode(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    std::wstring code = fieldAsString(args, "code");
    std::wstring projectPath = fieldAsString(args, "project_path");
    if (code.empty()) {
        return { false, L"", L"code is required.", L"", "", 0 };
    }
    return evaluateWithWorkingFolder(eval, code, projectPath, options, L"evaluate_nelson_code");
}
//=============================================================================
ToolResult
getNelsonVersion(Evaluator* eval, const McpOptions& options)
{
    ToolResult result = evaluate(eval, L"version");
    std::string json = "{";
    json += "\"version_output\":" + jsonString(result.text);
    json += ",\"nelsonroot\":" + jsonString(getNelsonRoot());
    json += ",\"binary_directory\":" + jsonString(getNelsonBinary());
    json += ",\"workspace_root\":" + jsonString(normalizePathForNelson(workspaceRootPath(options)));
    json += ",\"display_mode\":" + jsonString(options.displayMode);
    json += ",\"mcp_server_version\":" + jsonString(MCP_SERVER_VERSION);
    json += ",\"max_output_characters\":" + std::to_string(options.maxOutputCharacters);
    json += ",\"allow_execution\":" + std::string(options.allowExecution ? "true" : "false");
    json += ",\"allow_format\":" + std::string(options.allowFormat ? "true" : "false");
    json += "}";
    result.success = true;
    result.error.clear();
    result.text = utf8_to_wstring(json);
    result.dataJson = json;
    return result;
}
//=============================================================================
ToolResult
getNelsonWorkspace(const McpOptions& options)
{
    ToolResult result;
    std::filesystem::path temp = std::filesystem::temp_directory_path() / "nelson-ai";
    std::string json = "{";
    json += "\"nelsonroot\":" + jsonString(getNelsonRoot());
    json += ",\"modules_directory\":" + jsonString(getNelsonModulesRoot());
    json += ",\"binary_directory\":" + jsonString(getNelsonBinary());
    json += ",\"workspace_root\":" + jsonString(normalizePathForNelson(workspaceRootPath(options)));
    json += ",\"temp_directory\":" + jsonString(normalizePathForNelson(temp));
    json += ",\"display_mode\":" + jsonString(options.displayMode);
    json += ",\"max_output_characters\":" + std::to_string(options.maxOutputCharacters);
    json += ",\"allow_execution\":" + std::string(options.allowExecution ? "true" : "false");
    json += ",\"allow_format\":" + std::string(options.allowFormat ? "true" : "false");
    json += "}";
    result.text = utf8_to_wstring(json);
    result.dataJson = json;
    return result;
}
//=============================================================================
ToolResult
createNelsonPlot(Evaluator* eval, const ArrayOf& args, const McpOptions& options)
{
    if (options.displayMode == "cli") {
        return { false, L"", L"create_nelson_plot is not available in cli display mode.", L"", "",
            0 };
    }
    std::wstring code = fieldAsString(args, "code");
    std::wstring projectPath = fieldAsString(args, "project_path");
    if (code.empty()) {
        return { false, L"", L"code is required.", L"", "", 0 };
    }
    std::filesystem::path outputDir = std::filesystem::temp_directory_path() / "nelson-ai";
    std::filesystem::create_directories(outputDir);
    auto stamp = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch())
                     .count();
    std::filesystem::path outputFile = outputDir / ("plot_" + std::to_string(stamp) + ".png");
    std::wstring output = normalizePathForNelson(outputFile);
    std::wstring command = code + L"; drawnow(); saveas(gcf(), " + nelsonStringLiteral(output)
        + L"); close('all'); drawnow();";
    ToolResult result
        = evaluateWithWorkingFolder(eval, command, projectPath, options, L"create_nelson_plot");
    if (result.success && std::filesystem::is_regular_file(outputFile)) {
        result.image = output;
        auto size = std::filesystem::file_size(outputFile);
        result.dataJson = "{\"path\":" + jsonString(output)
            + ",\"mimeType\":\"image/png\",\"size\":" + std::to_string(size) + "}";
    } else if (result.success) {
        result.success = false;
        result.error = L"Plot image was not created.";
    }
    return result;
}
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
