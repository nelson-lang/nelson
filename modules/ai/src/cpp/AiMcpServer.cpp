//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "AiMcpServer.hpp"
#include "AiMcpServerPrivate.hpp"
#include "JsonDecode.hpp"
#include "characters_encoding.hpp"
#include <cstdlib>
#include <iostream>
#include <cwctype>
#include <sstream>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using namespace AiMcp;
    //=============================================================================
    std::string
    jsonId(const ArrayOf& request)
    {
        ArrayOf id = fieldOrEmpty(request, "id");
        if (id.isEmpty()) {
            return "null";
        }
        if (id.isRowVectorCharacterArray() || id.isScalarStringArray()) {
            return jsonString(id.asWideString());
        }
        if (id.isNumeric() || id.isLogical()) {
            double value = id.asDoubleScalar();
            std::ostringstream stream;
            stream << value;
            return stream.str();
        }
        return "null";
    }
    //=============================================================================
    std::string
    successResponse(const std::string& id, const std::string& resultJson)
    {
        return "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"result\":" + resultJson + "}";
    }
    //=============================================================================
    std::string
    errorResponse(const std::string& id, int code, const std::wstring& message)
    {
        return "{\"jsonrpc\":\"2.0\",\"id\":" + id + ",\"error\":{\"code\":" + std::to_string(code)
            + ",\"message\":" + jsonString(message) + "}}";
    }
    //=============================================================================
    std::string
    toolContentResponse(const std::string& id, const ToolResult& result)
    {
        std::string text = "{";
        text += "\"success\":" + std::string(result.success ? "true" : "false");
        text += ",\"output\":" + jsonString(result.text);
        text += ",\"error\":" + jsonString(result.error);
        text += ",\"image\":" + jsonString(result.image);
        text += ",\"duration\":" + std::to_string(result.duration);
        text += ",\"output_truncated\":" + std::string(result.outputTruncated ? "true" : "false");
        text += ",\"error_truncated\":" + std::string(result.errorTruncated ? "true" : "false");
        if (!result.dataJson.empty()) {
            text += ",\"data\":" + result.dataJson;
        }
        text += "}";
        return successResponse(id,
            "{\"content\":[{\"type\":\"text\",\"text\":" + jsonString(text)
                + "}],\"structuredContent\":" + text
                + ",\"isError\":" + std::string(result.success ? "false" : "true") + "}");
    }
    //=============================================================================
    std::vector<std::wstring>
    splitOptionString(const std::wstring& options)
    {
        std::vector<std::wstring> parts;
        std::wstring current;
        bool inSingleQuote = false;
        bool inDoubleQuote = false;
        bool escaped = false;
        auto pushCurrent = [&parts, &current]() {
            if (!current.empty()) {
                parts.push_back(current);
                current.clear();
            }
        };
        for (wchar_t ch : options) {
            if (escaped) {
                current.push_back(ch);
                escaped = false;
                continue;
            }
            if (ch == L'\\' && inDoubleQuote) {
                escaped = true;
                continue;
            }
            if (ch == L'\'' && !inDoubleQuote) {
                inSingleQuote = !inSingleQuote;
                continue;
            }
            if (ch == L'"' && !inSingleQuote) {
                inDoubleQuote = !inDoubleQuote;
                continue;
            }
            if (std::iswspace(ch) && !inSingleQuote && !inDoubleQuote) {
                pushCurrent();
                continue;
            }
            current.push_back(ch);
        }
        pushCurrent();
        return parts;
    }
    //=============================================================================
    void
    applyMaxOutputCharacters(McpOptions& options, const std::wstring& value)
    {
        try {
            options.maxOutputCharacters = static_cast<size_t>(std::stoull(value));
        } catch (const std::exception&) {
            options.maxOutputCharacters = MCP_DEFAULT_MAX_OUTPUT_CHARACTERS;
        }
    }
    //=============================================================================
    void
    applyOption(McpOptions& options, const std::wstring& option)
    {
        auto assign = option.find(L'=');
        std::wstring name = assign == std::wstring::npos ? option : option.substr(0, assign);
        std::wstring value = assign == std::wstring::npos ? L"true" : option.substr(assign + 1);
        if (name == L"--initial-working-folder") {
            options.initialWorkingFolder = value;
        } else if (name == L"--initialize-nelson-on-startup") {
            options.initializeNelsonOnStartup = (value == L"true" || value == L"1");
        } else if (name == L"--nelson-display-mode") {
            options.displayMode = wstring_to_utf8(value);
        } else if (name == L"--log-folder") {
            options.logFolder = value;
        } else if (name == L"--log-level") {
            options.logLevel = wstring_to_utf8(value);
        } else if (name == L"--workspace-root") {
            options.workspaceRoot = value;
        } else if (name == L"--max-output-characters") {
            applyMaxOutputCharacters(options, value);
        } else if (name == L"--allow-execution") {
            options.allowExecution = (value == L"true" || value == L"1");
        } else if (name == L"--allow-format") {
            options.allowFormat = (value == L"true" || value == L"1");
        }
    }
    //=============================================================================
    std::string
    getenvString(const char* name, const std::string& defaultValue)
    {
        const char* value = std::getenv(name);
        if (value == nullptr) {
            return defaultValue;
        }
        return value;
    }
    //=============================================================================
    std::wstring
    getenvWString(const char* name, const std::wstring& defaultValue)
    {
        const char* value = std::getenv(name);
        if (value == nullptr) {
            return defaultValue;
        }
        return utf8_to_wstring(value);
    }
    //=============================================================================
    size_t
    getenvSizeT(const char* name, size_t defaultValue)
    {
        const char* value = std::getenv(name);
        if (value == nullptr) {
            return defaultValue;
        }
        try {
            return static_cast<size_t>(std::stoull(value));
        } catch (const std::exception&) {
            return defaultValue;
        }
    }
    //=============================================================================
    McpOptions
    parseOptions(const ArrayOfVector& argIn)
    {
        McpOptions options;
        options.initialWorkingFolder
            = getenvWString("NELSON_MCP_SERVER_INITIAL_WORKING_FOLDER", L"");
        options.displayMode = getenvString("NELSON_MCP_SERVER_NELSON_DISPLAY_MODE", "adv-cli");
        options.logFolder = getenvWString("NELSON_MCP_SERVER_LOG_FOLDER", L"");
        options.logLevel = getenvString("NELSON_MCP_SERVER_LOG_LEVEL", "error");
        options.workspaceRoot = getenvWString("NELSON_MCP_SERVER_WORKSPACE_ROOT", getNelsonRoot());
        options.maxOutputCharacters = getenvSizeT(
            "NELSON_MCP_SERVER_MAX_OUTPUT_CHARACTERS", MCP_DEFAULT_MAX_OUTPUT_CHARACTERS);
        options.initializeNelsonOnStartup
            = getenvString("NELSON_MCP_SERVER_INITIALIZE_NELSON_ON_STARTUP", "false") == "true";
        options.allowExecution
            = getenvString("NELSON_MCP_SERVER_ALLOW_EXECUTION", "true") != "false";
        options.allowFormat = getenvString("NELSON_MCP_SERVER_ALLOW_FORMAT", "true") != "false";
        for (const auto& arg : argIn) {
            if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
                for (const auto& option : splitOptionString(arg.asWideString())) {
                    applyOption(options, option);
                }
            }
        }
        if (options.displayMode != "cli" && options.displayMode != "adv-cli"
            && options.displayMode != "gui") {
            options.displayMode = "adv-cli";
        }
        if (options.workspaceRoot.empty()) {
            options.workspaceRoot = getNelsonRoot();
        }
        return options;
    }
    //=============================================================================
    void
    truncateText(std::wstring& text, bool& truncated, size_t maximumCharacters)
    {
        truncated = false;
        if (maximumCharacters == 0 || text.size() <= maximumCharacters) {
            return;
        }
        if (maximumCharacters <= 16) {
            text = text.substr(0, maximumCharacters);
        } else {
            text = text.substr(0, maximumCharacters - 16);
            text += L"\n...<truncated>";
        }
        truncated = true;
    }
    //=============================================================================
    ToolResult
    applyOutputLimit(ToolResult result, const McpOptions& options)
    {
        truncateText(result.text, result.outputTruncated, options.maxOutputCharacters);
        truncateText(result.error, result.errorTruncated, options.maxOutputCharacters);
        return result;
    }
    //=============================================================================
    std::string
    toolsListJson()
    {
        std::string json = "{\"tools\":[";
        bool first = true;
        for (const auto& tool : toolDefinitions()) {
            if (!first) {
                json += ",";
            }
            first = false;
            json += "{\"name\":" + jsonString(tool.name);
            json += ",\"title\":" + jsonString(tool.title);
            json += ",\"description\":" + jsonString(tool.description);
            json += ",\"inputSchema\":" + std::string(tool.inputSchema);
            json += ",\"annotations\":{\"readOnlyHint\":"
                + std::string(tool.readOnly ? "true" : "false");
            json += ",\"destructiveHint\":" + std::string(tool.destructive ? "true" : "false");
            json += ",\"idempotentHint\":false";
            json += ",\"openWorldHint\":" + std::string(tool.openWorld ? "true" : "false") + "}}";
        }
        json += "]}";
        return json;
    }
    //=============================================================================
    std::string
    resourceJson(const char* uri, const char* name, const char* description,
        const char* mimeType = "text/markdown")
    {
        return "{\"uri\":" + jsonString(uri) + ",\"name\":" + jsonString(name) + ",\"description\":"
            + jsonString(description) + ",\"mimeType\":" + jsonString(mimeType) + "}";
    }
    //=============================================================================
    std::string
    resourcesListJson()
    {
        std::string json = "{\"resources\":[";
        json += resourceJson("guidelines://nelson-coding", "Nelson coding guidelines",
            "Local repository coding conventions for generated Nelson changes.");
        json += ","
            + resourceJson("guidelines://nelson-testing", "Nelson testing guidelines",
                "Local testing conventions and commands for Nelson agents.");
        json += ","
            + resourceJson("guidelines://nelson-plotting", "Nelson plotting guidelines",
                "Local plotting guidance for MCP agents using Nelson.");
        json += ","
            + resourceJson("guidelines://nelson-modules", "Nelson module guidelines",
                "Local Nelson module layout and packaging guidance.");
        json += ","
            + resourceJson("skills://nelson/catalog", "Nelson skill catalog",
                "Mini catalog of reusable Nelson skills for coding agents.");
        for (const auto& skill : skillDefinitions()) {
            json += "," + resourceJson(skill.uri, skill.name, skill.description);
        }
        json += "]}";
        return json;
    }
    //=============================================================================
    std::wstring
    resourceText(const std::wstring& uri)
    {
        if (uri == L"guidelines://nelson-coding") {
            return L"# Nelson Coding Guidelines\nUse simple Nelson code, prefer existing "
                   L"functions, keep generated .m files covered by tests, and keep changes scoped "
                   L"to the module being modified."
                + localDocumentExcerpt(L"Development", L"DEVELOPMENT.md")
                + localDocumentExcerpt(L"Contribution", L"CONTRIBUTING.md");
        }
        if (uri == L"guidelines://nelson-testing") {
            return L"# Nelson Testing Guidelines\nAdd tests in modules/<module>/tests and run "
                   L"test_run('<module>'). Mark graphical tests for adv-cli when needed."
                + localDocumentExcerpt(L"Building", L"BUILDING.md")
                + localDocumentExcerpt(L"Windows build", L"BUILDING_Windows.md");
        }
        if (uri == L"guidelines://nelson-plotting") {
            return L"# Nelson Plotting Guidelines\nUse nelson-adv-cli or gui mode for figures. "
                   L"Call drawnow before exporting graphics and save images with saveas when an "
                   L"agent needs to inspect results."
                + localDocumentExcerpt(
                    L"Graphics module help", L"modules/graphics/help/en_US/xml/chapter.xml")
                + localDocumentExcerpt(
                    L"Graphics IO module help", L"modules/graphics_io/help/en_US/xml/chapter.xml");
        }
        if (uri == L"guidelines://nelson-modules") {
            return L"# Nelson Module Guidelines\nNelson modules use src, builtin, functions, "
                   L"tests, help, etc, and loader.m. Keep module boundaries small and explicit."
                + localDocumentExcerpt(L"Module example", L"modules/ai/CMakeLists.txt")
                + localDocumentExcerpt(L"Module registry", L"modules/modules.m.in");
        }
        if (uri == L"skills://nelson/catalog") {
            return skillCatalogText();
        }
        return skillResourceText(uri);
    }
    //=============================================================================
    std::string
    promptsListJson()
    {
        return R"({"prompts":[{"name":"nelson-code-review","description":"Review Nelson code with repository conventions.","arguments":[{"name":"task","description":"File, module, or change to review.","required":false}]},{"name":"nelson-test-author","description":"Design focused Nelson tests for a module change.","arguments":[{"name":"module","description":"Nelson module name.","required":false},{"name":"task","description":"Feature or bug under test.","required":false}]},{"name":"nelson-plot-agent","description":"Create or debug Nelson plotting workflows through MCP.","arguments":[{"name":"task","description":"Plotting task or failure to inspect.","required":false}]}]})";
    }
    //=============================================================================
    std::wstring
    promptArgument(const ArrayOf& params, const std::string& field)
    {
        ArrayOf arguments = fieldOrEmpty(params, "arguments");
        if (arguments.isEmpty()) {
            return L"";
        }
        return fieldAsString(arguments, field);
    }
    //=============================================================================
    std::wstring
    promptText(const std::wstring& name, const ArrayOf& params)
    {
        std::wstring task = promptArgument(params, "task");
        std::wstring module = promptArgument(params, "module");
        if (name == L"nelson-code-review") {
            return L"Review the Nelson change for correctness, tests, packaging, and consistency "
                   L"with local module conventions. Focus on concrete defects first. Task: "
                + task;
        }
        if (name == L"nelson-test-author") {
            return L"Write focused Nelson tests for module " + module
                + L". Cover success, failure, and edge cases without broad unrelated refactors. "
                  L"Task: "
                + task;
        }
        if (name == L"nelson-plot-agent") {
            return L"Use Nelson graphical mode when needed, create inspectable PNG output, and "
                   L"return code plus diagnostics. Task: "
                + task;
        }
        return L"";
    }
    //=============================================================================
    std::string
    getPromptResponse(const std::string& id, const ArrayOf& params)
    {
        std::wstring name = fieldAsString(params, "name");
        std::wstring text = promptText(name, params);
        if (text.empty()) {
            return errorResponse(id, -32602, L"Unknown prompt.");
        }
        return successResponse(id,
            "{\"description\":" + jsonString(name)
                + ",\"messages\":[{\"role\":\"user\",\"content\":{\"type\":\"text\",\"text\":"
                + jsonString(text) + "}}]}");
    }
    //=============================================================================
    std::string
    readResourceResponse(const std::string& id, const ArrayOf& params)
    {
        std::wstring uri = fieldAsString(params, "uri");
        std::wstring text = resourceText(uri);
        if (text.empty()) {
            return errorResponse(id, -32602, L"Unknown resource URI.");
        }
        return successResponse(id,
            "{\"contents\":[{\"uri\":" + jsonString(uri)
                + ",\"mimeType\":\"text/markdown\",\"text\":" + jsonString(text) + "}]}");
    }
    //=============================================================================
    std::string
    initializeResponse(const std::string& id)
    {
        return successResponse(id,
            "{\"protocolVersion\":\"" + std::string(MCP_PROTOCOL_VERSION)
                + "\",\"capabilities\":{\"tools\":{},\"resources\":{},\"prompts\":{}},"
                  "\"serverInfo\":{\"name\":\""
                + std::string(MCP_SERVER_NAME) + "\",\"version\":\""
                + std::string(MCP_SERVER_VERSION) + "\"}}");
    }
    //=============================================================================
    std::string
    callTool(
        Evaluator* eval, const std::string& id, const ArrayOf& params, const McpOptions& options)
    {
        std::wstring name = fieldAsString(params, "name");
        if (name.empty()) {
            return errorResponse(id, -32602, L"Tool name is required.");
        }
        if ((name == L"evaluate_nelson_code" || name == L"run_nelson_file"
                || name == L"run_nelson_test_file" || name == L"create_nelson_plot")
            && !options.allowExecution) {
            ToolResult result;
            result.success = false;
            result.error = L"Execution tools are disabled by server policy.";
            return toolContentResponse(id, result);
        }
        if (name == L"format_nelson_file" && !options.allowFormat) {
            ToolResult result;
            result.success = false;
            result.error = L"Formatting tools are disabled by server policy.";
            return toolContentResponse(id, result);
        }
        ArrayOf arguments = fieldOrEmpty(params, "arguments");
        if (arguments.isEmpty()) {
            arguments = ArrayOf::structArray(stringVector {}, ArrayOfVector {});
        }
        ToolResult result;
        if (name == L"detect_nelson_modules") {
            result = detectNelsonModules(arguments);
        } else if (name == L"check_nelson_code") {
            result = checkNelsonCode(eval, arguments, options);
        } else if (name == L"lint_nelson_code") {
            result = lintNelsonCode(eval, arguments, options);
        } else if (name == L"check_nelson_format") {
            result = checkNelsonFormat(eval, arguments, options);
        } else if (name == L"evaluate_nelson_code") {
            result = evaluateNelsonCode(eval, arguments, options);
        } else if (name == L"run_nelson_file") {
            result = runNelsonFile(eval, arguments, options);
        } else if (name == L"run_nelson_test_file") {
            result = runNelsonTest(eval, arguments, options);
        } else if (name == L"format_nelson_file") {
            result = formatNelsonFile(eval, arguments, options);
        } else if (name == L"get_nelson_help") {
            result = getNelsonHelp(eval, arguments);
        } else if (name == L"find_nelson_symbol") {
            result = findNelsonSymbol(arguments);
        } else if (name == L"get_nelson_function_info") {
            result = getNelsonFunctionInfo(eval, arguments, options);
        } else if (name == L"get_nelson_version") {
            result = getNelsonVersion(eval, options);
        } else if (name == L"get_nelson_workspace") {
            result = getNelsonWorkspace(options);
        } else if (name == L"create_nelson_plot") {
            result = createNelsonPlot(eval, arguments, options);
        } else {
            return errorResponse(id, -32602, L"Unknown tool.");
        }
        return toolContentResponse(id, applyOutputLimit(result, options));
    }
    //=============================================================================
    std::string
    handleRequest(Evaluator* eval, const ArrayOf& request, const McpOptions& options)
    {
        std::string id = jsonId(request);
        if (!request.isStruct() || !request.isScalar()) {
            return errorResponse(id, -32600, L"Invalid JSON-RPC request.");
        }
        if (fieldAsString(request, "jsonrpc") != L"2.0") {
            return errorResponse(id, -32600, L"jsonrpc must be 2.0.");
        }
        std::wstring method = fieldAsString(request, "method");
        if (method.empty()) {
            return errorResponse(id, -32600, L"method is required.");
        }
        ArrayOf params = fieldOrEmpty(request, "params");
        if (params.isEmpty()) {
            params = ArrayOf::structArray(stringVector {}, ArrayOfVector {});
        }
        if (method == L"initialize") {
            return initializeResponse(id);
        }
        if (method == L"tools/list") {
            return successResponse(id, toolsListJson());
        }
        if (method == L"resources/list") {
            return successResponse(id, resourcesListJson());
        }
        if (method == L"resources/read") {
            return readResourceResponse(id, params);
        }
        if (method == L"prompts/list") {
            return successResponse(id, promptsListJson());
        }
        if (method == L"prompts/get") {
            return getPromptResponse(id, params);
        }
        if (method == L"tools/call") {
            return callTool(eval, id, params, options);
        }
        if (method == L"ping") {
            return successResponse(id, "{}");
        }
        return errorResponse(id, -32601, L"Method not found.");
    }
    //=============================================================================
    std::string
    handleDecodedInput(Evaluator* eval, const ArrayOf& request, const McpOptions& options)
    {
        if (request.isCell()) {
            indexType count = request.elementCount();
            if (count == 0) {
                return errorResponse("null", -32600, L"Batch request must not be empty.");
            }
            const auto* items = static_cast<const ArrayOf*>(request.dataPointer());
            std::string responses = "[";
            bool first = true;
            for (indexType k = 0; k < count; ++k) {
                if (!items[k].isStruct()) {
                    if (!first) {
                        responses += ",";
                    }
                    first = false;
                    responses += errorResponse("null", -32600, L"Invalid JSON-RPC batch item.");
                    continue;
                }
                bool isNotification = !hasField(items[k], "id");
                std::wstring method = fieldAsString(items[k], "method");
                if (isNotification && method == L"initialized") {
                    continue;
                }
                if (isNotification) {
                    continue;
                }
                if (!first) {
                    responses += ",";
                }
                first = false;
                responses += handleRequest(eval, items[k], options);
            }
            responses += "]";
            return first ? std::string() : responses;
        }
        if (!request.isStruct()) {
            return errorResponse("null", -32600, L"Invalid JSON-RPC request.");
        }
        bool isNotification = !hasField(request, "id");
        std::wstring method = fieldAsString(request, "method");
        if (isNotification && method == L"initialized") {
            return std::string();
        }
        if (isNotification) {
            return std::string();
        }
        return handleRequest(eval, request, options);
    }
    //=============================================================================
    std::string
    trimAscii(const std::string& text)
    {
        size_t first = text.find_first_not_of(" \t\r\n");
        if (first == std::string::npos) {
            return std::string();
        }
        size_t last = text.find_last_not_of(" \t\r\n");
        return text.substr(first, last - first + 1);
    }
    //=============================================================================
    std::vector<std::string>
    splitBatchObjects(const std::string& line, bool& valid)
    {
        valid = true;
        std::vector<std::string> items;
        std::string text = trimAscii(line);
        if (text.size() < 2 || text.front() != '[' || text.back() != ']') {
            valid = false;
            return items;
        }
        size_t start = std::string::npos;
        int depth = 0;
        bool inString = false;
        bool escaped = false;
        for (size_t k = 1; k + 1 < text.size(); ++k) {
            char ch = text[k];
            if (inString) {
                if (escaped) {
                    escaped = false;
                } else if (ch == '\\') {
                    escaped = true;
                } else if (ch == '"') {
                    inString = false;
                }
                continue;
            }
            if (ch == '"') {
                inString = true;
            } else if (ch == '{') {
                if (depth == 0) {
                    start = k;
                }
                ++depth;
            } else if (ch == '}') {
                --depth;
                if (depth < 0) {
                    valid = false;
                    return {};
                }
                if (depth == 0 && start != std::string::npos) {
                    items.push_back(text.substr(start, k - start + 1));
                    start = std::string::npos;
                }
            }
        }
        if (depth != 0 || inString) {
            valid = false;
            items.clear();
        }
        return items;
    }
    //=============================================================================
    std::string
    handleBatchLine(Evaluator* eval, const std::string& line, const McpOptions& options)
    {
        bool valid = true;
        std::vector<std::string> items = splitBatchObjects(line, valid);
        if (!valid) {
            return errorResponse("null", -32700, L"Invalid JSON-RPC batch.");
        }
        if (items.empty()) {
            return errorResponse("null", -32600, L"Batch request must not be empty.");
        }
        std::string responses = "[";
        bool first = true;
        for (const auto& item : items) {
            std::string errorMessage;
            ArrayOf request = jsonDecode(item, errorMessage);
            std::string response;
            if (!errorMessage.empty()) {
                response = errorResponse("null", -32700, utf8_to_wstring(errorMessage));
            } else {
                response = handleDecodedInput(eval, request, options);
            }
            if (response.empty()) {
                continue;
            }
            if (!first) {
                responses += ",";
            }
            first = false;
            responses += response;
        }
        responses += "]";
        return first ? std::string() : responses;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
mcpInfo()
{
    stringVector fieldNames { "name", "version", "protocol", "transport", "defaultDisplayMode",
        "workspaceRoot", "maxOutputCharacters", "tools", "resources", "prompts", "allowExecution",
        "allowFormat" };
    wstringVector toolNames;
    for (const auto& tool : toolDefinitions()) {
        toolNames.push_back(utf8_to_wstring(tool.name));
    }
    wstringVector resourceNames { L"guidelines://nelson-coding", L"guidelines://nelson-testing",
        L"guidelines://nelson-plotting", L"guidelines://nelson-modules",
        L"skills://nelson/catalog" };
    for (const auto& skill : skillDefinitions()) {
        resourceNames.push_back(utf8_to_wstring(skill.uri));
    }
    wstringVector promptNames { L"nelson-code-review", L"nelson-test-author",
        L"nelson-plot-agent" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(MCP_SERVER_NAME);
    values << ArrayOf::characterArray(MCP_SERVER_VERSION);
    values << ArrayOf::characterArray(MCP_PROTOCOL_VERSION);
    values << ArrayOf::characterArray("stdio");
    values << ArrayOf::characterArray("adv-cli");
    values << ArrayOf::characterArray(getNelsonRoot());
    values << ArrayOf::doubleScalar(MCP_DEFAULT_MAX_OUTPUT_CHARACTERS);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(toolNames);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(resourceNames);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(promptNames);
    values << ArrayOf::logicalScalar(true);
    values << ArrayOf::logicalScalar(true);
    return ArrayOf::structScalar(fieldNames, values);
}
//=============================================================================
void
runMcpServer(Evaluator* eval, const ArrayOfVector& argIn)
{
    McpOptions options = parseOptions(argIn);
    if (!options.initialWorkingFolder.empty()) {
        evaluate(eval, L"cd(" + nelsonStringLiteral(options.initialWorkingFolder) + L")");
    }
    if (options.initializeNelsonOnStartup) {
        evaluate(eval, L"version;");
    }
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) {
            continue;
        }
        if (trimAscii(line).rfind("[", 0) == 0) {
            std::string response = handleBatchLine(eval, line, options);
            if (!response.empty()) {
                std::cout << response << std::endl;
            }
            continue;
        }
        std::string errorMessage;
        ArrayOf request = jsonDecode(line, errorMessage);
        if (!errorMessage.empty()) {
            std::cout << errorResponse("null", -32700, utf8_to_wstring(errorMessage)) << std::endl;
            continue;
        }
        try {
            std::string response = handleDecodedInput(eval, request, options);
            if (!response.empty()) {
                std::cout << response << std::endl;
            }
        } catch (const NelsonException& e) {
            std::cout << errorResponse(jsonId(request), -32000, e.message()) << std::endl;
        } catch (const std::exception& e) {
            std::cout << errorResponse(jsonId(request), -32000, utf8_to_wstring(e.what()))
                      << std::endl;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
