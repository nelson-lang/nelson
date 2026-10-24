//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include "NelsonException.hpp"
#include "NelsonCodeIndent.hpp"
#include "Types.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
//=============================================================================
namespace {
//=============================================================================
struct FormatOptions
{
    bool includeSubfolders = false;
    bool fullFormat = true;
    bool check = false;
    int indentSize = 2;
};
//=============================================================================
struct CommandLineOptions
{
    FormatOptions options;
    bool hasIncludeSubfolders = false;
    bool hasFullFormat = false;
    bool hasIndentSize = false;
    bool hasConfigFile = false;
    std::filesystem::path configFile;
    Nelson::wstringVector names;
};
//=============================================================================
struct IgnoreGroup
{
    std::filesystem::path root;
    std::vector<std::string> patterns;
};
//=============================================================================
struct IgnoreConfiguration
{
    std::vector<IgnoreGroup> groups;
};
//=============================================================================
void
usage()
{
    std::cerr << "usage: nelson-format [--include-subfolders true|false] "
                 "[--indent-size n] [--full-format true|false] "
                 "[--config file] [--check] <path>...\n";
}
//=============================================================================
bool
readBool(const std::string& value, bool& result)
{
    if (value == "true" || value == "1") {
        result = true;
        return true;
    }
    if (value == "false" || value == "0") {
        result = false;
        return true;
    }
    return false;
}
//=============================================================================
bool
readInteger(const std::string& value, int& result)
{
    try {
        size_t parsed = 0;
        int converted = std::stoi(value, &parsed);
        if (parsed != value.size() || converted < 0) {
            return false;
        }
        result = converted;
        return true;
    } catch (...) {
        return false;
    }
}
//=============================================================================
std::string
normalizePath(const std::filesystem::path& path)
{
    std::string normalized = path.generic_string();
    std::replace(normalized.begin(), normalized.end(), '\\', '/');
    return normalized;
}
//=============================================================================
bool
readFile(const std::filesystem::path& path, std::string& content)
{
    std::ifstream stream(path, std::ios::binary);
    if (!stream.good()) {
        return false;
    }
    std::ostringstream buffer;
    buffer << stream.rdbuf();
    content = buffer.str();
    return stream.good() || stream.eof();
}
//=============================================================================
bool
writeFile(const std::filesystem::path& path, const std::string& content)
{
    std::ofstream stream(path, std::ios::binary | std::ios::trunc);
    if (!stream.good()) {
        return false;
    }
    stream << content;
    return stream.good();
}
//=============================================================================
std::filesystem::path
findConfigurationRoot(const std::vector<std::filesystem::path>& paths, const std::string& filename)
{
    std::vector<std::filesystem::path> candidates;
    for (const auto& path : paths) {
        std::filesystem::path candidate
            = std::filesystem::is_directory(path) ? path : path.parent_path();
        if (!candidate.empty()) {
            candidates.push_back(candidate);
        }
    }
    candidates.push_back(std::filesystem::current_path());
    for (const auto& candidate : candidates) {
        std::filesystem::path current = std::filesystem::absolute(candidate);
        while (!current.empty()) {
            if (std::filesystem::exists(current / filename)) {
                return current;
            }
            if (current == current.root_path()) {
                break;
            }
            current = current.parent_path();
        }
    }
    return {};
}
//=============================================================================
std::vector<std::filesystem::path>
toPaths(const Nelson::wstringVector& names)
{
    std::vector<std::filesystem::path> paths;
    paths.reserve(names.size());
    for (const std::wstring& name : names) {
        paths.push_back(std::filesystem::path(name));
    }
    return paths;
}
//=============================================================================
std::vector<std::string>
readExcludePathContains(const std::filesystem::path& path)
{
    std::string content;
    if (!readFile(path, content)) {
        throw std::runtime_error("failed to read configuration file: " + normalizePath(path));
    }
    nlohmann::json config;
    try {
        config = nlohmann::json::parse(content);
    } catch (const std::exception& e) {
        throw std::runtime_error(
            "failed to parse configuration file: " + normalizePath(path) + ": " + e.what());
    }
    if (!config.is_object()) {
        throw std::runtime_error(
            "configuration file must contain a JSON object: " + normalizePath(path));
    }
    std::vector<std::string> patterns;
    if (!config.contains("excludePathContains")) {
        return patterns;
    }
    const nlohmann::json& excludes = config["excludePathContains"];
    if (!excludes.is_array()) {
        throw std::runtime_error("excludePathContains must be an array in: " + normalizePath(path));
    }
    for (const auto& item : excludes) {
        if (!item.is_string()) {
            throw std::runtime_error(
                "excludePathContains entries must be strings in: " + normalizePath(path));
        }
        std::string pattern = item.get<std::string>();
        std::replace(pattern.begin(), pattern.end(), '\\', '/');
        if (pattern.rfind("**/", 0) == 0) {
            pattern = pattern.substr(3);
        }
        if (!pattern.empty()) {
            patterns.push_back(pattern);
        }
    }
    return patterns;
}
//=============================================================================
void
applyFormatConfiguration(
    const std::filesystem::path& path, FormatOptions& options, IgnoreGroup& formatIgnoreGroup)
{
    std::string content;
    if (!readFile(path, content)) {
        throw std::runtime_error("failed to read configuration file: " + normalizePath(path));
    }
    nlohmann::json config;
    try {
        config = nlohmann::json::parse(content);
    } catch (const std::exception& e) {
        throw std::runtime_error(
            "failed to parse configuration file: " + normalizePath(path) + ": " + e.what());
    }
    if (!config.is_object()) {
        throw std::runtime_error(
            "configuration file must contain a JSON object: " + normalizePath(path));
    }
    const std::set<std::string> knownKeys
        = { "indentSize", "fullFormat", "includeSubfolders", "excludePathContains" };
    for (const auto& item : config.items()) {
        if (knownKeys.find(item.key()) == knownKeys.end()) {
            throw std::runtime_error(
                "unknown configuration key '" + item.key() + "' in: " + normalizePath(path));
        }
    }
    if (config.contains("indentSize")) {
        if (!config["indentSize"].is_number_integer() || config["indentSize"].get<int>() < 0) {
            throw std::runtime_error(
                "indentSize must be a non-negative integer in: " + normalizePath(path));
        }
        options.indentSize = config["indentSize"].get<int>();
    }
    if (config.contains("fullFormat")) {
        if (!config["fullFormat"].is_boolean()) {
            throw std::runtime_error("fullFormat must be a boolean in: " + normalizePath(path));
        }
        options.fullFormat = config["fullFormat"].get<bool>();
    }
    if (config.contains("includeSubfolders")) {
        if (!config["includeSubfolders"].is_boolean()) {
            throw std::runtime_error(
                "includeSubfolders must be a boolean in: " + normalizePath(path));
        }
        options.includeSubfolders = config["includeSubfolders"].get<bool>();
    }
    formatIgnoreGroup.root = std::filesystem::absolute(path).parent_path();
    formatIgnoreGroup.patterns = readExcludePathContains(path);
}
//=============================================================================
IgnoreGroup
loadFormatConfiguration(const std::vector<std::filesystem::path>& paths,
    const CommandLineOptions& commandLine, FormatOptions& options)
{
    IgnoreGroup formatIgnoreGroup;
    std::filesystem::path configFile;
    if (commandLine.hasConfigFile) {
        configFile = commandLine.configFile;
        if (!std::filesystem::exists(configFile) || !std::filesystem::is_regular_file(configFile)) {
            throw std::runtime_error(
                "configuration file does not exist: " + normalizePath(configFile));
        }
    } else {
        std::filesystem::path root = findConfigurationRoot(paths, "nelson-format.json");
        if (root.empty()) {
            return formatIgnoreGroup;
        }
        configFile = root / "nelson-format.json";
    }
    applyFormatConfiguration(configFile, options, formatIgnoreGroup);
    return formatIgnoreGroup;
}
//=============================================================================
IgnoreConfiguration
loadIgnoreConfiguration(
    const std::vector<std::filesystem::path>& paths, const IgnoreGroup& formatIgnoreGroup)
{
    IgnoreConfiguration config;
    std::filesystem::path root = findConfigurationRoot(paths, "nelson-format-ignore.json");
    if (!root.empty()) {
        IgnoreGroup ignoreGroup;
        ignoreGroup.root = root;
        ignoreGroup.patterns = readExcludePathContains(root / "nelson-format-ignore.json");
        if (!ignoreGroup.patterns.empty()) {
            config.groups.push_back(ignoreGroup);
        }
    }
    if (!formatIgnoreGroup.root.empty() && !formatIgnoreGroup.patterns.empty()) {
        config.groups.push_back(formatIgnoreGroup);
    }
    return config;
}
//=============================================================================
void
applyCommandLineOverrides(const CommandLineOptions& commandLine, FormatOptions& options)
{
    if (commandLine.hasIncludeSubfolders) {
        options.includeSubfolders = commandLine.options.includeSubfolders;
    }
    if (commandLine.hasFullFormat) {
        options.fullFormat = commandLine.options.fullFormat;
    }
    if (commandLine.hasIndentSize) {
        options.indentSize = commandLine.options.indentSize;
    }
    options.check = commandLine.options.check;
}
//=============================================================================
bool
parseCommandLine(int argc, char* argv[], CommandLineOptions& commandLine)
{
    for (int k = 1; k < argc; ++k) {
        std::string arg = argv[k];
        if (arg == "--include-subfolders") {
            if (++k >= argc || !readBool(argv[k], commandLine.options.includeSubfolders)) {
                usage();
                return false;
            }
            commandLine.hasIncludeSubfolders = true;
        } else if (arg == "--indent-size") {
            if (++k >= argc || !readInteger(argv[k], commandLine.options.indentSize)) {
                usage();
                return false;
            }
            commandLine.hasIndentSize = true;
        } else if (arg == "--full-format") {
            if (++k >= argc || !readBool(argv[k], commandLine.options.fullFormat)) {
                usage();
                return false;
            }
            commandLine.hasFullFormat = true;
        } else if (arg == "--config") {
            if (++k >= argc) {
                usage();
                return false;
            }
            commandLine.configFile = std::filesystem::path(Nelson::utf8_to_wstring(argv[k]));
            commandLine.hasConfigFile = true;
        } else if (arg == "--check") {
            commandLine.options.check = true;
        } else if (!arg.empty() && arg[0] == '-') {
            usage();
            return false;
        } else {
            commandLine.names.push_back(Nelson::utf8_to_wstring(arg));
        }
    }
    if (commandLine.names.empty()) {
        usage();
        return false;
    }
    return true;
}
//=============================================================================
IgnoreConfiguration
loadConfiguration(const std::vector<std::filesystem::path>& paths,
    const CommandLineOptions& commandLine, FormatOptions& options)
{
    IgnoreGroup formatIgnoreGroup = loadFormatConfiguration(paths, commandLine, options);
    applyCommandLineOverrides(commandLine, options);
    return loadIgnoreConfiguration(paths, formatIgnoreGroup);
}
//=============================================================================
bool
isIgnored(const std::filesystem::path& file, const IgnoreConfiguration& config)
{
    if (config.groups.empty()) {
        return false;
    }
    const std::filesystem::path absolute = std::filesystem::absolute(file);
    std::string filename = file.filename().string();
    for (const IgnoreGroup& group : config.groups) {
        std::error_code errorCode;
        std::filesystem::path relativePath
            = std::filesystem::relative(absolute, group.root, errorCode);
        std::string relative = normalizePath(errorCode ? absolute : relativePath);
        for (const std::string& pattern : group.patterns) {
            if (relative.find(pattern) != std::string::npos || filename == pattern) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
isNelsonFile(const std::filesystem::path& path)
{
    return path.extension() == ".m";
}
//=============================================================================
void
collectFilesFromDirectory(const std::filesystem::path& directory, bool includeSubfolders,
    std::set<std::filesystem::path>& files)
{
    if (includeSubfolders) {
        for (const auto& entry : std::filesystem::recursive_directory_iterator(directory)) {
            if (entry.is_regular_file() && isNelsonFile(entry.path())) {
                files.insert(std::filesystem::absolute(entry.path()));
            }
        }
    } else {
        for (const auto& entry : std::filesystem::directory_iterator(directory)) {
            if (entry.is_regular_file() && isNelsonFile(entry.path())) {
                files.insert(std::filesystem::absolute(entry.path()));
            }
        }
    }
}
//=============================================================================
bool
collectFiles(const Nelson::wstringVector& names, const FormatOptions& options,
    std::vector<std::filesystem::path>& files)
{
    std::set<std::filesystem::path> uniqueFiles;
    for (const std::wstring& name : names) {
        std::filesystem::path path(name);
        if (!std::filesystem::exists(path)) {
            std::cerr << "path does not exist: " << Nelson::wstring_to_utf8(name) << "\n";
            return false;
        }
        if (std::filesystem::is_directory(path)) {
            collectFilesFromDirectory(path, options.includeSubfolders, uniqueFiles);
        } else if (std::filesystem::is_regular_file(path) && isNelsonFile(path)) {
            uniqueFiles.insert(std::filesystem::absolute(path));
        }
    }
    files.assign(uniqueFiles.begin(), uniqueFiles.end());
    return true;
}
//=============================================================================
std::string
toSingleLineMessage(const std::string& message)
{
    std::string line;
    std::istringstream stream(message);
    std::string part;
    while (std::getline(stream, part)) {
        part.erase(0, part.find_first_not_of(" \t\r"));
        part.erase(part.find_last_not_of(" \t\r") + 1);
        if (part.empty() || part == "Error:") {
            continue;
        }
        line += (line.empty() ? "" : " ") + part;
    }
    return line;
}
//=============================================================================
void
reportFileError(const std::filesystem::path& file, const std::string& message)
{
    std::cerr << normalizePath(file) << ": " << toSingleLineMessage(message) << "\n";
}
//=============================================================================
bool
formatFile(const std::filesystem::path& file, const FormatOptions& options, bool& changed)
{
    changed = false;
    std::string content;
    if (!readFile(file, content)) {
        std::cerr << "failed to read file: " << normalizePath(file) << "\n";
        return false;
    }
    std::wstring source;
    if (!Nelson::utf8_to_wstring(content, source)) {
        std::cerr << "failed to decode UTF-8 file: " << normalizePath(file) << "\n";
        return false;
    }
    std::wstring formattedSource;
    try {
        formattedSource = Nelson::smartIndentCode(
            source, options.indentSize, file.wstring(), options.fullFormat);
    } catch (const Nelson::NelsonException& e) {
        reportFileError(file, Nelson::wstring_to_utf8(e.message()));
        return false;
    } catch (const std::exception& e) {
        reportFileError(file, e.what());
        return false;
    }
    std::string formattedContent;
    if (!Nelson::wstring_to_utf8(formattedSource, formattedContent)) {
        std::cerr << "failed to encode UTF-8 file: " << normalizePath(file) << "\n";
        return false;
    }
    changed = formattedContent != content;
    if (changed && !options.check && !writeFile(file, formattedContent)) {
        std::cerr << "failed to write file: " << normalizePath(file) << "\n";
        return false;
    }
    return true;
}
//=============================================================================
} // namespace
//=============================================================================
int
main(int argc, char* argv[])
{
    CommandLineOptions commandLine;
    if (!parseCommandLine(argc, argv, commandLine)) {
        return 2;
    }

    try {
        FormatOptions options;
        const std::vector<std::filesystem::path> inputPaths = toPaths(commandLine.names);
        const IgnoreConfiguration ignoreConfiguration
            = loadConfiguration(inputPaths, commandLine, options);
        std::vector<std::filesystem::path> files;
        if (!collectFiles(commandLine.names, options, files)) {
            return 2;
        }
        bool hasChanges = false;
        bool hasFailures = false;
        for (const auto& file : files) {
            if (isIgnored(file, ignoreConfiguration)) {
                continue;
            }
            bool changed = false;
            if (!formatFile(file, options, changed)) {
                // Report and keep going: one unparsable file must not hide the others.
                hasFailures = true;
                continue;
            }
            if (changed) {
                hasChanges = true;
                std::cout << (options.check ? "would format: " : "formatted: ")
                          << normalizePath(file) << "\n";
            }
        }
        if (hasFailures) {
            return 2;
        }
        return options.check && hasChanges ? 1 : 0;
    } catch (const Nelson::NelsonException& e) {
        std::cerr << Nelson::wstring_to_utf8(e.formattedErrorMessage()) << "\n";
        return 2;
    } catch (const std::exception& e) {
        std::cerr << e.what() << "\n";
        return 2;
    } catch (...) {
        std::cerr << "internal error\n";
        return 2;
    }
}
//=============================================================================
