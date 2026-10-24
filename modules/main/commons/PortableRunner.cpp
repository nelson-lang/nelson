//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PortableRunner.hpp"
#include <filesystem>
#include <iostream>
#include <optional>
#include <string>
#include <vector>
#include "PortableEngine.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace {
struct Options
{
    std::optional<std::string> command;
    std::optional<std::string> file;
    std::wstring language;
    std::vector<std::wstring> arguments;
    bool help = false;
    bool version = false;
    std::string error;
};
//=============================================================================
std::string
quoteString(const std::string& value)
{
    std::string quoted = value;
    size_t position = 0;
    while ((position = quoted.find('\'', position)) != std::string::npos) {
        quoted.insert(position, 1, '\'');
        position += 2;
    }
    return "'" + quoted + "'";
}
//=============================================================================
std::optional<std::string>
inlineValue(const std::string& argument, const std::string& option)
{
    const std::string prefix = option + "=";
    if (argument.rfind(prefix, 0) == 0) {
        return argument.substr(prefix.size());
    }
    return std::nullopt;
}
//=============================================================================
Nelson::PortableResources
installedResources(const std::string& executableArgument)
{
    namespace fs = std::filesystem;
    std::error_code error;
    fs::path executable = fs::absolute(fs::path(executableArgument), error);
    if (error) {
        return {};
    }
    executable = fs::weakly_canonical(executable, error);
    if (error) {
        return {};
    }
    const fs::path binaryDirectory = executable.parent_path();
    const fs::path prefix = binaryDirectory.parent_path();
    const fs::path rootDirectory
        = prefix / "share" / fs::path(Nelson::wstring_to_utf8(NELSON_PROJECT_NAME));
    const fs::path modulesDirectory = rootDirectory / "modules";
    if (!fs::is_regular_file(modulesDirectory / "modules.m", error) || error) {
        return {};
    }
    fs::path libraryDirectory = prefix / fs::path(Nelson::wstring_to_utf8(NLS_LIBRARY_PATH_PREFIX));
    if (!fs::is_directory(libraryDirectory, error) || error) {
        libraryDirectory = binaryDirectory;
    }
    return { rootDirectory.generic_wstring(), modulesDirectory.generic_wstring(),
        libraryDirectory.generic_wstring(), binaryDirectory.generic_wstring() };
}
//=============================================================================
Options
parse(const std::vector<std::string>& arguments, const std::vector<std::wstring>& wideArguments)
{
    Options options;
    options.arguments = wideArguments;
    for (size_t index = 1; index < arguments.size(); ++index) {
        const std::string& argument = arguments[index];
        auto takeValue = [&](const std::string& name) -> std::optional<std::string> {
            if (index + 1 >= arguments.size()) {
                options.error = "Missing value for " + name + ".";
                return std::nullopt;
            }
            return arguments[++index];
        };
        if (argument == "--execute" || argument == "-e") {
            options.command = takeValue(argument);
        } else if (auto value = inlineValue(argument, "--execute")) {
            options.command = *value;
        } else if (argument == "--file" || argument == "-f") {
            options.file = takeValue(argument);
        } else if (auto value = inlineValue(argument, "--file")) {
            options.file = *value;
        } else if (argument == "--language") {
            const auto value = takeValue(argument);
            if (value) {
                options.language = Nelson::utf8_to_wstring(*value);
            }
        } else if (auto value = inlineValue(argument, "--language")) {
            options.language = Nelson::utf8_to_wstring(*value);
        } else if (argument == "--help" || argument == "-h") {
            options.help = true;
        } else if (argument == "--version" || argument == "-v") {
            options.version = true;
        } else if (argument == "--no-startup" || argument == "--no-user-modules"
            || argument == "--no-ipc" || argument == "--quiet") {
            continue;
        } else if (argument == "--") {
            break;
        } else {
            options.error = "Unsupported portable-runtime option: " + argument;
            break;
        }
        if (!options.error.empty()) {
            break;
        }
    }
    if (options.command && options.file) {
        options.error = "The --execute and --file options are mutually exclusive.";
    }
    return options;
}
//=============================================================================
void
displayHelp()
{
    std::cout << "Nelson portable runtime options:\n"
              << "  -e, --execute CODE   evaluate CODE\n"
              << "  -f, --file FILE      execute FILE\n"
              << "  -h, --help           show this help\n"
              << "  -v, --version        show the version\n";
}
//=============================================================================
int
run(const std::vector<std::string>& arguments, const std::vector<std::wstring>& wideArguments)
{
    const Options options = parse(arguments, wideArguments);
    if (!options.error.empty()) {
        std::cerr << options.error << '\n';
        return 2;
    }
    if (options.help) {
        displayHelp();
        return 0;
    }
    if (options.version) {
        std::cout << NELSON_PRODUCT_NAME << " version: \"" << NELSON_VERSION_STRING << "\"\n";
        return 0;
    }
    if (!options.command && !options.file) {
        std::cerr
            << "Interactive input is unavailable in the portable CLI. Use --execute or --file.\n";
        return 2;
    }

    Nelson::PortableConfiguration configuration;
    configuration.language = options.language;
    configuration.commandLineArguments = options.arguments;
    configuration.resources = installedResources(arguments.front());
    configuration.stdoutCallback = [](std::string_view text) { std::cout << text; };
    configuration.stderrCallback = [](std::string_view text) { std::cerr << text; };
    Nelson::PortableEngine engine;
    if (!engine.start(configuration)) {
        std::cerr << "Unable to start the Nelson portable engine.\n";
        return 1;
    }
    const std::string command
        = options.command ? *options.command : "run(" + quoteString(*options.file) + ")";
    const Nelson::EvaluationResult result = engine.evaluate(command);
    engine.stop();
    return result.succeeded ? 0 : 1;
}
//=============================================================================
} // namespace
//=============================================================================
int
runPortableNelson(int argc, char* argv[])
{
    std::vector<std::string> arguments;
    std::vector<std::wstring> wideArguments;
    for (int index = 0; index < argc; ++index) {
        arguments.emplace_back(argv[index]);
        wideArguments.emplace_back(Nelson::utf8_to_wstring(argv[index]));
    }
    return run(arguments, wideArguments);
}
#ifdef _MSC_VER
int
runPortableNelson(int argc, wchar_t* argv[])
{
    std::vector<std::string> arguments;
    std::vector<std::wstring> wideArguments;
    for (int index = 0; index < argc; ++index) {
        wideArguments.emplace_back(argv[index]);
        arguments.emplace_back(Nelson::wstring_to_utf8(argv[index]));
    }
    return run(arguments, wideArguments);
}
#endif
//=============================================================================
