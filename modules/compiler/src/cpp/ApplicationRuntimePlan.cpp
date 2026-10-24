//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ApplicationLinkPlan.hpp"
#include "ApplicationDependencies.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "DynamicLibrary.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include <deque>
#include <filesystem>
#include <map>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    using Json = nlohmann::json;
    class RuntimePlan
    {
    public:
        RuntimePlan()
            : root(fs::weakly_canonical(fs::u8path(
                  wstring_to_utf8(NelsonConfiguration::getInstance()->getNelsonRootDirectory()))))
        {
            modulesRoot = fs::weakly_canonical(root / "modules");
            for (auto* definition : BuiltInFunctionDefManager::getInstance()->getTable()) {
                auto* builtin = static_cast<BuiltInFunctionDef*>(definition);
                builtins.emplace(builtin->getName(), builtin);
            }
        }

        void
        module(const std::string& name)
        {
            Application::require(name != "compiler",
                "The build-only compiler module is unavailable in deployed applications.");
            if (name.empty() || !modules.insert(name).second) {
                return;
            }
            Application::require(modules.size() <= 512, "Runtime module count limit exceeded.");
            const auto directory = root / "modules" / fs::u8path(name);
            Application::require(fs::is_directory(directory), "Missing runtime module directory.");
            for (const auto& relative : { "loader.m", "etc/startup.m", "etc/finish.m" }) {
                const auto script = directory / relative;
                if (fs::is_regular_file(script)) {
                    source(script);
                }
            }
        }

        void
        builtin(const std::string& name, bool required)
        {
            const auto found = builtins.find(name);
            if (found != builtins.end()) {
                module(wstring_to_utf8(found->second->moduleName));
            } else if (required && !macro(name)) {
                throw std::runtime_error("Missing runtime bootstrap function: " + name);
            }
        }

        bool
        macro(const std::string& name)
        {
            std::wstring resolved;
            if (!PathFunctionIndexerManager::getInstance()->find(name, resolved)) {
                return false;
            }
            const auto path = fs::canonical(fs::u8path(wstring_to_utf8(resolved)));
            const auto relative = path.lexically_relative(modulesRoot);
            if (relative.empty() || *relative.begin() == ".." || relative.is_absolute()) {
                return false;
            }
            module(relative.begin()->u8string());
            if (path.extension() == ".m") {
                source(path);
            }
            return true;
        }

        void
        source(const fs::path& filename)
        {
            const auto path = fs::canonical(filename);
            if (files.insert(path.generic_u8string()).second) {
                Application::require(files.size() <= 10000, "Runtime source count limit exceeded.");
                digests.emplace(path.generic_u8string(), Application::digestFile(path));
                pending.push_back(path);
            }
        }

        void
        analyze()
        {
            while (!pending.empty()) {
                const auto filename = pending.front();
                pending.pop_front();
                const auto dependencies = Application::analyzeSource(filename);
                Application::require(
                    Application::digestFile(filename) == digests.at(filename.generic_u8string()),
                    "Runtime bootstrap source changed during analysis.");
                for (const auto& reference : dependencies.references) {
                    if (reference.kind == "dynamic") {
                        dynamicStartup = dynamicStartup
                            || (reference.name != "addpath" && reference.name != "rmpath"
                                && reference.name != "path" && reference.name != "run"
                                && reference.name != "cd");
                        continue;
                    }
                    builtin(reference.name, false);
                    if (builtins.count(reference.name) != 0) {
                        continue;
                    }
                    macro(reference.name);
                }
            }
        }

        Json
        finish()
        {
            std::set<std::string> libraries;
            const auto directory = fs::u8path(
                wstring_to_utf8(NelsonConfiguration::getInstance()->getNelsonLibraryDirectory()));
            libraries.insert((directory / ("libnlsEngine" + get_dynamic_library_extension()))
                    .generic_u8string());
            for (const auto& item : builtins) {
                auto* definition = item.second;
                if (modules.count(wstring_to_utf8(definition->moduleName)) != 0
                    && !definition->getFilename().empty()) {
                    libraries.insert(
                        fs::u8path(wstring_to_utf8(definition->getFilename())).generic_u8string());
                }
            }
            Json inputs = Json::array();
            for (const auto& item : digests) {
                inputs.push_back({ { "source", item.first }, { "sha256", item.second } });
            }
            return Json { { "format", 1 }, { "modules", modules }, { "libraries", libraries },
                { "bootstrapFiles", files }, { "inputs", inputs },
                { "dynamicStartup", dynamicStartup } };
        }

    private:
        fs::path root;
        fs::path modulesRoot;
        std::set<std::string> modules;
        std::set<std::string> files;
        std::map<std::string, std::string> digests;
        std::map<std::string, BuiltInFunctionDef*> builtins;
        std::deque<fs::path> pending;
        bool dynamicStartup = false;
    };
    //=============================================================================
} // namespace
//=============================================================================
std::string
buildApplicationRuntimePlan(const std::wstring& application, bool gui)
{
    Application::require(
        application.size() <= 64 * 1024 * 1024, "Application link plan is too large.");
    const auto plan = Json::parse(wstring_to_utf8(application));
    Application::require(plan.is_object() && plan.contains("modules"),
        "Application link plan must contain modules.");
    auto modules = plan.at("modules");
    // A singleton structure array becomes a scalar object after a JSON round trip.
    if (modules.is_object()) {
        modules = Json::array({ modules });
    }
    Application::require(
        modules.is_array(), "Application modules must be an array or a module record.");
    Application::require(gui || !plan.value("requiresGraphics", false),
        "Graphical dependencies require a graphical application runtime.");
    RuntimePlan runtime;
    for (const auto& item : modules) {
        Application::require(
            item.is_object() && item.contains("name") && item.at("name").is_string(),
            "Each runtime module must be a record with a string name.");
        runtime.module(item.at("name").get<std::string>());
    }
    for (const char* module : { "functions_manager", "modules_manager", "core", "engine",
             "interpreter", "error_manager", "string", "types", "display_format", "console",
             "memory_manager", "operators", "overload" }) {
        runtime.module(module);
    }
    // This trusted bootstrap invokes the already analyzed entry through feval;
    // it does not widen the application's own dynamic-dependency decision.
    for (const char* name : { "argv", "tempname", "onCleanup", "rmdir", "pwd", "path",
             "__application_archive__", "fileread", "jsondecode", "__bytecode_artifact__",
             "isstruct", "isscalar", "isfield", "isequal", "error", "numel", "iscell", "isempty",
             "islogical", "ischar", "regexp", "any", "strcmp", "addpath", "fullfile", "cd",
             "fileparts", "run", "feval", "clear", "strsplit", "isrow", "startsWith", "char",
             "cellfun", "isquietmode", "__nelsonc_application_help__" }) {
        runtime.builtin(name, true);
    }
    if (gui) {
        for (const char* module : { "gui", "graphics" }) {
            runtime.module(module);
        }
        runtime.builtin("__nelsonc_wait_for_windows__", true);
    }
    if (plan.contains("nativeLibraries") && !plan.at("nativeLibraries").empty()) {
        runtime.builtin("__library_search_paths__", true);
    }
    runtime.analyze();
    return runtime.finish().dump(2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
