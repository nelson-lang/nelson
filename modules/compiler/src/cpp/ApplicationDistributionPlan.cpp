//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationDistributionPlan.hpp"
#include "ApplicationDistribution.hpp"
#include "ApplicationDependencies.hpp"
#include "ApplicationInputs.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <filesystem>
#include <map>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace D = Application::Distribution;
    using Application::require;
    using D::Json;
    class RuntimeUnion
    {
    public:
        explicit RuntimeUnion(const Json& first)
            : identity(first)
            , windows(first["architecture"] == "win64" || first["architecture"] == "woa64")
        {
        }

        void
        add(const Json& report)
        {
            for (const auto* field : { "architecture", "runtimeVersion", "engineFingerprint" }) {
                require(report.at(field) == identity.at(field),
                    "Build reports require the same architecture, runtime version and engine.");
            }
            const auto& runtime = report.at("runtime");
            require(runtime.at("startupOrder") == identity["runtime"]["startupOrder"],
                "Build reports have conflicting runtime startup ordering.");
            gui = gui || runtime.at("gui").get<bool>();
            dynamic = dynamic || runtime.at("dynamicStartup").get<bool>();
            appendStrings(modules, runtime.at("modules"));
            appendStrings(bootstrap, runtime.at("bootstrapFiles"));
            appendStrings(system, runtime.at("systemLibraries"));
            appendStrings(applications, Json::array({ report.at("applicationName") }));
            for (const auto& folder : runtime.at("directories")) {
                const auto name = D::text(folder);
                const auto previous = directories.emplace(D::key(name, windows), name);
                if (!previous.second && name < previous.first->second) {
                    previous.first->second = name;
                }
            }
            for (const auto& file : runtime.at("files")) {
                appendFile(files, file, "relative");
            }
            for (const auto& file : runtime.at("inputs")) {
                appendFile(inputs, file, "source");
            }
            for (const auto& library : runtime.at("libraries")) {
                appendFile(libraries, library, "filename");
            }
            require(files.size() + directories.size() <= 100000,
                "Merged runtime file count limit exceeded.");
        }

        Json
        finish(bool verifyFiles)
        {
            auto runtime = identity.at("runtime");
            runtime["files"] = values(files);
            runtime["inputs"] = values(inputs);
            runtime["libraries"] = values(libraries);
            runtime["directories"] = values(directories);
            runtime["modules"] = modules;
            runtime["moduleOrder"] = Json::array();
            for (const auto& module : runtime.at("startupOrder")) {
                if (modules.count(module.get<std::string>())) {
                    runtime["moduleOrder"].push_back(module);
                }
            }
            runtime["bootstrapFiles"] = bootstrap;
            runtime["systemLibraries"] = system;
            runtime["gui"] = gui;
            runtime["dynamicStartup"] = dynamic;
            runtime["mode"] = "installed";
            D::validateDestinations(runtime["files"], runtime["directories"], windows);
            if (verifyFiles) {
                verify(runtime["files"]);
                verify(runtime["inputs"]);
            }
            return { { "format", 1 }, { "kind", "nelson-runtime-distribution" },
                { "architecture", identity["architecture"] },
                { "runtimeVersion", identity["runtimeVersion"] },
                { "engineFingerprint", identity["engineFingerprint"] },
                { "applications", applications }, { "runtime", runtime } };
        }

    private:
        void
        appendFile(std::map<std::string, Json>& collection, const Json& file, const char* field)
        {
            const auto name = D::key(D::text(file.at(field)), windows);
            const auto previous = collection.emplace(name, file);
            if (previous.second) {
                return;
            }
            auto& retained = previous.first->second;
            require(retained.at("sha256") == file.at("sha256"),
                "Build reports contain conflicting file identities.");
            // Equal destination bytes may come from separate build installations.
            // Select one source deterministically, independent of report order.
            if (D::text(file.at("source")) < D::text(retained.at("source"))) {
                retained = file;
            } else if (file.at("source") == retained.at("source")
                && file.dump() < retained.dump()) {
                retained = file;
            }
        }

        static void
        appendStrings(std::set<std::string>& collection, const Json& names)
        {
            for (const auto& name : names) {
                collection.insert(D::text(name));
            }
        }

        template <class Value>
        static Json
        values(const std::map<std::string, Value>& collection)
        {
            auto result = Json::array();
            for (const auto& item : collection) {
                result.push_back(item.second);
            }
            return result;
        }

        void
        verify(const Json& collection)
        {
            for (const auto& file : collection) {
                const auto name = D::text(file.at("source"));
                const auto hash = D::text(file.at("sha256"));
                const auto previous = verified.emplace(D::key(name, windows), hash);
                require(previous.second || previous.first->second == hash,
                    "A runtime source has conflicting recorded digests.");
                if (previous.second) {
                    require(Application::digestFile(std::filesystem::u8path(name)) == hash,
                        "Runtime file changed or is missing since the build report was created.");
                }
            }
        }

        Json identity;
        bool windows;
        bool gui = false;
        bool dynamic = false;
        std::set<std::string> modules, bootstrap, system, applications;
        std::map<std::string, std::string> directories, verified;
        std::map<std::string, Json> files, inputs, libraries;
    };
    //=============================================================================
} // namespace
//=============================================================================
std::string
collectApplicationInstallerInputs(const std::wstring& encoded)
{
    namespace fs = std::filesystem;
    const auto inputs = D::parse(encoded);
    require(inputs.is_array() && inputs.size() <= 10000,
        "Installer inputs must be an array of at most 10000 paths.");
#ifdef _WIN32
    const bool windows = true;
#else
    const bool windows = false;
#endif
    std::map<std::string, Json> files, folders;
    for (const auto& input : inputs) {
        const auto name = D::text(input);
        const auto requested = fs::u8path(name);
        const auto selection = Application::expandInputs({ utf8_to_wstring(name) });
        const bool directory = fs::is_directory(requested);
        const auto base = directory ? fs::canonical(requested).parent_path() : fs::path();
        for (const auto& path : selection.files) {
            const auto relative = directory ? path.lexically_relative(base).generic_u8string()
                                            : path.filename().u8string();
            Json file = { { "source", path.generic_u8string() }, { "relative", relative },
                { "sha256", Application::digestFile(path) } };
            const auto previous = files.emplace(D::key(relative, windows), file);
            require(previous.second || previous.first->second["sha256"] == file["sha256"],
                "Installer inputs contain conflicting destination files.");
        }
        for (const auto& path : selection.directories) {
            const auto relative = path.lexically_relative(base).generic_u8string();
            folders.emplace(D::key(relative, windows),
                Json { { "source", path.generic_u8string() }, { "relative", relative } });
        }
        require(files.size() + folders.size() <= 100000, "Installer input count limit exceeded.");
    }
    Json result = { { "files", Json::array() }, { "directories", Json::array() },
        { "folders", Json::array() } };
    for (const auto& item : files) {
        result["files"].push_back(item.second);
    }
    for (const auto& item : folders) {
        result["folders"].push_back(item.second);
        result["directories"].push_back(item.second["relative"]);
    }
    D::validateDestinations(result["files"], result["directories"], windows);
    return result.dump(2);
}
//=============================================================================
std::string
mergeApplicationRuntimeReports(const std::wstring& encoded, bool verifyFiles)
{
    auto reports = D::records(D::parse(encoded));
    require(!reports.empty() && reports.size() <= 64,
        "A runtime distribution requires between one and 64 build reports.");
    for (auto& report : reports) {
        report = D::normalizeReport(std::move(report));
    }
    RuntimeUnion merged(reports.front());
    for (const auto& report : reports) {
        merged.add(report);
    }
    return merged.finish(verifyFiles).dump(2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
