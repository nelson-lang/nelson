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
#include "ApplicationInputs.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BytecodeArtifact.hpp"
#include "ClassdefParser.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "MxGetExtension.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include "picoSHA2/picosha2.h"
#include <algorithm>
#include <cctype>
#include <deque>
#include <exception>
#include <map>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    using Json = nlohmann::json;
    namespace fs = std::filesystem;
    using Application::fileKey;
    using Application::within;
    struct SourceAnalysis
    {
        std::string digest;
        std::vector<Application::Reference> directives;
        Application::SourceDependencies dependencies;
        std::exception_ptr failure;
    };
    //=============================================================================
    struct DirectiveState
    {
        std::set<std::string> forcedFiles;
        std::set<std::string> excludedFiles;
        std::set<std::string> excludedNames;
        std::map<std::pair<std::string, std::string>, Json> exclusions;
        std::map<std::string, SourceAnalysis> sources;
        Application::PackageSelection packages;
    };
    //=============================================================================
    bool
    nativeFile(const fs::path& path)
    {
        auto extension = path.extension().u8string();
        std::transform(extension.begin(), extension.end(), extension.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return extension == ".dll" || extension == ".dylib" || extension == ".so"
            || path.filename().u8string().find(".so.") != std::string::npos
            || extension == "." + wstring_to_utf8(getMexExtension());
    }
    //=============================================================================
    fs::path
    functionPath(const fs::path& root, const std::string& name)
    {
        fs::path result = root;
        size_t begin = 0;
        size_t dot = name.find('.');
        while (dot != std::string::npos) {
            result /= fs::u8path("+" + name.substr(begin, dot - begin));
            begin = dot + 1;
            dot = name.find('.', begin);
        }
        result /= fs::u8path(name.substr(begin) + "." + wstring_to_utf8(getMexExtension()));
        if (fs::is_regular_file(result)) {
            return result;
        }
        result.replace_extension(".m");
        if (fs::is_regular_file(result)) {
            return result;
        }
        result.replace_extension(".nbc");
        if (fs::is_regular_file(result)) {
            return result;
        }
        const auto leaf = name.substr(begin);
        result = result.parent_path() / fs::u8path("@" + leaf) / fs::u8path(leaf + ".m");
        if (fs::is_regular_file(result)) {
            return result;
        }
        result.replace_extension(".nbc");
        return fs::is_regular_file(result) ? result : fs::path();
    }
    //=============================================================================
    std::string
    owningModule(const fs::path& path, const fs::path& modulesRoot)
    {
        const auto relative = path.lexically_relative(modulesRoot);
        if (relative.empty() || *relative.begin() == "..") {
            return {};
        }
        return relative.begin()->u8string();
    }
    //=============================================================================
    class LinkPlan
    {
    public:
        LinkPlan(const fs::path& entry, DirectiveState& state,
            const Application::InputSelection& selected, bool detectData)
            : applicationRoot(Application::functionSearchRoot(entry.parent_path()))
            , modulesRoot(fs::weakly_canonical(
                  fs::u8path(
                      wstring_to_utf8(NelsonConfiguration::getInstance()->getNelsonRootDirectory()))
                  / "modules"))
            , directives(state)
            , inputs(selected)
            , autoDetectDataFiles(detectData)
        {
            document = { { "format", 1 }, { "runtimeVersion", NELSON_VERSION_STRING },
                { "engineFingerprint", bytecodeEngineFingerprint() },
                { "entry", entry.generic_u8string() },
                { "applicationRoot", applicationRoot.generic_u8string() },
                { "files", Json::array() }, { "edges", Json::array() },
                { "diagnostics", Json::array() }, { "dynamicFallback", false },
                { "autoDetectDataFiles", autoDetectDataFiles } };
            for (auto* definition : BuiltInFunctionDefManager::getInstance()->getTable()) {
                auto* builtin = static_cast<BuiltInFunctionDef*>(definition);
                builtins.emplace(builtin->getName(), builtin);
            }
        }

        void
        retain(const fs::path& source, const std::string& reason, bool resource = false)
        {
            const auto path = fs::canonical(source);
            const auto key = path.generic_u8string();
            if (excludedPackage(path)) {
                Application::require(reason != "entry" && reason != "explicit",
                    "An explicit application input belongs to an excluded support package.");
                return;
            }
            if (excludedFile(path)) {
                return;
            }
            if (files.count(key) != 0) {
                return;
            }
            Application::require(files.size() < 10000, "Application file count limit exceeded.");
            Application::require(
                fs::is_regular_file(path), "Application dependency must be a file.");
            const auto module
                = within(path, applicationRoot) || directives.forcedFiles.count(fileKey(path)) != 0
                ? ""
                : owningModule(path, modulesRoot);
            const auto root = Application::inputRoot(path, applicationRoot, inputs);
            std::string representation = resource ? "resource" : "bytecode";
            if (nativeFile(path)) {
                representation = "native";
                retainModule("dynamic_link", false);
                if (path.extension() == fs::path(L"." + getMexExtension())) {
                    retainModule("mex", false);
                }
                retainModule(module, false);
            } else if (!resource && !module.empty()) {
                representation = "runtime-source";
                retainModule(module, false);
            } else if (!resource && path.extension() == ".nbc") {
                representation = "bytecode";
            }
            files.emplace(key,
                Json { { "source", key }, { "sha256", Application::digestFile(path) },
                    { "bytes", fs::file_size(path) }, { "reason", reason }, { "module", module },
                    { "representation", representation }, { "root", root.generic_u8string() },
                    { "relative", path.lexically_relative(root).generic_u8string() } });
            if (!resource && (path.extension() == ".m" || path.extension() == ".nbc")) {
                pending.push_back(path);
            }
        }

        void
        analyze()
        {
            while (!pending.empty()) {
                auto path = pending.front();
                pending.pop_front();
                if (excludedFile(path)) {
                    continue;
                }
                auto& source = sourceAnalysis(path);
                for (const auto& directive : source.directives) {
                    if (directive.kind == "pragma-exclude") {
                        registerExclusion(path, directive);
                    }
                }
                const auto& dependencies = source.dependencies;
                if (source.failure != nullptr) {
                    failures.push_back(source.failure);
                }
                if (path.generic_u8string() == document["entry"]) {
                    document["entryKind"] = dependencies.script ? "script" : "function";
                }
                if (dependencies.sourceRequired) {
                    auto& file = files[path.generic_u8string()];
                    if (file["module"] == "") {
                        file["representation"] = "source";
                    }
                }
                for (const auto& method : dependencies.methods) {
                    methods[method].insert(path.generic_u8string());
                }
                for (const auto& reference : dependencies.references) {
                    resolveChecked(path, reference);
                }
                for (const auto& directive : source.directives) {
                    if (directive.kind == "pragma-function") {
                        if (dependencies.definitions.count(directive.name) != 0) {
                            document["edges"].push_back({ { "from", path.generic_u8string() },
                                { "to", path.generic_u8string() }, { "name", directive.name },
                                { "kind", "pragma-local" }, { "line", directive.line } });
                        } else {
                            resolveChecked(path, directive);
                        }
                    }
                }
            }
        }

        std::string
        finish()
        {
            if (!failures.empty()) {
                std::rethrow_exception(failures.front());
            }
            if (!directives.exclusions.empty()) {
                document["exclusions"] = Json::array();
                for (const auto& item : directives.exclusions) {
                    document["exclusions"].push_back(item.second);
                }
            }
            finishMethodReferences();
            if (document["dynamicFallback"].get<bool>()) {
                for (const auto& item : builtins) {
                    const auto module = wstring_to_utf8(item.second->moduleName);
                    if (module != "compiler" && !excludedModule(module)) {
                        retainModule(module, true);
                    }
                }
            }
            for (const auto& item : files) {
                document["files"].push_back(item.second);
            }
            Application::finishInputPaths(document, applicationRoot, inputs);
            document["modules"] = Json::array();
            std::set<std::string> graphicsModules;
            for (const auto& item : modules) {
                document["modules"].push_back(
                    { { "name", item.first }, { "complete", item.second } });
                for (const char* graphical :
                    { "gui", "graphics", "graphics_io", "qml_engine", "text_editor" }) {
                    if (item.first == graphical) {
                        graphicsModules.insert(item.first);
                    }
                }
            }
            document["requiresGraphics"] = !graphicsModules.empty();
            document["graphicsModules"] = graphicsModules;
            Json catalog = Json::array();
            for (const auto& item : builtins) {
                const auto* builtin = item.second;
                if (modules.count(wstring_to_utf8(builtin->moduleName)) != 0) {
                    catalog.push_back({ { "name", item.first },
                        { "module", wstring_to_utf8(builtin->moduleName) },
                        { "inputs", builtin->argCount }, { "outputs", builtin->retCount },
                        { "prototype", builtin->builtinPrototype } });
                }
            }
            document["builtinCatalog"] = catalog;
            document["builtinCatalogDigest"] = picosha2::hash256_hex_string(catalog.dump());
            document["complete"] = document["diagnostics"].empty();
            return document.dump(2);
        }

    private:
        bool
        excludedModule(const std::string& module) const
        {
            return directives.packages.excludedModules.count(module) != 0;
        }

        bool
        excludedPackage(const fs::path& path) const
        {
            if (directives.packages.excludedRoots.empty() || path.empty()) {
                return false;
            }
            const auto canonical = fs::u8path(fileKey(path));
            return std::any_of(directives.packages.excludedRoots.begin(),
                directives.packages.excludedRoots.end(),
                [&](const fs::path& root) { return within(canonical, root); });
        }

        bool
        excludedFile(const fs::path& path) const
        {
            if (directives.excludedFiles.empty()) {
                return false;
            }
            const auto key = fileKey(path);
            return directives.forcedFiles.count(key) == 0
                && directives.excludedFiles.count(key) != 0;
        }

        SourceAnalysis&
        sourceAnalysis(const fs::path& path)
        {
            const auto key = path.generic_u8string();
            auto& source = directives.sources[key];
            const auto digest = files.at(key).at("sha256").get<std::string>();
            if (source.digest != digest) {
                source = SourceAnalysis {};
                source.digest = digest;
                try {
                    source.directives = Application::readSourceDirectives(path);
                    source.dependencies = Application::analyzeSource(path);
                } catch (...) {
                    source.failure = std::current_exception();
                }
            }
            return source;
        }

        void
        registerExclusion(const fs::path& caller, const Application::Reference& reference)
        {
            const auto key = std::make_pair(caller.generic_u8string(), reference.name);
            if (directives.exclusions.count(key) != 0) {
                return;
            }
            Application::require(directives.exclusions.size() < 10000,
                "Application exclusion count limit exceeded.");
            directives.excludedNames.insert(reference.name);
            auto resolved = resolveDirectiveFile(caller, reference.name);
            if (resolved.empty()) {
                resolved = resolveCallable(caller, reference.name);
            }
            auto path = fs::u8path(reference.name);
            if (resolved.empty() && (path.extension() == ".m" || path.extension() == ".nbc")) {
                const auto name = path.replace_extension().generic_u8string();
                directives.excludedNames.insert(name);
                resolved = resolveCallable(caller, name);
            }
            Json record { { "from", caller.generic_u8string() }, { "name", reference.name },
                { "kind", reference.kind }, { "line", reference.line }, { "to", "" },
                { "explicitOverride", false } };
            if (!resolved.empty()) {
                directives.excludedFiles.insert(fileKey(resolved));
                record["to"] = fs::canonical(resolved).generic_u8string();
                record["explicitOverride"] = directives.forcedFiles.count(fileKey(resolved)) != 0;
            }
            directives.exclusions.emplace(key, std::move(record));
        }

        bool
        excludedReference(const Application::Reference& reference, const fs::path& resolved) const
        {
            if (directives.excludedNames.empty() && directives.excludedFiles.empty()) {
                return false;
            }
            if (!resolved.empty() && directives.forcedFiles.count(fileKey(resolved)) != 0) {
                return false;
            }
            return directives.excludedNames.count(reference.name) != 0
                || (!resolved.empty() && excludedFile(resolved));
        }

        void
        resolveChecked(const fs::path& caller, const Application::Reference& reference)
        {
            try {
                resolve(caller, reference, reference.imports);
            } catch (...) {
                failures.push_back(std::current_exception());
            }
        }

        void
        finishMethodReferences()
        {
            // Without type inference, retain every reachable class's candidate
            // for a function-form method call. All of their bodies are analyzed.
            for (auto& edge : document["edges"]) {
                if (!edge.value("unresolved", false)) {
                    continue;
                }
                const auto found = methods.find(edge["name"].get<std::string>());
                if (edge["kind"] == "call" && found != methods.end()) {
                    edge.erase("unresolved");
                    edge["kind"] = "method-dispatch";
                    edge["candidates"] = found->second;
                } else {
                    document["diagnostics"].push_back(edge);
                }
            }
        }

        void
        retainModule(const std::string& module, bool complete)
        {
            Application::require(module != "compiler",
                "The build-only compiler module is unavailable in deployed applications.");
            if (!module.empty()) {
                modules[module] = modules[module] || complete;
            }
        }

        void
        fallback(const fs::path& caller, const Application::Reference& reference)
        {
            document["dynamicFallback"] = true;
            document["edges"].push_back(
                { { "from", caller.generic_u8string() }, { "name", reference.name },
                    { "kind", "dynamic-fallback" }, { "line", reference.line } });
            if (fallbackExpanded) {
                return;
            }
            fallbackExpanded = true;
            for (fs::recursive_directory_iterator it(applicationRoot), end; it != end; ++it) {
                const auto name = it->path().filename().u8string();
                if (it->is_symlink() || excludedPackage(it->path()) || name == ".git"
                    || name == "temp" || name == "build") {
                    if (it->is_directory()) {
                        it.disable_recursion_pending();
                    }
                    continue;
                }
                if (it->is_regular_file()
                    && (it->path().extension() == ".m"
                        || it->path().extension() == fs::path(L"." + getMexExtension()))) {
                    retain(it->path(), "dynamic-project-fallback");
                }
            }
            retainDynamicSearchPath();
        }

        void
        retainDynamicSearchPath()
        {
            for (const auto& directory :
                PathFunctionIndexerManager::getInstance()->getPathNameVector()) {
                const auto root = fs::weakly_canonical(fs::u8path(wstring_to_utf8(directory)));
                if (!fs::is_directory(root) || !owningModule(root, modulesRoot).empty()
                    || root == applicationRoot || excludedPackage(root)) {
                    continue;
                }
                for (fs::recursive_directory_iterator it(root), end; it != end; ++it) {
                    if (it->is_symlink() || excludedPackage(it->path())) {
                        it.disable_recursion_pending();
                        continue;
                    }
                    if (it->is_directory()) {
                        const auto name = it->path().filename().u8string();
                        if (name != "private"
                            && (name.empty() || (name[0] != '+' && name[0] != '@'))) {
                            it.disable_recursion_pending();
                        }
                    } else if (it->path().extension() == ".m" || it->path().extension() == ".nbc"
                        || it->path().extension() == fs::path(L"." + getMexExtension())) {
                        retain(it->path(), "dynamic-search-path-fallback");
                    }
                }
            }
        }

        fs::path
        resolveFunction(const fs::path& caller, const std::string& name)
        {
            auto path = functionPath(applicationRoot, name);
            if (!path.empty()) {
                return path;
            }
            std::wstring indexed;
            if (PathFunctionIndexerManager::getInstance()->find(name, indexed)) {
                return fs::u8path(wstring_to_utf8(indexed));
            }
            for (const auto& root : inputs.lookupPaths) {
                path = functionPath(root, name);
                if (!path.empty()) {
                    return path;
                }
            }
            return functionPath(caller.parent_path() / "private", name);
        }

        fs::path
        resolveCallable(const fs::path& caller, const std::string& name)
        {
            auto path = resolveFunction(caller, name);
            if (!path.empty()) {
                return path;
            }
            const auto dot = name.find_last_of('.');
            if (dot != std::string::npos) {
                path = resolveFunction(caller, name.substr(0, dot));
                if (!path.empty() && path.extension() == ".m"
                    && classdefIsClassdefFile(utf8_to_wstring(path.u8string()))) {
                    return path;
                }
            }
            return {};
        }

        fs::path
        resolveFile(const fs::path& directory, const std::string& name, bool script,
            bool loadFilename = false)
        {
            auto path = fs::u8path(name);
            if (!path.is_absolute()) {
                path = directory / path;
            }
            if (fs::is_regular_file(path)) {
                return path;
            }
            if (path.extension().empty()) {
                // Match the generic load reader without changing other resource readers.
                const std::vector<std::string> extensions = loadFilename
                    ? std::vector<std::string> { ".nh5", ".mat" }
                    : (script ? std::vector<std::string> { ".m" } : std::vector<std::string> {});
                for (const auto& extension : extensions) {
                    path.replace_extension(extension);
                    if (fs::is_regular_file(path)) {
                        return path;
                    }
                }
            }
            return {};
        }

        fs::path
        resolveIncludedFile(const std::string& name, bool script, bool loadFilename = false)
        {
            for (const auto& root : inputs.lookupPaths) {
                const auto file = resolveFile(root, name, script, loadFilename);
                if (!file.empty()) {
                    return file;
                }
            }
            return {};
        }

        fs::path
        resolveDirectiveFile(const fs::path& caller, const std::string& name)
        {
            auto resolved = resolveFile(caller.parent_path(), name, false);
            const auto relative = fs::u8path(name);
            if (!resolved.empty() || relative.is_absolute()) {
                return resolved;
            }
            auto* manager = PathFunctionIndexerManager::getInstance();
            std::vector<fs::path> roots { fs::current_path() };
            const auto userPath = manager->getUserPath();
            if (!userPath.empty()) {
                roots.push_back(fs::u8path(wstring_to_utf8(userPath)));
            }
            const auto paths = manager->getPathNameVector();
            for (const auto& path : paths) {
                roots.push_back(fs::u8path(wstring_to_utf8(path)));
            }
            for (const auto& root : roots) {
                const auto path = root / relative;
                if (fs::is_regular_file(path)) {
                    return path;
                }
            }
            return resolveIncludedFile(name, false);
        }

        void
        resolve(const fs::path& caller, const Application::Reference& reference,
            const std::vector<std::string>& imports)
        {
            if (!autoDetectDataFiles && reference.kind == "resource") {
                document["edges"].push_back({ { "from", caller.generic_u8string() },
                    { "name", reference.name }, { "kind", "ignored-data" },
                    { "referenceKind", reference.kind }, { "line", reference.line } });
                return;
            }
            if (reference.kind == "dynamic") {
                if (directives.excludedNames.count(reference.name) == 0) {
                    fallback(caller, reference);
                }
                return;
            }
            bool resource = reference.kind == "resource" || reference.kind == "native-library";
            const bool fileReference
                = resource || reference.kind == "script" || reference.kind == "method-file";
            fs::path resolved;
            std::string name = reference.name;
            if (fileReference) {
                resolved
                    = resolveFile(caller.parent_path(), name, !resource, reference.loadFilename);
                if (resolved.empty()) {
                    resolved = resolveIncludedFile(name, !resource, reference.loadFilename);
                }
            } else if (reference.kind != "builtin") {
                for (const auto& spec : imports) {
                    const auto dot = spec.find_last_of('.');
                    const auto leaf = spec.substr(dot == std::string::npos ? 0 : dot + 1);
                    const auto candidate
                        = leaf == "*" ? spec.substr(0, spec.size() - 1) + name : spec;
                    if (leaf == "*" || leaf == name) {
                        resolved = resolveCallable(caller, candidate);
                        if (!resolved.empty()) {
                            name = candidate;
                            break;
                        }
                    }
                }
                if (resolved.empty()) {
                    resolved = resolveCallable(caller, name);
                }
            }
            if (reference.kind == "pragma-function" && resolved.empty()) {
                resolved = resolveDirectiveFile(caller, name);
                if (!resolved.empty()) {
                    resource = resolved.extension() != ".m" && resolved.extension() != ".nbc"
                        && !nativeFile(resolved);
                }
            }
            Json edge { { "from", caller.generic_u8string() }, { "name", name },
                { "kind", reference.kind }, { "line", reference.line } };
            const auto builtin = builtins.find(name);
            if (excludedPackage(resolved)
                || (resolved.empty() && builtin != builtins.end()
                    && excludedModule(wstring_to_utf8(builtin->second->moduleName)))) {
                edge["kind"] = "excluded-support-package";
                edge["referenceKind"] = reference.kind;
                if (!resolved.empty()) {
                    edge["to"] = fs::canonical(resolved).generic_u8string();
                }
                document["edges"].push_back(std::move(edge));
                return;
            }
            if (excludedReference(reference, resolved)) {
                edge["kind"] = "excluded";
                edge["referenceKind"] = reference.kind;
                if (!resolved.empty()) {
                    edge["to"] = fs::canonical(resolved).generic_u8string();
                }
                document["edges"].push_back(std::move(edge));
                return;
            }
            if (reference.kind == "callback") {
                const auto found = builtins.find(name);
                if (resolved.empty() && found != builtins.end()) {
                    const auto module = wstring_to_utf8(found->second->moduleName);
                    if (module == "graphics" || module == "time" || module == "audio"
                        || module == "handle" || module == "gui" || module == "qml_engine") {
                        fallback(caller, reference);
                    }
                }
                return;
            }
            if (!resolved.empty()) {
                retain(resolved, name, resource);
                edge["to"] = fs::canonical(resolved).generic_u8string();
            } else if (!fileReference && builtin != builtins.end()) {
                const auto* definition = builtin->second;
                const auto module = wstring_to_utf8(definition->moduleName);
                retainModule(module, false);
                edge["module"] = module;
                edge["library"] = wstring_to_utf8(definition->fileName);
                if (name == "load" && module == "stream_manager") {
                    // load dispatches to optional format readers by name inside native code.
                    for (const char* dependency :
                        { "isnh5file", "loadnh5", "ismatfile", "loadmat" }) {
                        if (builtins.count(dependency) != 0
                            || !resolveCallable(caller, dependency).empty()) {
                            resolveChecked(
                                caller, { dependency, "native-dependency", reference.line });
                        }
                    }
                }
            } else if ((reference.kind == "class" || reference.kind == "pragma-function")
                && (name == "handle" || classdefIsBuiltinClass(name))) {
                retainModule("handle", false);
                edge["module"] = "handle";
            } else if (reference.kind == "identifier" || reference.kind == "possible-call") {
                return;
            } else {
                edge["unresolved"] = true;
            }
            document["edges"].push_back(std::move(edge));
        }

        fs::path applicationRoot;
        fs::path modulesRoot;
        DirectiveState& directives;
        const Application::InputSelection& inputs;
        bool autoDetectDataFiles;
        std::vector<std::exception_ptr> failures;
        Json document;
        std::map<std::string, BuiltInFunctionDef*> builtins;
        std::map<std::string, Json> files;
        std::map<std::string, bool> modules;
        std::map<std::string, std::set<std::string>> methods;
        std::deque<fs::path> pending;
        bool fallbackExpanded = false;
    };
    //=============================================================================
} // namespace
//=============================================================================
std::string
buildApplicationLinkPlan(const std::wstring& entry, const std::vector<std::wstring>& includedFiles,
    bool autoDetectDataFiles)
{
    return buildApplicationLinkPlan(entry, includedFiles, autoDetectDataFiles, L"");
}
//=============================================================================
std::string
buildApplicationLinkPlan(const std::wstring& entry, const std::vector<std::wstring>& includedFiles,
    bool autoDetectDataFiles, const std::wstring& excludedPackages)
{
    const auto path = fs::canonical(fs::u8path(wstring_to_utf8(entry)));
    Application::require(path.extension() == ".m", "Application entry must be a source file.");
    DirectiveState directives;
    directives.packages = Application::readPackageSelection(excludedPackages);
    auto inputs = Application::expandInputs(includedFiles);
    inputs.lookupPaths.insert(inputs.lookupPaths.end(), directives.packages.lookupPaths.begin(),
        directives.packages.lookupPaths.end());
    for (const auto& directory : inputs.directories) {
        const auto canonical = fs::u8path(fileKey(directory));
        for (const auto& root : directives.packages.excludedRoots) {
            Application::require(!within(canonical, root),
                "An explicit application directory belongs to an excluded support package.");
        }
    }
    directives.forcedFiles.insert(fileKey(path));
    for (const auto& included : inputs.files) {
        directives.forcedFiles.insert(fileKey(included));
    }
    for (;;) {
        const auto revision = directives.exclusions.size();
        LinkPlan plan(path, directives, inputs, autoDetectDataFiles);
        plan.retain(path, "entry");
        for (const auto& file : inputs.files) {
            plan.retain(file, "explicit", file.extension() != ".m" && file.extension() != ".nbc");
        }
        plan.analyze();
        // New exclusions can invalidate earlier closure decisions. Recompute
        // membership, while reusing analyses whose source digests are unchanged.
        if (directives.exclusions.size() == revision) {
            return plan.finish();
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
