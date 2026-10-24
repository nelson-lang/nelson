//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationDistributionPlan.hpp"
#include "ApplicationDistribution.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "ApplicationLauncherContract.hpp"
#include "ApplicationDependencies.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cwctype>
#include <filesystem>
#include <map>
#include <set>
//=============================================================================
namespace Nelson::Application::Distribution {
namespace fs = std::filesystem;
Json
parse(const std::wstring& encoded)
{
    require(encoded.size() <= 64 * 1024 * 1024, "Distribution report size limit exceeded.");
    return Json::parse(wstring_to_utf8(encoded), [](int depth, Json::parse_event_t, Json&) {
        require(depth <= 64, "Distribution report nesting limit exceeded.");
        return true;
    });
}
//=============================================================================
std::string
text(const Json& value)
{
    require(value.is_string(), "A distribution text field must be a string.");
    const auto result = value.get<std::string>();
    require(!result.empty() && result.size() <= 32768
            && std::none_of(result.begin(), result.end(), [](unsigned char c) { return c < 32; }),
        "Invalid distribution text field.");
    return result;
}
//=============================================================================
std::string
key(const std::string& value, bool windows)
{
    if (!windows) {
        return value;
    }
    auto wide = utf8_to_wstring(value);
    std::transform(wide.begin(), wide.end(), wide.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return wstring_to_utf8(wide);
}
//=============================================================================
Json
records(const Json& value)
{
    if (value.is_object()) {
        return Json::array({ value });
    }
    require(value.is_array(), "Distribution records must be an array or a scalar record.");
    require(value.size() <= 100000, "Distribution record count limit exceeded.");
    return value;
}
//=============================================================================
namespace {
    Json
    normalizeLauncher(const Json& document, const std::string& buildType)
    {
        require(document.is_object() && document.size() == 6,
            "Invalid application launcher contract in build report.");
        auto number = [&](const char* name) {
            const auto& value = document.at(name);
            require(value.is_number_integer() && value >= 0 && value <= UINT32_MAX,
                "Invalid application launcher contract version.");
            return value.get<uint32_t>();
        };
        const auto kind = text(document.at("kind"));
        require(kind == (buildType == "standaloneWindowsApplication" ? "windowed" : "console"),
            "Application launcher kind does not match the build type.");
        ApplicationLauncher::Contract value { number("format"), number("startupInterface"),
            number("archiveMinVersion"), number("archiveMaxVersion"),
            number("runtimeDiscoveryVersion"),
            kind == "console" ? ApplicationLauncher::Kind::Console
                              : ApplicationLauncher::Kind::Windowed };
        ApplicationLauncher::validate(value);
        return document;
    }
    //=============================================================================
    std::string
    relative(const Json& value)
    {
        const auto path = text(value);
        require(path.size() <= 4096 && path.back() != '/'
                && ApplicationArchive::validRelativePath(path),
            "Invalid distribution destination path.");
        return path;
    }
    //=============================================================================
    std::string
    source(const Json& value)
    {
        const auto path = fs::u8path(text(value));
        require(path.is_absolute(), "Distribution source paths must be absolute.");
        return path.lexically_normal().generic_u8string();
    }
    //=============================================================================
    std::string
    digest(const Json& value)
    {
        const auto hash = text(value);
        require(ApplicationArchive::validDigest(hash), "Invalid distribution SHA256 digest.");
        return hash;
    }
    //=============================================================================
    Json
    strings(const Json& value, bool identifiers)
    {
        require(value.is_array() && value.size() <= 100000,
            "Distribution names must be a bounded array.");
        std::set<std::string> seen;
        for (const auto& item : value) {
            const auto name = text(item);
            if (identifiers) {
                require(name.size() <= 128
                        && std::all_of(name.begin(), name.end(),
                            [](unsigned char c) {
                                return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                                    || (c >= '0' && c <= '9') || c == '_';
                            }),
                    "Invalid runtime module name.");
            }
            require(seen.insert(name).second, "Duplicate distribution name.");
        }
        return value;
    }
    //=============================================================================
    Json
    normalizeFiles(const Json& value, bool withSource)
    {
        Json result = Json::array();
        for (const auto& file : records(value)) {
            require(file.is_object(), "Distribution file must be a record.");
            Json item { { "relative", relative(file.at("relative")) },
                { "sha256", digest(file.at("sha256")) } };
            if (withSource) {
                item["source"] = source(file.at("source"));
            }
            result.push_back(std::move(item));
        }
        require(!result.empty(), "Distribution must contain files.");
        std::sort(result.begin(), result.end(),
            [](const Json& a, const Json& b) { return a.at("relative") < b.at("relative"); });
        return result;
    }
    //=============================================================================
    Json
    normalizeInputs(const Json& value)
    {
        Json result = Json::array();
        std::map<std::string, std::string> seen;
        for (const auto& file : records(value)) {
            const auto path = source(file.at("source"));
            const auto hash = digest(file.at("sha256"));
            const auto previous = seen.emplace(path, hash);
            require(previous.second || previous.first->second == hash,
                "Conflicting distribution input digests.");
            if (previous.second) {
                result.push_back({ { "source", path }, { "sha256", hash } });
            }
        }
        return result;
    }
    //=============================================================================
    Json
    normalizeRuntime(Json runtime, const std::string& architecture, const std::string& engine)
    {
        require(
            runtime.is_object() && runtime.at("format") == 1, "Invalid runtime snapshot version.");
        runtime["files"] = normalizeFiles(runtime.at("files"), true);
        runtime["inputs"] = normalizeInputs(runtime.at("inputs"));
        runtime["modules"] = strings(runtime.at("modules"), true);
        runtime["moduleOrder"] = strings(runtime.at("moduleOrder"), true);
        runtime["startupOrder"] = strings(runtime.at("startupOrder"), true);
        require(runtime["startupOrder"].size() <= 512, "Runtime module count limit exceeded.");
        runtime["directories"] = strings(runtime.at("directories"), false);
        for (const auto& folder : runtime["directories"]) {
            relative(folder);
        }
        const bool windows = architecture == "win64" || architecture == "woa64";
        validateDestinations(runtime["files"], runtime["directories"], windows);
        const auto modules = runtime["modules"].get<std::set<std::string>>();
        require(modules.count("compiler") == 0, "A deployed runtime cannot include compiler.");
        require(modules.count("core") && modules.count("engine") && modules.count("interpreter"),
            "Runtime snapshot is missing core startup modules.");
        Json ordered = Json::array();
        for (const auto& name : runtime["startupOrder"]) {
            if (modules.count(name.get<std::string>())) {
                ordered.push_back(name);
            }
        }
        require(ordered == runtime["moduleOrder"] && ordered.size() == modules.size(),
            "Runtime startup order does not match selected modules.");
        require(runtime.at("gui").is_boolean() && runtime.at("dynamicStartup").is_boolean(),
            "Runtime flags must be logical scalars.");
        require(
            !runtime.at("gui").get<bool>() || (modules.count("graphics") && modules.count("gui")),
            "A graphical runtime must include its graphical modules.");
        const auto binary = architecture == "win64" ? "x64"
            : architecture == "woa64"               ? "ARM64"
            : architecture == "glnxa64"             ? "linux"
                                                    : "macOS";
        const auto extension = windows ? ".dll" : architecture == "glnxa64" ? ".so" : ".dylib";
        const auto interpreter = std::string("bin/") + binary + "/libnlsInterpreter" + extension;
        std::map<std::string, Json> files;
        for (const auto& file : runtime["files"]) {
            files.emplace(key(file["relative"], windows), file);
        }
        const auto found = files.find(key(interpreter, windows));
        require(found != files.end() && found->second["sha256"] == engine,
            "Runtime interpreter does not match the build engine fingerprint.");
        require(
            files.count(key(std::string("bin/") + binary + "/libnlsEngine" + extension, windows)),
            "Runtime snapshot is missing the native engine library.");
        runtime["libraries"] = records(runtime.at("libraries"));
        for (auto& library : runtime["libraries"]) {
            const auto name = relative(library.at("filename"));
            require(
                name.find('/') == std::string::npos, "Native library must have a leaf filename.");
            const auto path = source(library.at("source"));
            const auto hash = digest(library.at("sha256"));
            const auto item = files.find(key(std::string("bin/") + binary + "/" + name, windows));
            require(item != files.end() && item->second["sha256"] == hash
                    && key(item->second["source"], windows) == key(path, windows),
                "Native library is not represented by its runtime file.");
            library["source"] = path;
            library["needed"] = strings(library.at("needed"), false);
            library["delayed"] = strings(library.at("delayed"), false);
        }
        runtime["systemLibraries"] = strings(runtime.at("systemLibraries"), false);
        runtime["bootstrapFiles"] = strings(runtime.at("bootstrapFiles"), false);
        for (auto& file : runtime.at("bootstrapFiles")) {
            file = source(file);
        }
        Json result;
        for (const auto* field :
            { "format", "files", "inputs", "modules", "moduleOrder", "startupOrder", "directories",
                "gui", "dynamicStartup", "libraries", "systemLibraries", "bootstrapFiles" }) {
            result[field] = runtime.at(field);
        }
        result["mode"] = "installed";
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
validateDestinations(const Json& files, const Json& directories, bool windows)
{
    std::set<std::string> occupied;
    const std::set<std::string> reserved { "runtime.json", "modules/modules.m" };
    for (const auto& file : files) {
        const auto name = key(relative(file.at("relative")), windows);
        require(reserved.count(name) == 0 && occupied.insert(name).second,
            "Conflicting distribution file destinations.");
    }
    occupied.insert(reserved.begin(), reserved.end());
    auto checkParents = [&](const std::string& name) {
        auto offset = name.find('/');
        while (offset != std::string::npos) {
            require(occupied.count(name.substr(0, offset)) == 0,
                "A distribution file cannot be a parent directory.");
            offset = name.find('/', offset + 1);
        }
    };
    for (const auto& file : files) {
        checkParents(key(file.at("relative"), windows));
    }
    for (const auto& name : reserved) {
        checkParents(name);
    }
    std::set<std::string> folders;
    for (const auto& directory : directories) {
        const auto name = key(relative(directory), windows);
        require(occupied.count(name) == 0 && folders.insert(name).second,
            "Conflicting distribution directory destinations.");
        checkParents(name);
    }
}
//=============================================================================
Json
normalizeReport(Json report)
{
    require(report.is_object() && report.at("format") == 2
            && report.at("kind") == "nelson-standalone-build",
        "Unsupported standalone build report format; rebuild the application.");
    const auto architecture = text(report.at("architecture"));
    const std::set<std::string> platforms { "win64", "woa64", "glnxa64", "maci64", "maca64" };
    require(platforms.count(architecture) != 0, "Unsupported build report architecture.");
    const auto name = text(report.at("applicationName"));
    require(ApplicationArchive::validRuntimeDirectory(name), "Invalid application name.");
    const auto type = text(report.at("buildType"));
    require(type == "standaloneApplication" || type == "standaloneWindowsApplication",
        "Unsupported distribution build type.");
    const bool windows = architecture == "win64" || architecture == "woa64";
    require(windows || type != "standaloneWindowsApplication",
        "Windowed build report requires a Windows architecture.");
    const auto executable = relative(report.at("executable"));
    auto files = normalizeFiles(report.at("files"), false);
    validateDestinations(files, Json::array(), windows);
    bool found = false;
    for (const auto& file : files) {
        require(key(file["relative"], windows) != "buildresult.json",
            "The build report cannot include itself as an application artifact.");
        found = found || file["relative"] == executable;
    }
    require(found, "The build report must include its executable artifact.");
    const auto fingerprint = digest(report.at("engineFingerprint"));
    const auto runtimeVersion = text(report.at("runtimeVersion"));
    const auto version = text(report.at("executableVersion"));
    return { { "format", 2 }, { "kind", "nelson-standalone-build" },
        { "architecture", architecture }, { "runtimeVersion", runtimeVersion },
        { "engineFingerprint", fingerprint }, { "applicationName", name },
        { "executableVersion", version }, { "buildType", type }, { "executable", executable },
        { "files", files }, { "launcher", normalizeLauncher(report.at("launcher"), type) },
        { "includedSupportPackages", strings(report.at("includedSupportPackages"), false) },
        { "runtime", normalizeRuntime(report.at("runtime"), architecture, fingerprint) } };
}
//=============================================================================
} // namespace Nelson::Application::Distribution
//=============================================================================
namespace Nelson {
std::string
normalizeApplicationBuildReport(const std::wstring& encoded)
{
    return Application::Distribution::normalizeReport(Application::Distribution::parse(encoded))
        .dump(2);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
