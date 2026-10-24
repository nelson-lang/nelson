//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RuntimeInstaller.hpp"
#include <algorithm>
#include <stdexcept>
//=============================================================================
namespace Nelson::RuntimeInstaller {
void
require(bool condition, const std::string& message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
bool
plain(const std::string& value)
{
    return !value.empty() && value.size() <= 32768
        && std::none_of(
            value.begin(), value.end(), [](unsigned char c) { return c < 32 || c == 127; });
}
//=============================================================================
void
relative(const std::string& value)
{
    require(plain(value) && value.size() <= 4096 && value.front() != '/' && value.back() != '/'
            && value.find('\\') == std::string::npos && value.find(':') == std::string::npos,
        "Invalid relative runtime path.");
    for (const auto& part : fs::u8path(value)) {
        require(!part.empty() && part != "." && part != "..", "Invalid runtime path component.");
    }
    require(value.find("//") == std::string::npos && value != "runtime.json"
            && value.compare(0, 13, "runtime.json/") != 0 && value != "modules/modules.m"
            && value.compare(0, 18, "modules/modules.m/") != 0
            && value.compare(0, 15, ".nelson-runtime") != 0,
        "Runtime input collides with installer metadata.");
}
//=============================================================================
Json
parse(const std::string& value)
{
    return Json::parse(value, [](int depth, Json::parse_event_t, Json&) {
        require(depth <= 32, "Runtime metadata nesting limit exceeded.");
        return true;
    });
}
//=============================================================================
namespace {
    std::vector<std::string>
    names(const Json& value)
    {
        require(value.is_array() && value.size() <= 512, "Invalid runtime module array.");
        std::set<std::string> seen;
        std::vector<std::string> result;
        for (const auto& item : value) {
            const auto name = item.get<std::string>();
            require(!name.empty() && name.size() <= 128
                    && name.find_first_not_of(
                           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
                        == std::string::npos
                    && seen.insert(name).second,
                "Invalid or repeated runtime module.");
            result.push_back(name);
        }
        return result;
    }
    //=============================================================================
    bool
    validDigest(const std::string& value)
    {
        return value.size() == 64
            && value.find_first_not_of("0123456789abcdef") == std::string::npos;
    }
    //=============================================================================
    void
    parents(Inventory& result, const std::string& name)
    {
        auto parent = fs::u8path(name).parent_path();
        while (!parent.empty()) {
            result.directories.insert(parent.generic_u8string());
            require(result.directories.size() <= 100000, "Runtime directory count limit exceeded.");
            parent = parent.parent_path();
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
Inventory
decode(const Json& value)
{
    require(value.is_object() && value.size() == 10 && value.at("format") == 1
            && value.at("kind") == "nelson-runtime-inventory",
        "Unsupported runtime inventory.");
    Inventory result;
    result.architecture = value.at("architecture").get<std::string>();
    result.version = value.at("runtimeVersion").get<std::string>();
    result.engine = value.at("engineFingerprint").get<std::string>();
#ifdef __APPLE__
#if defined(__aarch64__) || defined(__arm64__)
    constexpr const char* nativeArchitecture = "maca64";
    constexpr const char* nativeBinary = "bin/macOS/libnlsInterpreter.dylib";
#else
    constexpr const char* nativeArchitecture = "maci64";
    constexpr const char* nativeBinary = "bin/macOS/libnlsInterpreter.dylib";
#endif
#else
    constexpr const char* nativeArchitecture = "glnxa64";
    constexpr const char* nativeBinary = "bin/linux/libnlsInterpreter.so";
#endif
    require(result.architecture == nativeArchitecture && plain(result.version)
            && validDigest(result.engine),
        "Invalid runtime identity.");
    result.startup = names(value.at("startupOrder"));
    result.modules = names(value.at("moduleOrder"));
    const auto& flags = value.at("flags");
    require(flags.is_object() && flags.size() == 2, "Invalid runtime flags.");
    result.gui = flags.at("gui").get<bool>();
    result.dynamic = flags.at("dynamicStartup").get<bool>();
    const std::set<std::string> selected(result.modules.begin(), result.modules.end());
    require(selected.count("core") && selected.count("engine") && selected.count("interpreter")
            && !selected.count("compiler"),
        "Invalid runtime core modules.");
    std::vector<std::string> ordered;
    for (const auto& name : result.startup) {
        if (selected.count(name)) {
            ordered.push_back(name);
        }
    }
    require(ordered == result.modules
            && (!result.gui || (selected.count("graphics") && selected.count("gui"))),
        "Inconsistent runtime startup order.");
    auto files = value.at("files");
    if (files.is_object()) {
        files = Json::array({ files });
    }
    const auto& directories = value.at("directories");
    require(files.is_array() && !files.empty() && directories.is_array()
            && files.size() + directories.size() <= 100000,
        "Invalid runtime file inventory.");
    for (const auto& item : files) {
        require(item.is_object() && item.size() == 3, "Invalid runtime file record.");
        const auto name = item.at("relative").get<std::string>();
        relative(name);
        require(
            item.at("mode").is_number_integer() && item.at("mode") >= 0 && item.at("mode") <= 0777,
            "Invalid runtime permission mode.");
        File file { item.at("sha256").get<std::string>(), item.at("mode").get<unsigned>() };
        require(validDigest(file.digest) && (file.mode == 0644 || file.mode == 0755)
                && result.files.emplace(name, file).second,
            "Invalid or duplicate runtime file.");
        parents(result, name);
    }
    for (const auto& item : directories) {
        const auto name = item.get<std::string>();
        relative(name);
        result.directories.insert(name);
        parents(result, name);
    }
    result.directories.insert("modules");
    for (const auto& name : result.directories) {
        require(result.files.count(name) == 0, "Conflicting runtime file and directory.");
    }
    const auto interpreter = result.files.find(nativeBinary);
    require(interpreter != result.files.end() && interpreter->second.digest == result.engine,
        "Interpreter does not match the runtime identity.");
    require(
        encode(result).dump().size() <= 16 * 1024 * 1024, "Runtime inventory size limit exceeded.");
    return result;
}
//=============================================================================
Json
encode(const Inventory& value)
{
    auto files = Json::array();
    for (const auto& [name, file] : value.files) {
        files.push_back({ { "relative", name }, { "sha256", file.digest }, { "mode", file.mode } });
    }
    return { { "format", 1 }, { "architecture", value.architecture },
        { "runtimeVersion", value.version }, { "engineFingerprint", value.engine },
        { "startupOrder", value.startup }, { "moduleOrder", value.modules },
        { "flags", { { "gui", value.gui }, { "dynamicStartup", value.dynamic } } },
        { "files", files }, { "directories", value.directories },
        { "kind", "nelson-runtime-inventory" } };
}
//=============================================================================
Inventory
stateInventory(const Json& value, const fs::path& registration)
{
    require(value.is_object() && value.size() == 4 && value.at("format") == 1
            && value.at("registration") == registration.generic_u8string(),
        "Invalid installed runtime ownership.");
    require(
        validDigest(value.at("helperDigest").get<std::string>()), "Invalid uninstaller identity.");
    return decode(value.at("inventory"));
}
//=============================================================================
Inventory
merge(const Inventory& old, const Inventory& added)
{
    require(old.architecture == added.architecture && old.engine == added.engine
            && old.version == added.version && old.startup == added.startup,
        "Runtime identities or startup catalogs differ.");
    Inventory result = old;
    for (const auto& [name, file] : added.files) {
        const auto previous = result.files.emplace(name, file);
        require(previous.second
                || (previous.first->second.digest == file.digest
                    && previous.first->second.mode == file.mode),
            "Conflicting runtime file: " + name);
    }
    result.directories.insert(added.directories.begin(), added.directories.end());
    std::set<std::string> selected(old.modules.begin(), old.modules.end());
    selected.insert(added.modules.begin(), added.modules.end());
    result.modules.clear();
    for (const auto& name : result.startup) {
        if (selected.count(name)) {
            result.modules.push_back(name);
        }
    }
    result.gui = old.gui || added.gui;
    result.dynamic = old.dynamic || added.dynamic;
    return decode(encode(result));
}
//=============================================================================
std::string
startupText(const Inventory& inventory)
{
    std::string result = "modules_list = {\n";
    for (const auto& name : inventory.modules) {
        result += "{'" + name + "', true};\n";
    }
    return result + "};\n";
}
//=============================================================================
std::string
manifestText(const Inventory& inventory)
{
    auto value = encode(inventory);
    value.erase("startupOrder");
    value.erase("flags");
    value["kind"] = "nelson-shared-runtime";
    value["mode"] = "installed";
    value["modules"] = inventory.modules;
    value["gui"] = inventory.gui;
    value["dynamicStartup"] = inventory.dynamic;
    value["generated"]
        = { { "relative", "modules/modules.m" }, { "sha256", digest(startupText(inventory)) } };
    return value.dump() + "\n";
}
//=============================================================================
} // namespace Nelson::RuntimeInstaller
//=============================================================================
