//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
#include "LauncherInputFile.hpp"
#include "SharedRuntimeRegistry.hpp"
#ifdef _MSC_VER
#pragma warning(push)
// The upstream SHA header fills its byte buffer with an integer zero.
#pragma warning(disable : 4244)
#endif
#include "picoSHA2/picosha2.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#include <cstdlib>
#include <set>
#include <vector>
#ifndef NELSONC_INSTALL_DATADIR
#define NELSONC_INSTALL_DATADIR "share/Nelson"
#endif
#ifndef NELSONC_INSTALL_BINDIR
#define NELSONC_INSTALL_BINDIR "bin"
#endif
#ifndef NELSONC_INSTALL_LIBDIR
#ifdef _WIN32
#define NELSONC_INSTALL_LIBDIR "bin"
#else
#define NELSONC_INSTALL_LIBDIR "lib/Nelson"
#endif
#endif
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <Windows.h>
#endif
namespace Nelson::ApplicationRuntime {
namespace fs = std::filesystem;
using ApplicationArchive::require;
struct Location
{
    fs::path root;
    fs::path libraries;
    fs::path binaries;
};
//=============================================================================
struct InstallLayout
{
    fs::path data;
    fs::path libraries;
    fs::path binaries;
};
//=============================================================================
inline const std::vector<InstallLayout>&
installLayouts()
{
    static const std::vector<InstallLayout> layouts = []() {
        std::vector<InstallLayout> result { { NELSONC_INSTALL_DATADIR, NELSONC_INSTALL_LIBDIR,
            NELSONC_INSTALL_BINDIR } };
#ifdef _WIN32
        const std::vector<InstallLayout> defaults { { "share/Nelson", "bin", "bin" },
            { "share/nelson", "bin", "bin" } };
#else
        const std::vector<InstallLayout> defaults { { "share/Nelson", "lib/Nelson", "bin" },
            { "share/Nelson", "lib64/Nelson", "bin" }, { "share/nelson", "lib/nelson", "bin" },
            { "share/nelson", "lib64/nelson", "bin" }, { "share/nelson", "lib", "bin" },
            { "share/nelson", "bin", "bin" } };
#endif
        for (const auto& layout : defaults) {
            const auto& configured = result.front();
            if (configured.data != layout.data || configured.libraries != layout.libraries
                || configured.binaries != layout.binaries) {
                result.push_back(layout);
            }
        }
        return result;
    }();
    return layouts;
}
//=============================================================================
inline fs::path
environmentPath(const char* name)
{
#ifdef _WIN32
    const std::string key(name);
    const std::wstring wideKey(key.begin(), key.end());
    const auto size = GetEnvironmentVariableW(wideKey.c_str(), nullptr, 0);
    if (size == 0) {
        return {};
    }
    std::wstring value(size, L'\0');
    const auto count = GetEnvironmentVariableW(wideKey.c_str(), value.data(), size);
    require(count < size, "Cannot read runtime environment.");
    value.resize(count);
    return fs::path(value);
#else
    const auto* value = std::getenv(name);
    return value == nullptr ? fs::path() : fs::path(value);
#endif
}
//=============================================================================
inline std::string
libraryName(const std::string& component)
{
#ifdef _WIN32
    return "libnls" + component + ".dll";
#elif defined(__APPLE__)
    return "libnls" + component + ".dylib";
#else
    return "libnls" + component + ".so";
#endif
}
//=============================================================================
inline fs::path
libraryDirectory(const fs::path& root)
{
#ifdef _WIN32
#if defined(_M_ARM64) || defined(__aarch64__)
    std::vector<fs::path> candidates { root / "bin" / "ARM64" };
#else
    std::vector<fs::path> candidates { root / "bin" / "x64" };
#endif
#elif defined(__APPLE__)
    std::vector<fs::path> candidates { root / "bin" / "macOS", root / "bin" / "macos" };
#else
    std::vector<fs::path> candidates { root / "bin" / "linux" };
#endif
    for (const auto& layout : installLayouts()) {
        candidates.push_back(root / layout.libraries);
    }
    for (const auto& candidate : candidates) {
        std::error_code error;
        if (fs::is_regular_file(candidate / libraryName("Engine"), error)) {
            return candidate;
        }
    }
    return {};
}
//=============================================================================
inline bool
hasRuntimeData(const fs::path& data)
{
    std::error_code error;
    return fs::is_regular_file(data / "modules" / "modules.m", error)
        && fs::is_regular_file(
            data / "modules" / "interpreter" / "functions" / "__nelsonc_run__.m", error);
}
//=============================================================================
inline Location
fromInstallPrefix(const fs::path& prefix, const InstallLayout& layout)
{
    const Location location { prefix / layout.data, prefix / layout.libraries,
        prefix / layout.binaries };
    std::error_code error;
    if (hasRuntimeData(location.root)
        && fs::is_regular_file(location.libraries / libraryName("Engine"), error)
        && fs::is_directory(location.binaries, error)) {
        return location;
    }
    return {};
}
//=============================================================================
inline fs::path
removeLayoutSuffix(fs::path path, const fs::path& relative)
{
    for (auto component = relative.begin(); component != relative.end(); ++component) {
        path = path.parent_path();
    }
    return path;
}
//=============================================================================
inline Location
fromRoot(const fs::path& root)
{
    if (!root.is_absolute()) {
        return {};
    }
    std::error_code error;
    const auto canonical = fs::canonical(root, error);
    if (error) {
        return {};
    }
    const bool isData = hasRuntimeData(canonical);
    if (isData) {
        const auto libraries = libraryDirectory(canonical);
        if (!libraries.empty()) {
            return { canonical, libraries, libraries };
        }
    }
    for (const auto& layout : installLayouts()) {
        const auto prefix = isData ? removeLayoutSuffix(canonical, layout.data) : canonical;
        const auto location = fromInstallPrefix(prefix, layout);
        if (!location.root.empty()
            && (!isData || fs::equivalent(location.root, canonical, error))) {
            return location;
        }
    }
    return {};
}
//=============================================================================
inline Location
fromBinaryDirectory(const fs::path& directory)
{
    if (!directory.is_absolute()) {
        return {};
    }
    std::error_code error;
    const auto binaries = fs::canonical(directory, error);
    if (error || !fs::is_directory(binaries, error)) {
        return {};
    }
    // Flat build/runtime layouts have bin/<platform>; installed layouts have known suffixes.
    const auto flat = fromRoot(binaries.parent_path().parent_path());
    if (!flat.root.empty() && fs::equivalent(flat.binaries, binaries, error)) {
        return flat;
    }
    for (const auto& layout : installLayouts()) {
        const auto prefix = removeLayoutSuffix(binaries, layout.binaries);
        const auto location = fromInstallPrefix(prefix, layout);
        if (!location.root.empty() && fs::equivalent(location.binaries, binaries, error)) {
            return location;
        }
    }
    return {};
}
//=============================================================================
inline bool
matchesEngine(const Location& location, const std::string& fingerprint)
{
    if (location.root.empty() || !ApplicationArchive::validDigest(fingerprint)) {
        return false;
    }
    auto input = LauncherInputFile::open(location.libraries / libraryName("Interpreter"));
    if (!input) {
        return false;
    }
    picosha2::hash256_one_by_one hash;
    std::array<char, 32768> block;
    size_t count;
    do {
        count = std::fread(block.data(), 1, block.size(), input.get());
        hash.process(block.begin(), block.begin() + count);
    } while (count == block.size());
    if (std::ferror(input.get())) {
        return false;
    }
    hash.finish();
    std::array<unsigned char, picosha2::k_digest_size> digest {};
    hash.get_hash_bytes(digest.begin(), digest.end());
    constexpr char hex[] = "0123456789abcdef";
    for (size_t i = 0; i < digest.size(); ++i) {
        if (hex[digest[i] >> 4] != fingerprint[2 * i]
            || hex[digest[i] & 15] != fingerprint[2 * i + 1]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
inline Location
findInstalled(const std::string& fingerprint, const fs::path& executable = {})
{
    require(ApplicationArchive::validDigest(fingerprint), "Invalid runtime engine fingerprint.");
    const auto explicitRoot = environmentPath("NELSONC_RUNTIME_ROOT");
    if (!explicitRoot.empty()) {
        require(explicitRoot.is_absolute(),
            "NELSONC_RUNTIME_ROOT must be an absolute installation root.");
        const auto location = fromRoot(explicitRoot);
        require(!location.root.empty(), "Nelson installation not found at NELSONC_RUNTIME_ROOT.");
        require(matchesEngine(location, fingerprint),
            "Incompatible installed Nelson runtime: bytecode engine build or architecture "
            "differs.");
        return location;
    }
    if (executable.is_absolute()) {
        const auto adjacent = executable.parent_path()
            / (executable.stem().native() + fs::path(".runtime").native());
        std::error_code error;
        const auto exists = fs::exists(adjacent, error);
        require(!error, "Cannot inspect the adjacent application runtime.");
        if (exists) {
            const auto location = fromRoot(adjacent);
            require(matchesEngine(location, fingerprint),
                "The adjacent application runtime is incomplete or incompatible.");
            return location;
        }
    }
    const auto environment = fromBinaryDirectory(environmentPath("NELSON_RUNTIME_PATH"));
    if (matchesEngine(environment, fingerprint)) {
        return environment;
    }
    auto roots = SharedRuntimeRegistry::roots(fingerprint);
#ifdef _WIN32
    for (const auto* variable : { "LOCALAPPDATA", "ProgramW6432", "ProgramFiles" }) {
        const auto base = environmentPath(variable);
        if (base.is_absolute()) {
            roots.push_back(base / "Nelson" / "Runtime" / fingerprint);
        }
    }
#endif
    std::set<fs::path> visited;
    for (const auto& root : roots) {
        const auto location = fromRoot(root);
        if (!location.root.empty() && visited.insert(location.root).second
            && matchesEngine(location, fingerprint)) {
            return location;
        }
    }
    std::vector<fs::path> directories;
    const auto search = environmentPath("PATH").native();
#ifdef _WIN32
    const auto separator = L';';
#else
    const auto separator = ':';
#endif
    size_t begin = 0;
    do {
        const auto end = search.find(separator, begin);
        auto entry = search.substr(begin, end == fs::path::string_type::npos ? end : end - begin);
#ifdef _WIN32
        if (entry.size() >= 2 && entry.front() == L'"' && entry.back() == L'"') {
            entry = entry.substr(1, entry.size() - 2);
        }
#endif
        directories.emplace_back(entry);
        if (end == fs::path::string_type::npos) {
            break;
        }
        begin = end + 1;
    } while (begin < search.size());
    for (const auto& directory : directories) {
        if (!directory.is_absolute()) {
            continue;
        }
        const auto location = fromBinaryDirectory(directory);
        if (!location.root.empty() && visited.insert(location.root).second
            && matchesEngine(location, fingerprint)) {
            return location;
        }
    }
    throw std::runtime_error(
        "No compatible installed Nelson runtime found. Set NELSONC_RUNTIME_ROOT to the matching "
        "installation root. Required engine SHA-256: "
        + fingerprint);
}
//=============================================================================
} // namespace Nelson::ApplicationRuntime
//=============================================================================
