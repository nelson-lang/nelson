//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if defined(__linux__) || defined(__APPLE__)
#include <algorithm>
#include <array>
#include <cerrno>
#include <cstdlib>
#include <filesystem>
#include <fcntl.h>
#include <string>
#include <sys/stat.h>
#include <unistd.h>
#include <vector>
//=============================================================================
namespace Nelson::SharedRuntimeRegistry::Posix {
inline constexpr size_t maximumRecordSize = 32768;
inline constexpr size_t maximumSystemDirectories = 16;
inline constexpr char recordHeader[] = "NELSON_RUNTIME_ROOT_1\n";
inline bool
plain(const std::string& value)
{
    return !value.empty() && std::none_of(value.begin(), value.end(), [](unsigned char c) {
        return c < 32 || c == 127;
    });
}
//=============================================================================
inline std::string
environment(const char* name)
{
    const auto* value = std::getenv(name);
    if (!value) {
        return {};
    }
    size_t size = 0;
    while (size <= maximumRecordSize && value[size]) {
        ++size;
    }
    return size <= maximumRecordSize ? std::string(value, size) : std::string();
}
//=============================================================================
inline void
appendDirectory(std::vector<std::filesystem::path>& directories, const std::string& value)
{
    if (plain(value) && value.front() == '/') {
        const auto path = std::filesystem::u8path(value);
        if (std::find(directories.begin(), directories.end(), path) == directories.end()) {
            directories.push_back(path);
        }
    }
}
//=============================================================================
inline std::vector<std::filesystem::path>
configurationDirectories()
{
    std::vector<std::filesystem::path> directories;
#ifdef __APPLE__
    const auto home = environment("HOME");
    if (plain(home) && home.front() == '/') {
        appendDirectory(directories, home + "/Library/Application Support/Nelson/config");
    }
    appendDirectory(directories, "/Library/Application Support/Nelson/config");
#else
    appendDirectory(directories, environment("XDG_CONFIG_HOME"));
    if (directories.empty()) {
        const auto home = environment("HOME");
        if (plain(home) && home.front() == '/') {
            appendDirectory(directories, home + "/.config");
        }
    }
    auto system = environment("XDG_CONFIG_DIRS");
    if (system.empty()) {
        system = "/etc/xdg";
    }
    size_t begin = 0;
    for (size_t count = 0; count < maximumSystemDirectories; ++count) {
        const auto end = system.find(':', begin);
        appendDirectory(
            directories, system.substr(begin, end == std::string::npos ? end : end - begin));
        if (end == std::string::npos) {
            break;
        }
        begin = end + 1;
    }
#endif
    return directories;
}
//=============================================================================
inline std::filesystem::path
readRoot(const std::filesystem::path& filename)
{
    const auto descriptor
        = ::open(filename.c_str(), O_RDONLY | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK);
    if (descriptor < 0) {
        return {};
    }
    struct Closer
    {
        int descriptor;
        ~Closer() { ::close(descriptor); }
    } closer { descriptor };
    struct stat info {};
    if (::fstat(descriptor, &info) != 0 || !S_ISREG(info.st_mode) || info.st_size <= 0
        || info.st_size > static_cast<off_t>(maximumRecordSize)) {
        return {};
    }
    std::array<char, maximumRecordSize + 1> bytes {};
    size_t used = 0;
    while (used < bytes.size()) {
        const auto count = ::read(descriptor, bytes.data() + used, bytes.size() - used);
        if (count < 0 && errno == EINTR) {
            continue;
        }
        if (count < 0) {
            return {};
        }
        if (count == 0) {
            break;
        }
        used += static_cast<size_t>(count);
    }
    if (used != static_cast<size_t>(info.st_size)) {
        return {};
    }
    const std::string record(bytes.data(), used);
    const std::string prefix(recordHeader);
    if (record.compare(0, prefix.size(), prefix) != 0 || record.back() != '\n') {
        return {};
    }
    const auto root = record.substr(prefix.size(), record.size() - prefix.size() - 1);
    return plain(root) && root.front() == '/' ? std::filesystem::u8path(root)
                                              : std::filesystem::path();
}
//=============================================================================
inline std::vector<std::filesystem::path>
roots(const std::string& fingerprint)
{
    std::vector<std::filesystem::path> result;
    if (fingerprint.size() != 64
        || fingerprint.find_first_not_of("0123456789abcdef") != std::string::npos) {
        return result;
    }
#ifdef __APPLE__
#if defined(__aarch64__) || defined(__arm64__)
    const auto architecture = "maca64";
#else
    const auto architecture = "maci64";
#endif
#else
    const auto architecture = sizeof(void*) == 8 ? "glnxa64" : "glnxa32";
#endif
    for (const auto& directory : configurationDirectories()) {
        const auto root
            = readRoot(directory / "nelson" / "runtimes" / architecture / (fingerprint + ".root"));
        if (!root.empty() && std::find(result.begin(), result.end(), root) == result.end()) {
            result.push_back(root);
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson::SharedRuntimeRegistry::Posix
#endif
//=============================================================================
