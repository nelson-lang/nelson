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
#include <cerrno>
#include <cstdlib>
#include <filesystem>
#include <stdexcept>
#include <string>
#include <system_error>
#include <vector>
#if defined(__linux__)
#include <sys/auxv.h>
#include <unistd.h>
#elif defined(__APPLE__)
#include <unistd.h>
#endif
namespace Nelson::RuntimeLibrarySearch {
#ifdef _WIN32
inline std::filesystem::path
windowsLibraryDirectory(const std::filesystem::path& directory)
{
    if (!directory.is_absolute()) {
        throw std::runtime_error("Runtime library directory must be absolute.");
    }
    const auto native = directory.lexically_normal().make_preferred().native();
    if (native.compare(0, 4, L"\\\\?\\") == 0) {
        return native;
    }
    if (native.compare(0, 2, L"\\\\") == 0) {
        return L"\\\\?\\UNC\\" + native.substr(2);
    }
    return L"\\\\?\\" + native;
}
#endif
inline std::string
linuxLibraryPath(const std::string& directory, const std::string& inherited)
{
    if (directory.empty() || directory.front() != '/'
        || directory.find_first_of(":;$") != std::string::npos
        || directory.find('\0') != std::string::npos || inherited.find('\0') != std::string::npos) {
        throw std::runtime_error(
            "Linux runtime library paths must be absolute and cannot contain ':', ';' or '$'.");
    }
    const auto separator = inherited.find_first_of(":;");
    if (inherited.substr(0, separator) == directory) {
        return inherited;
    }
    return inherited.empty() ? directory : directory + ':' + inherited;
}
//=============================================================================
inline std::string
macLibraryPath(const std::string& directory, const std::string& inherited)
{
    if (directory.empty() || directory.front() != '/'
        || directory.find_first_of(":;$") != std::string::npos
        || directory.find('\0') != std::string::npos || inherited.find('\0') != std::string::npos) {
        throw std::runtime_error(
            "macOS runtime library paths must be absolute and cannot contain ':', ';' or '$'.");
    }
    const auto separator = inherited.find(':');
    if (inherited.substr(0, separator) == directory) {
        return inherited;
    }
    return inherited.empty() ? directory : directory + ':' + inherited;
}
//=============================================================================
inline std::vector<char*>
argumentPointers(std::vector<std::string>& arguments)
{
    std::vector<char*> pointers;
    pointers.reserve(arguments.size() + 1);
    for (auto& argument : arguments) {
        pointers.push_back(argument.data());
    }
    pointers.push_back(nullptr);
    return pointers;
}
//=============================================================================
inline void
initialize(const std::filesystem::path& executable, const std::filesystem::path& libraries,
    const std::vector<std::filesystem::path::string_type>& userArguments)
{
#if defined(__linux__) || defined(__APPLE__)
#ifdef __linux__
    if (getauxval(AT_SECURE) != 0) {
        throw std::runtime_error("Privileged execution of compiler launchers is not supported.");
    }
    constexpr const char* variable = "LD_LIBRARY_PATH";
#else
    constexpr const char* variable = "DYLD_LIBRARY_PATH";
#endif
    const auto* current = std::getenv(variable);
    const bool wasSet = current != nullptr;
    const std::string inherited = wasSet ? current : "";
#ifdef __linux__
    const auto desired
        = linuxLibraryPath(std::filesystem::canonical(libraries).native(), inherited);
#else
    const auto desired = macLibraryPath(std::filesystem::canonical(libraries).native(), inherited);
    constexpr const char* frameworkVariable = "DYLD_FRAMEWORK_PATH";
    const auto* currentFramework = std::getenv(frameworkVariable);
    const bool frameworkWasSet = currentFramework != nullptr;
    const std::string inheritedFramework = frameworkWasSet ? currentFramework : "";
    const auto frameworks = libraries / "Frameworks";
    const auto desiredFramework = std::filesystem::is_directory(frameworks)
        ? macLibraryPath(std::filesystem::canonical(frameworks).native(), inheritedFramework)
        : inheritedFramework;
#endif
#ifdef __APPLE__
    if (desired == inherited && desiredFramework == inheritedFramework) {
#else
    if (desired == inherited) {
#endif
        return;
    }
    std::vector<std::string> arguments { executable.native() };
    arguments.insert(arguments.end(), userArguments.begin(), userArguments.end());
    const auto pointers = argumentPointers(arguments);
    if (setenv(variable, desired.c_str(), 1) != 0) {
        throw std::system_error(
            errno, std::generic_category(), "Cannot set runtime library search");
    }
#ifdef __APPLE__
    if (setenv(frameworkVariable, desiredFramework.c_str(), 1) != 0) {
        throw std::system_error(
            errno, std::generic_category(), "Cannot set runtime framework search");
    }
#endif
    // The loader reads its search environment at process startup. Re-enter
    // before changing cwd or creating resources whose destructors must run.
    execv(executable.c_str(), pointers.data());
    const int error = errno;
    if (wasSet) {
        setenv(variable, inherited.c_str(), 1);
    } else {
        unsetenv(variable);
    }
#ifdef __APPLE__
    if (frameworkWasSet) {
        setenv(frameworkVariable, inheritedFramework.c_str(), 1);
    } else {
        unsetenv(frameworkVariable);
    }
#endif
    throw std::system_error(error, std::generic_category(), "Cannot restart runtime launcher");
#else
    (void)executable;
    (void)libraries;
    (void)userArguments;
#endif
}
//=============================================================================
} // namespace Nelson::RuntimeLibrarySearch
//=============================================================================
