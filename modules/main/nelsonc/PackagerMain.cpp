//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
#include "ApplicationLauncherContract.hpp"
#include "StartNelsonApplication.h"
#include "NelSon_engine_mode.h"
#include <filesystem>
#include <cstdlib>
#include <cstdio>
#include <string>
#include <vector>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#ifdef NELSONC_WINDOWED
#include <shellapi.h>
#endif
#else
#include <dlfcn.h>
#include <unistd.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
#endif
#include "ApplicationRuntime.hpp"
#include "RuntimeLibrarySearch.hpp"
#include "RuntimeLog.hpp"
//=============================================================================
namespace {
namespace fs = std::filesystem;
using Nelson::ApplicationArchive::require;
static_assert(Nelson::ApplicationLauncher::Contract {}.startupInterface
    == NELSON_APPLICATION_STARTUP_VERSION);
#if defined(NELSONC_TOOL)
constexpr auto launcherContract
    = Nelson::ApplicationLauncher::record(Nelson::ApplicationLauncher::Kind::Compiler);
#elif defined(NELSONC_WINDOWED)
constexpr auto launcherContract
    = Nelson::ApplicationLauncher::record(Nelson::ApplicationLauncher::Kind::Windowed);
#else
constexpr auto launcherContract
    = Nelson::ApplicationLauncher::record(Nelson::ApplicationLauncher::Kind::Console);
#endif
#ifndef NELSONC_TOOL
constexpr auto startupConfiguration = Nelson::ApplicationStartup::record();
#endif
fs::path
executablePath()
{
#ifdef _WIN32
    std::wstring path(32768, L'\0');
    const auto length = GetModuleFileNameW(nullptr, path.data(), static_cast<DWORD>(path.size()));
    require(length > 0 && length < path.size(), "Cannot locate executable.");
    path.resize(length);
    return fs::path(path);
#elif defined(__APPLE__)
    uint32_t size = 0;
    _NSGetExecutablePath(nullptr, &size);
    std::string path(size, '\0');
    require(_NSGetExecutablePath(path.data(), &size) == 0, "Cannot locate executable.");
    return fs::canonical(path.c_str());
#else
    return fs::canonical("/proc/self/exe");
#endif
}
//=============================================================================
fs::path
findLibraryDirectory(const fs::path& root)
{
    const auto directory = Nelson::ApplicationRuntime::libraryDirectory(root);
    require(!directory.empty(), "Missing application runtime.");
    return directory;
}
#ifdef _WIN32
[[noreturn]] void
loaderFailure(const char* message, const fs::path& path, DWORD code)
{
    throw std::runtime_error(std::string(message) + " Path: " + path.generic_u8string()
        + " (Windows error " + std::to_string(code) + ").");
}
#endif
int
startEngine(const fs::path& libraries, const NelsonApplicationStartup& startup,
    Nelson::RuntimeLogging::Log& log)
{
#ifdef _WIN32
    if (!SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS)) {
        loaderFailure("Cannot configure runtime library search.", libraries, GetLastError());
    }
    // Dependent DLL names can cross MAX_PATH even when the directory does not.
    const auto directory = Nelson::RuntimeLibrarySearch::windowsLibraryDirectory(libraries);
    auto cookie = AddDllDirectory(directory.c_str());
    if (cookie == nullptr) {
        loaderFailure("Cannot configure runtime library directory.", directory, GetLastError());
    }
    const auto library = directory / "libnlsEngine.dll";
    HMODULE handle = LoadLibraryExW(library.c_str(), nullptr,
        LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
    if (handle == nullptr) {
        const auto code = GetLastError();
        RemoveDllDirectory(cookie);
        loaderFailure("Cannot load application runtime.", library, code);
    }
    auto start = reinterpret_cast<NelsonApplicationStartFunction>(
        GetProcAddress(handle, "StartNelsonApplicationV1"));
    if (start == nullptr) {
        const auto code = GetLastError();
        FreeLibrary(handle);
        RemoveDllDirectory(cookie);
        loaderFailure("Incompatible application runtime: native startup interface V1 is missing.",
            library, code);
    }
#else
#ifdef __APPLE__
    const auto library = libraries / "libnlsEngine.dylib";
#else
    const auto library = libraries / "libnlsEngine.so";
#endif
    void* handle = dlopen(library.c_str(), RTLD_NOW | RTLD_GLOBAL);
    if (handle == nullptr) {
        const auto* error = dlerror();
        throw std::runtime_error("Cannot load application runtime: " + library.u8string() + ": "
            + (error == nullptr ? "unknown loader error" : error));
    }
    auto start = reinterpret_cast<NelsonApplicationStartFunction>(
        dlsym(handle, "StartNelsonApplicationV1"));
    require(start != nullptr,
        "Incompatible application runtime: native startup interface V1 is missing.");
#endif
#ifdef _WIN32
    NelsonApplicationOutputFunction bindOutput = nullptr;
    if (log.enabled()) {
        bindOutput = reinterpret_cast<NelsonApplicationOutputFunction>(
            GetProcAddress(handle, "ConfigureNelsonApplicationOutputV1"));
        if (!bindOutput) {
            const auto code = GetLastError();
            FreeLibrary(handle);
            RemoveDllDirectory(cookie);
            loaderFailure(
                "Incompatible application runtime: output capture interface V1 is missing.",
                library, code);
        }
        if (bindOutput(reinterpret_cast<intptr_t>(GetStdHandle(STD_OUTPUT_HANDLE)),
                reinterpret_cast<intptr_t>(GetStdHandle(STD_ERROR_HANDLE)))
            != 0) {
            bindOutput(reinterpret_cast<intptr_t>(log.originalOutput(false)),
                reinterpret_cast<intptr_t>(log.originalOutput(true)));
            FreeLibrary(handle);
            RemoveDllDirectory(cookie);
            throw std::runtime_error("Cannot initialize runtime output capture.");
        }
    }
#endif
    auto result = start(&startup);
#ifdef _WIN32
    if (bindOutput
        && bindOutput(reinterpret_cast<intptr_t>(log.originalOutput(false)),
               reinterpret_cast<intptr_t>(log.originalOutput(true)))
            != 0) {
        std::fputs("nelsonc: Cannot finish runtime output capture.\n", stderr);
        if (result == 0) {
            result = 2;
        }
    }
    FreeLibrary(handle);
    RemoveDllDirectory(cookie);
#else
    dlclose(handle);
#endif
    return result;
}
#ifdef NELSONC_TOOL
NELSON_ENGINE_MODE
compilerMode(const std::vector<fs::path::string_type>& arguments)
{
    for (size_t i = 0; i < arguments.size(); ++i) {
        const auto& argument = arguments[i];
        if (argument == fs::path("-a").native() || argument == fs::path("-o").native()
            || argument == fs::path("--runtime").native()
            || argument == fs::path("--runtime-log-file").native()
            || argument == fs::path("--executable-version").native()
            || argument == fs::path("--executable-icon").native()
            || argument == fs::path("--help-file").native()) {
            ++i;
        } else if (argument == fs::path("--cli").native()) {
            return BASIC_TERMINAL;
        }
    }
    return ADVANCED_TERMINAL;
}
#endif
int
launch(const std::vector<fs::path::string_type>& userArguments, Nelson::RuntimeLogging::Log& log)
{
    const auto self = executablePath();
    const auto invokingDirectory = fs::current_path();
    fs::path libraries = self.parent_path();
    fs::path binaries;
    fs::path root;
    uint32_t flags = 0;
#ifdef NELSONC_TOOL
    const auto installed = Nelson::ApplicationRuntime::fromBinaryDirectory(self.parent_path());
    require(!installed.root.empty(), "Cannot locate the compiler's Nelson installation.");
    libraries = installed.libraries;
    binaries = installed.binaries;
    root = installed.root;
    flags = NELSON_APPLICATION_COMPILER;
    if (compilerMode(userArguments) == ADVANCED_TERMINAL) {
        flags |= NELSON_APPLICATION_GUI;
    }
#else
    const auto trailer = Nelson::LauncherInputFile::readTrailer(self);
    const auto logFilename = Nelson::LauncherInputFile::readLogFilename(
        self, Nelson::ApplicationStartup::embeddedLocation(startupConfiguration), trailer.offset);
    if (!logFilename.empty()) {
        log.open(fs::u8path(logFilename), self);
    }
    if (trailer.installedRuntime) {
        const auto installed
            = Nelson::ApplicationRuntime::findInstalled(trailer.engineFingerprint, self);
        root = installed.root;
        libraries = installed.libraries;
        binaries = installed.binaries;
    } else {
        root = self.parent_path() / fs::u8path(trailer.runtimeDirectory);
        libraries = findLibraryDirectory(root);
    }
    if (trailer.gui) {
        flags |= NELSON_APPLICATION_GUI;
    }
#endif
    Nelson::RuntimeLibrarySearch::initialize(self, libraries, userArguments);
    log.startCapture();
    if (binaries.empty()) {
        binaries = libraries;
    }
    std::vector<const fs::path::value_type*> pointers;
    pointers.reserve(userArguments.size());
    for (const auto& argument : userArguments) {
        pointers.push_back(argument.c_str());
    }
    const NelsonApplicationStartup startup { sizeof(NelsonApplicationStartup),
        Nelson::ApplicationLauncher::startupInterface(launcherContract), flags,
        static_cast<int>(pointers.size()), pointers.data(), self.c_str(), root.c_str(),
        libraries.c_str(), binaries.c_str(), invokingDirectory.c_str() };
    return startEngine(libraries, startup, log);
}
//=============================================================================
int
applicationMain(int argc, fs::path::value_type* argv[])
{
    Nelson::RuntimeLogging::Log log;
    int result = 2;
    try {
        result = launch({ argv + 1, argv + argc }, log);
    } catch (const std::exception& error) {
        const std::string message = std::string("nelsonc: ") + error.what() + "\n";
        log.diagnostic(message);
        std::fputs(message.c_str(), stderr);
    }
    if (!log.finish()) {
        std::fputs("nelsonc: Cannot complete runtime log writes.\n", stderr);
        if (result == 0) {
            result = 2;
        }
    }
    return result;
}
//=============================================================================
} // namespace
#if defined(_WIN32) && defined(NELSONC_WINDOWED)
int WINAPI
wWinMain(HINSTANCE, HINSTANCE, PWSTR, int)
{
    int argc = 0;
    auto* argv = CommandLineToArgvW(GetCommandLineW(), &argc);
    if (argv == nullptr) {
        std::fputs("nelsonc: Cannot read application arguments.\n", stderr);
        return 2;
    }
    const auto result = applicationMain(argc, argv);
    LocalFree(argv);
    return result;
}
#elif defined(_WIN32)
int
wmain(int argc, wchar_t* argv[])
{
    return applicationMain(argc, argv);
}
#else
int
main(int argc, char* argv[])
{
    return applicationMain(argc, argv);
}
#endif
//=============================================================================
