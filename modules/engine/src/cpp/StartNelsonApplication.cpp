//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StartNelsonApplication.h"
#include "StartNelson.h"
#include "ApplicationDirectory.hpp"
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iomanip>
#include <iostream>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <fcntl.h>
#include <io.h>
#endif
namespace {
namespace fs = std::filesystem;
#ifdef _WIN32
bool
bindOutput(FILE* stream, intptr_t value)
{
    fflush(stream);
    if (_fileno(stream) < 0) {
        FILE* initialized = nullptr;
        if (freopen_s(&initialized, "NUL", "wb", stream) != 0) {
            return false;
        }
    }
    const auto source = reinterpret_cast<HANDLE>(value);
    HANDLE copy = INVALID_HANDLE_VALUE;
    if (source == nullptr || source == INVALID_HANDLE_VALUE) {
        copy = CreateFileW(L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
            OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
        if (copy == INVALID_HANDLE_VALUE) {
            return false;
        }
    } else if (!DuplicateHandle(GetCurrentProcess(), source, GetCurrentProcess(), &copy, 0, FALSE,
                   DUPLICATE_SAME_ACCESS)) {
        return false;
    }
    const auto descriptor
        = _open_osfhandle(reinterpret_cast<intptr_t>(copy), _O_BINARY | _O_WRONLY);
    if (descriptor < 0) {
        CloseHandle(copy);
        return false;
    }
    const auto result = _dup2(descriptor, _fileno(stream));
    _close(descriptor);
    return result == 0;
}
#endif
void
require(bool condition, const char* message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}
//=============================================================================
class ExtractionDirectory
{
public:
    ExtractionDirectory()
    {
        std::random_device random;
        std::ostringstream name;
        name << "nelsonc-" << std::hex << std::setfill('0');
        for (size_t i = 0; i < 4; ++i) {
            name << std::setw(8) << random();
        }
        directory = fs::canonical(fs::temp_directory_path()) / name.str();
        Nelson::ApplicationArchive::createPrivateDirectory(directory);
    }

    ~ExtractionDirectory()
    {
        std::error_code error;
        fs::current_path(directory.parent_path(), error);
        fs::remove_all(directory, error);
        if (error) {
            std::fprintf(stderr, "nelsonc: Cannot remove application extraction directory: %s\n",
                error.message().c_str());
        }
    }

    ExtractionDirectory(const ExtractionDirectory&) = delete;
    ExtractionDirectory&
    operator=(const ExtractionDirectory&)
        = delete;

    const fs::path&
    path() const
    {
        return directory;
    }

private:
    fs::path directory;
};
//=============================================================================
void
environment(const char* name, const fs::path& value)
{
#ifdef _WIN32
    const std::string key(name);
    const std::wstring wideKey(key.begin(), key.end());
    require(SetEnvironmentVariableW(wideKey.c_str(), value.empty() ? nullptr : value.c_str()) != 0,
        "Cannot initialize runtime environment.");
#else
    const int result = value.empty() ? unsetenv(name) : setenv(name, value.c_str(), 1);
    require(result == 0, "Cannot initialize runtime environment.");
#endif
}
//=============================================================================
void
validate(const NelsonApplicationStartup* startup)
{
    require(startup != nullptr && startup->size == sizeof(NelsonApplicationStartup)
            && startup->version == NELSON_APPLICATION_STARTUP_VERSION,
        "Incompatible application startup interface.");
    require((startup->flags & ~(NELSON_APPLICATION_COMPILER | NELSON_APPLICATION_GUI)) == 0,
        "Invalid application startup flags.");
    require(startup->argc >= 0 && (startup->argc == 0 || startup->argv != nullptr),
        "Invalid application startup arguments.");
    for (int i = 0; i < startup->argc; ++i) {
        require(startup->argv[i] != nullptr, "Invalid application startup argument.");
    }
    for (const auto* path : { startup->executable, startup->root, startup->libraries,
             startup->binaries, startup->invokingDirectory }) {
        require(path != nullptr && fs::path(path).is_absolute(),
            "Application startup paths must be absolute.");
    }
}
//=============================================================================
int
run(const NelsonApplicationStartup& startup)
{
    const fs::path root(startup.root);
    const fs::path libraries(startup.libraries);
    const bool compiler = (startup.flags & NELSON_APPLICATION_COMPILER) != 0;
    const bool gui = (startup.flags & NELSON_APPLICATION_GUI) != 0;
    environment("NELSON_ROOT_PATH", root);
    environment("NELSON_LIBRARIES_PATH", libraries);
    environment("NELSON_BINARIES_PATH", startup.binaries);
    environment("NELSON_MODULES_PATH", {});
    environment("NELSONC_INVOKING_DIRECTORY", startup.invokingDirectory);

    std::optional<ExtractionDirectory> extraction;
    if (!compiler) {
        environment("NELSON_USERPATH", root);
        fs::current_path(root);
        extraction.emplace();
        environment("NELSONC_EXTRACTION_DIRECTORY", extraction->path());
        if (gui) {
            environment("QT_PLUGIN_PATH", libraries / "plugins");
            environment("QT_QPA_PLATFORM_PLUGIN_PATH", libraries / "plugins" / "platforms");
            environment("QML_IMPORT_PATH", libraries / "qml");
            environment("QML2_IMPORT_PATH", libraries / "qml");
        }
    }

    std::vector<fs::path::string_type> arguments { startup.executable };
    for (const char* option : { "--quiet", "--no-user-startup", "--no-user-modules", "--no-ipc",
             "--without-file-watcher", "--execute" }) {
        arguments.push_back(fs::path(option).native());
    }
    const char* operation = compiler ? "__load_compiler__(); __nelsonc_cli__" : "__nelsonc_run__";
    const std::string command
        = std::string("try; ") + operation + "(); catch ex; disp(ex.message); exit(1); end";
    arguments.push_back(fs::path(command).native());
    arguments.push_back(fs::path("--").native());
    for (int i = 0; i < startup.argc; ++i) {
        arguments.emplace_back(startup.argv[i]);
    }
    std::vector<fs::path::value_type*> pointers;
    pointers.reserve(arguments.size());
    for (auto& argument : arguments) {
        pointers.push_back(argument.data());
    }
    const auto mode = gui ? ADVANCED_TERMINAL : BASIC_TERMINAL;
#ifdef _WIN32
    return StartNelsonW(static_cast<int>(pointers.size()), pointers.data(), mode);
#else
    return StartNelsonA(static_cast<int>(pointers.size()), pointers.data(), mode);
#endif
}
//=============================================================================
} // namespace
//=============================================================================
int
StartNelsonApplicationV1(const NelsonApplicationStartup* startup)
{
    try {
        validate(startup);
        return run(*startup);
    } catch (const std::exception& error) {
        std::fprintf(stderr, "nelsonc: %s\n", error.what());
    } catch (...) {
        std::fputs("nelsonc: Unexpected application startup failure.\n", stderr);
    }
    return 2;
}
#ifdef _WIN32
int
ConfigureNelsonApplicationOutputV1(intptr_t output, intptr_t error)
{
    try {
        std::cout.flush();
        std::cerr.flush();
        const bool outputReady = bindOutput(stdout, output);
        const bool errorReady = bindOutput(stderr, error);
        std::cout.clear();
        std::cerr.clear();
        return outputReady && errorReady ? 0 : 2;
    } catch (...) {
        return 2;
    }
}
#endif
//=============================================================================
