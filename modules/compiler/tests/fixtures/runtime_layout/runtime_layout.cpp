// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "ApplicationRuntime.hpp"
#include "RuntimeLibrarySearch.hpp"
#include <iostream>
#include <random>

namespace {
namespace fs = std::filesystem;
namespace runtime = Nelson::ApplicationRuntime;
using Nelson::ApplicationArchive::require;

#ifdef _WIN32
void
checkLibraryDirectory()
{
    using Nelson::RuntimeLibrarySearch::windowsLibraryDirectory;
    require(
        windowsLibraryDirectory(L"C:/runtime/data/../bin").native() == L"\\\\?\\C:\\runtime\\bin",
        "Drive library directory was not normalized.");
    require(windowsLibraryDirectory(L"\\\\server\\share\\runtime\\bin").native()
            == L"\\\\?\\UNC\\server\\share\\runtime\\bin",
        "UNC library directory was not extended.");
    const fs::path extended(L"\\\\?\\C:\\runtime\\bin");
    require(windowsLibraryDirectory(extended) == extended, "Extended path was prefixed twice.");
    const fs::path unicode(L"C:\\runtime \u00e9\\bin");
    require(windowsLibraryDirectory(unicode).native() == L"\\\\?\\" + unicode.native(),
        "Unicode library directory was changed.");
    bool rejected = false;
    try {
        windowsLibraryDirectory(L"relative/bin");
    } catch (const std::runtime_error&) {
        rejected = true;
    }
    require(rejected, "Relative library directory was accepted.");
}
#endif

void
write(const fs::path& path, const std::string& contents)
{
    fs::create_directories(path.parent_path());
    std::ofstream output(path, std::ios::binary);
    output << contents;
    require(static_cast<bool>(output), "Cannot create runtime locator fixture.");
}

void
dataFiles(const fs::path& data)
{
    write(data / "modules/modules.m", "Fixture, never executed.");
    write(data / "modules/interpreter/functions/__nelsonc_run__.m", "Fixture, never executed.");
}

void
libraryFiles(const fs::path& libraries)
{
    write(libraries / runtime::libraryName("Engine"), "Fixture, never loaded.");
    write(libraries / runtime::libraryName("Interpreter"), "Interpreter fixture.");
}

void
checkLocation(const runtime::Location& location, const fs::path& data, const fs::path& libraries,
    const fs::path& binaries)
{
    require(!location.root.empty(), "Configured installation prefix was not located.");
    require(fs::equivalent(location.root, data), "Wrong runtime data root.");
    require(fs::equivalent(location.libraries, libraries), "Wrong runtime libraries.");
    require(fs::equivalent(location.binaries, binaries), "Wrong runtime executables.");
    require(runtime::matchesEngine(
                location, picosha2::hash256_hex_string(std::string("Interpreter fixture."))),
        "Matching installed interpreter was rejected.");
    require(!runtime::matchesEngine(location, std::string(64, '0')),
        "Different installed interpreter was accepted.");
}

void
setEnvironment(const char* name, const fs::path& value)
{
#ifdef _WIN32
    require(SetEnvironmentVariableW(fs::path(name).c_str(), value.empty() ? nullptr : value.c_str())
            != 0,
        "Cannot set test environment.");
#else
    require((value.empty() ? unsetenv(name) : setenv(name, value.c_str(), 1)) == 0,
        "Cannot set test environment.");
#endif
}

void
checkInstalled(const fs::path& root)
{
    const auto prefix = root / "relocated installation";
    const auto data = prefix / NELSONC_INSTALL_DATADIR;
    const auto libraries = prefix / NELSONC_INSTALL_LIBDIR;
    const auto binaries = prefix / NELSONC_INSTALL_BINDIR;
    dataFiles(data);
    libraryFiles(libraries);
    fs::create_directories(binaries);
    checkLocation(runtime::fromRoot(prefix), data, libraries, binaries);
    checkLocation(runtime::fromRoot(data), data, libraries, binaries);
    checkLocation(runtime::fromBinaryDirectory(binaries), data, libraries, binaries);
    checkLocation(runtime::fromRoot(prefix / ""), data, libraries, binaries);
    checkLocation(runtime::fromRoot(data / ""), data, libraries, binaries);
    checkLocation(runtime::fromBinaryDirectory(binaries / ""), data, libraries, binaries);
    fs::create_directories(binaries / "unrelated child");
    require(runtime::fromBinaryDirectory(binaries / "unrelated child").root.empty(),
        "Unrelated PATH directory was treated as the installation's binary directory.");
    require(runtime::fromRoot(prefix / "missing").root.empty(), "Missing root was accepted.");
    require(runtime::fromRoot("relative-root").root.empty(), "Relative root was accepted.");
    const auto fingerprint = picosha2::hash256_hex_string(std::string("Interpreter fixture."));
    setEnvironment("NELSON_RUNTIME_PATH", {});
    setEnvironment("PATH", binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", prefix);
    checkLocation(runtime::findInstalled(fingerprint), data, libraries, binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", data);
    checkLocation(runtime::findInstalled(fingerprint), data, libraries, binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", prefix / "missing");
    bool rejected = false;
    try {
        runtime::findInstalled(fingerprint);
    } catch (const std::exception&) {
        rejected = true;
    }
    require(rejected, "Invalid explicit root silently fell back to PATH.");
    setEnvironment("NELSONC_RUNTIME_ROOT", {});
    checkLocation(runtime::findInstalled(fingerprint), data, libraries, binaries);
}

void
checkStandardFallback(const fs::path& root)
{
    const auto prefix = root / "standard installation";
    const auto data = prefix / "share/Nelson";
#ifdef _WIN32
    const auto libraries = prefix / "bin";
#else
    const auto libraries = prefix / "lib/Nelson";
#endif
    const auto binaries = prefix / "bin";
    dataFiles(data);
    libraryFiles(libraries);
    fs::create_directories(binaries);
    checkLocation(runtime::fromRoot(prefix), data, libraries, binaries);
    checkLocation(runtime::fromRoot(data), data, libraries, binaries);
    checkLocation(runtime::fromBinaryDirectory(binaries), data, libraries, binaries);
}

void
checkAdjacent(const fs::path& root)
{
    const auto fallback = runtime::fromRoot(root / "flat runtime");
    const auto data = root / "application.runtime";
    const auto libraries = data / fallback.libraries.lexically_relative(fallback.root);
    dataFiles(data);
    libraryFiles(libraries);
    const auto fingerprint = picosha2::hash256_hex_string(std::string("Interpreter fixture."));
    const auto executable = root / "application.exe";
    setEnvironment("NELSONC_RUNTIME_ROOT", {});
    setEnvironment("NELSON_RUNTIME_PATH", fallback.binaries);
    setEnvironment("PATH", fallback.binaries);
    checkLocation(runtime::findInstalled(fingerprint, executable), data, libraries, libraries);
    checkLocation(runtime::findInstalled(fingerprint, root / "other.exe"), fallback.root,
        fallback.libraries, fallback.binaries);
    checkLocation(runtime::findInstalled(fingerprint, "application.exe"), fallback.root,
        fallback.libraries, fallback.binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", fallback.root);
    checkLocation(runtime::findInstalled(fingerprint, executable), fallback.root,
        fallback.libraries, fallback.binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", {});
    write(libraries / runtime::libraryName("Interpreter"), "Wrong interpreter.");
    bool rejected = false;
    try {
        runtime::findInstalled(fingerprint, executable);
    } catch (const std::exception&) {
        rejected = true;
    }
    require(rejected, "Incompatible adjacent runtime silently fell back to an external runtime.");
}

void
checkFlat(const fs::path& root)
{
    const auto data = root / "flat runtime";
#if defined(_WIN32) && (defined(_M_ARM64) || defined(__aarch64__))
    const auto libraries = data / "bin/ARM64";
#elif defined(_WIN32)
    const auto libraries = data / "bin/x64";
#elif defined(__APPLE__)
    const auto libraries = data / "bin/macOS";
#else
    const auto libraries = data / "bin/linux";
#endif
    dataFiles(data);
    libraryFiles(libraries);
    checkLocation(runtime::fromRoot(data), data, libraries, libraries);
    checkLocation(runtime::fromBinaryDirectory(libraries), data, libraries, libraries);
}

#ifdef _WIN32
void
checkShared(const fs::path& root)
{
    const auto fallback = runtime::fromRoot(root / "flat runtime");
    const auto fingerprint = picosha2::hash256_hex_string(std::string("Interpreter fixture."));
    const auto local = root / "local data";
    const auto data = local / "Nelson/Runtime" / fingerprint;
    const auto libraries = data / fallback.libraries.lexically_relative(fallback.root);
    dataFiles(data);
    libraryFiles(libraries);
    setEnvironment("LOCALAPPDATA", local);
    setEnvironment("ProgramW6432", root / "programs64");
    setEnvironment("ProgramFiles", root / "programs");
    setEnvironment("NELSONC_RUNTIME_ROOT", {});
    setEnvironment("NELSON_RUNTIME_PATH", {});
    setEnvironment("PATH", fallback.binaries);
    checkLocation(runtime::findInstalled(fingerprint), data, libraries, libraries);
    setEnvironment("NELSON_RUNTIME_PATH", fallback.binaries);
    checkLocation(
        runtime::findInstalled(fingerprint), fallback.root, fallback.libraries, fallback.binaries);
    setEnvironment("NELSON_RUNTIME_PATH", {});
    write(libraries / runtime::libraryName("Interpreter"), "Incorrect shared engine.");
    checkLocation(
        runtime::findInstalled(fingerprint), fallback.root, fallback.libraries, fallback.binaries);
    libraryFiles(libraries);

    // Redirect predefined hives only in this process; the real runtime registry is untouched.
    const auto sandboxName = L"Software\\Nelson\\Tests\\" + root.filename().wstring();
    HKEY sandbox = nullptr;
    require(RegCreateKeyExW(HKEY_CURRENT_USER, sandboxName.c_str(), 0, nullptr, 0, KEY_ALL_ACCESS,
                nullptr, &sandbox, nullptr)
            == ERROR_SUCCESS,
        "Cannot create isolated registry fixture.");
    struct RestoreRegistry
    {
        HKEY sandbox;
        std::wstring name;
        ~RestoreRegistry()
        {
            RegOverridePredefKey(HKEY_CURRENT_USER, nullptr);
            RegOverridePredefKey(HKEY_LOCAL_MACHINE, nullptr);
            RegCloseKey(sandbox);
            RegDeleteTreeW(HKEY_CURRENT_USER, name.c_str());
            RegDeleteKeyW(HKEY_CURRENT_USER, name.c_str());
        }
    } restore { sandbox, sandboxName };
    require(RegOverridePredefKey(HKEY_CURRENT_USER, sandbox) == ERROR_SUCCESS
            && RegOverridePredefKey(HKEY_LOCAL_MACHINE, sandbox) == ERROR_SUCCESS,
        "Cannot isolate runtime registry lookup.");
#if defined(_M_ARM64) || defined(__aarch64__)
    const auto registryPath = L"Software\\Nelson\\Runtime\\woa64\\";
#else
    const auto registryPath = L"Software\\Nelson\\Runtime\\win64\\";
#endif
    const auto keyName = registryPath + std::wstring(fingerprint.begin(), fingerprint.end());
    HKEY key = nullptr;
    require(RegCreateKeyExW(HKEY_CURRENT_USER, keyName.c_str(), 0, nullptr, 0,
                KEY_ALL_ACCESS | KEY_WOW64_64KEY, nullptr, &key, nullptr)
            == ERROR_SUCCESS,
        "Cannot create runtime registration fixture.");
    const auto value = fallback.root.wstring();
    const auto status
        = RegSetValueExW(key, L"Root", 0, REG_SZ, reinterpret_cast<const BYTE*>(value.c_str()),
            static_cast<DWORD>((value.size() + 1) * sizeof(wchar_t)));
    RegCloseKey(key);
    require(status == ERROR_SUCCESS, "Cannot populate runtime registration fixture.");
    checkLocation(
        runtime::findInstalled(fingerprint), fallback.root, fallback.libraries, fallback.binaries);
    setEnvironment("NELSONC_RUNTIME_ROOT", data);
    checkLocation(runtime::findInstalled(fingerprint), data, libraries, libraries);
    setEnvironment("NELSONC_RUNTIME_ROOT", {});
}
#endif
}

int
main(int argc, char** argv)
{
    try {
        require(argc == 2, "An absolute test output directory is required.");
        const fs::path parent(argv[1]);
        require(parent.is_absolute(), "Test output directory must be absolute.");
        fs::create_directories(parent);
        const auto root = parent
            / ("run-" + std::to_string(std::random_device {}()) + "-"
                + std::to_string(std::random_device {}()));
        require(fs::create_directory(root), "Test directory already exists.");
        checkFlat(root);
        checkInstalled(root);
        checkStandardFallback(root);
        checkAdjacent(root);
#ifdef _WIN32
        checkLibraryDirectory();
        checkShared(root);
#endif
        std::cout << "PASS native runtime layout and installed interpreter identity\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
