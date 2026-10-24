//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BuildCodeHelpers.hpp"
#include "NFlowCMakeCommand.hpp"
#include "SystemCommand.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include <algorithm>
#include <tuple>
#include <cstddef>
#include <cstdlib>
#include <fstream>
#include <mutex>
#include "ErrorID.hpp"
//=============================================================================
namespace nflow {
//=============================================================================
static std::wstring
buildJobCount()
{
    // Defaults to 1: generated models are single translation units, and many
    // builds at once (parallel test suite) would oversubscribe the CPU. Set
    // NFLOW_BUILD_JOBS to a positive integer to raise it.
    std::string value;
#ifdef _MSC_VER
    char* buffer = nullptr;
    size_t length = 0;
    if (_dupenv_s(&buffer, &length, "NFLOW_BUILD_JOBS") == 0 && buffer != nullptr) {
        value.assign(buffer);
        free(buffer);
    }
#else
    const char* env = std::getenv("NFLOW_BUILD_JOBS");
    if (env != nullptr) {
        value.assign(env);
    }
#endif
    bool valid = !value.empty();
    for (char c : value) {
        if (c < '0' || c > '9') {
            valid = false;
            break;
        }
    }
    if (valid && value.find_first_not_of('0') != std::string::npos) {
        return std::wstring(value.begin(), value.end());
    }
    return L"1";
}
//=============================================================================
static std::wstring
cmakeBinDir()
{
    std::wstring cmakePath = L"";
#ifdef _MSC_VER
    cmakePath = Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/tools/cmake/bin/";
#endif
    return cmakePath;
}
//=============================================================================
static bool
detectCToolChain(std::wstring& errorMessage)
{
    const std::wstring cmakeDir = cmakeBinDir();

    std::wstring command = buildCMakeCommand(cmakeDir, L"--version");
    size_t mainEvaluatorID = 0;
    uint64_t timeout = 30000;
    bool withEventsLoop = false;

    std::tuple<int, std::wstring, uint64_t> res
        = Nelson::SystemCommand(command, timeout, withEventsLoop, mainEvaluatorID);
    if (std::get<0>(res) != 0) {
        errorMessage = Nelson::resolveMessageTemplate(L"Nelson:nflow_engine:cToolchainFoundPlease",
            L"C toolchain not found. Please install CMake and ensure it is in your PATH.");
        return false;
    }

    nfs::path tempDir = nfs::path(Nelson::FileSystemWrapper::Path::unique_path().wstring());
    nfs::create_directories(tempDir);

    nfs::path cmakeLists = tempDir / L"CMakeLists.txt";

    // Minimal project that forces compiler detection
    std::wofstream file(cmakeLists);
    file << L"cmake_minimum_required(VERSION 3.15)\n";
    file << L"project(TestC C)\n";
    file.close();

    command = buildCMakeCommand(cmakeDir,
        L"-S \"" + tempDir.wstring() + L"\" -B \"" + (tempDir / L"build").wstring() + L"\"");

    res = Nelson::SystemCommand(command, timeout, false, 0);

    std::error_code ec;
    nfs::remove_all(tempDir, ec);

    if (std::get<0>(res) != 0) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:nflow_engine:cToolchainDetectedCompiler",
                L"No C toolchain detected (compiler not found).");
        return false;
    }
    return true;
}
//=============================================================================
bool
haveCToolChain(std::wstring& errorMessage)
{
    // Cached for the process: detectCToolChain() runs a full cmake compiler
    // detection (seconds on a cold MSVC toolchain) and the toolchain does not
    // change while Nelson runs.
    static std::once_flag onceFlag;
    static bool cachedResult = false;
    static std::wstring cachedError;
    std::call_once(onceFlag, [&]() { cachedResult = detectCToolChain(cachedError); });
    errorMessage = cachedError;
    return cachedResult;
}
//=============================================================================
static bool
detectRustToolChain(std::wstring& errorMessage)
{
    std::wstring command = L"cargo --version";
    size_t mainEvaluatorID = 0;
    uint64_t timeout = 5000;
    bool withEventsLoop = false;

    std::tuple<int, std::wstring, uint64_t> res
        = Nelson::SystemCommand(command, timeout, withEventsLoop, mainEvaluatorID);
    if (std::get<0>(res) != 0) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:nflow_engine:rustToolchainFoundPlease",
                L"Rust toolchain not found. Please install Rust and ensure cargo is in your PATH.");
        return false;
    }
    command = L"rustc --version";
    res = Nelson::SystemCommand(command, timeout, withEventsLoop, mainEvaluatorID);
    if (std::get<0>(res) != 0) {
        errorMessage
            = Nelson::resolveMessageTemplate(L"Nelson:nflow_engine:rustToolchainFoundPlease2",
                L"Rust toolchain not found. Please install Rust and ensure rustc is in your PATH.");
        return false;
    }
    return true;
}
//=============================================================================
bool
haveRustToolChain(std::wstring& errorMessage)
{
    // Cached per process, like haveCToolChain().
    static std::once_flag onceFlag;
    static bool cachedResult = false;
    static std::wstring cachedError;
    std::call_once(onceFlag, [&]() { cachedResult = detectRustToolChain(cachedError); });
    errorMessage = cachedError;
    return cachedResult;
}
//=============================================================================
bool
callCMake(const std::wstring& cmakeFilePath, const std::wstring& options,
    const std::wstring& buildDir, std::wstring& errorMessage, const std::wstring& buildConfig)
{
    size_t mainEvaluatorID = 0;
    uint64_t timeout = 0;
    bool withEventsLoop = false;

    const std::wstring cmakeDir = cmakeBinDir();

    nfs::path sourceDir = nfs::path(cmakeFilePath).parent_path();

    std::wstring configOpts = options;
    if (!buildConfig.empty()) {
        if (!configOpts.empty()) {
            configOpts += L" ";
        }
        configOpts += L"-DCMAKE_BUILD_TYPE=" + buildConfig;
    }

    std::wstring configureCmd = buildCMakeCommand(
        cmakeDir, L"-S \"" + sourceDir.wstring() + L"\" -B \"" + buildDir + L"\" " + configOpts);

    auto res = Nelson::SystemCommand(configureCmd, timeout, withEventsLoop, mainEvaluatorID);

    if (std::get<0>(res) != 0) {
        errorMessage = Nelson::formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:nflow_engine:cmakeConfigureFailed", L"CMake configure failed:\n%s"),
            std::get<1>(res));
        return false;
    }

    std::wstring buildCmd = buildCMakeCommand(cmakeDir, L"--build \"" + buildDir + L"\"");
    if (!buildConfig.empty()) {
        buildCmd += L" --config " + buildConfig;
    }
    // Bound the job count (default 1, see buildJobCount) to avoid CPU oversubscription.
    buildCmd += L" --parallel " + buildJobCount();

    auto buildRes = Nelson::SystemCommand(buildCmd, timeout, withEventsLoop, mainEvaluatorID);

    if (std::get<0>(buildRes) != 0) {
        errorMessage = Nelson::formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:nflow_engine:cmakeBuildFailed", L"CMake build failed:\n%s"),
            std::get<1>(buildRes));
        return false;
    }

    return true;
}
//=============================================================================
bool
callCargoToml(const std::wstring& cargoFilePath, const std::wstring& options,
    const std::wstring& buildDir, std::wstring& errorMessage)
{
    size_t mainEvaluatorID = 0;
    uint64_t timeout = 0;
    bool withEventsLoop = false;

    nfs::path cargoDir = nfs::path(cargoFilePath).parent_path();

    // cargo defaults to one job per logical core; bound it like buildJobCount.
    std::wstring command = L"cargo build --jobs " + buildJobCount() + L" --manifest-path \""
        + cargoFilePath + L"\" --target-dir \"" + buildDir + L"\" " + options;

    auto res = Nelson::SystemCommand(command, timeout, withEventsLoop, mainEvaluatorID);

    if (std::get<0>(res) != 0) {
        errorMessage = Nelson::formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:nflow_engine:cargoBuildFailed", L"Cargo build failed:\n%s"),
            std::get<1>(res));
        return false;
    }

    return true;
}
//=============================================================================
std::wstring
getNflowCMakeDefines()
{
    std::wstring nelsonRoot = Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    // BlockPlugin.h and the block ABI headers live in nflow_blocks; the generated
    // plugin CMake resolves ${NFLOW_ROOT}/src/include against this module.
    std::wstring nflowBlocksRoot = nelsonRoot + L"/modules/nflow_blocks";
    // Use forward slashes for cmake compatibility
    std::replace(nflowBlocksRoot.begin(), nflowBlocksRoot.end(), L'\\', L'/');
    return L"-DNFLOW_ROOT=\"" + nflowBlocksRoot + L"\"";
}
//=============================================================================
} // namespace nflow
//=============================================================================
