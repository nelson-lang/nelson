// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "RuntimeLibrarySearch.hpp"
#include <dlfcn.h>
#include <iostream>

namespace {
namespace fs = std::filesystem;
namespace search = Nelson::RuntimeLibrarySearch;

void
require(bool condition, const char* message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}

std::string
environment(const char* name)
{
    const auto* value = std::getenv(name);
    return value == nullptr ? "" : value;
}

void
setEnvironment(const char* name, const std::string& value)
{
    require(setenv(name, value.c_str(), 1) == 0, "Cannot set probe environment.");
}

void
checkLibrary(const fs::path& library)
{
    void* handle = dlopen(library.c_str(), RTLD_NOW | RTLD_LOCAL);
    if (handle == nullptr) {
        throw std::runtime_error(dlerror());
    }
    const auto value = reinterpret_cast<int (*)()>(dlsym(handle, "runtime_search_value"));
    require(
        value != nullptr && value() == 42, "Dependent native library was not loaded correctly.");
    require(dlclose(handle) == 0, "Cannot unload native probe.");
}

void
checkFailedExec(const fs::path& libraries, const std::vector<std::string>& arguments)
{
    const auto absent = fs::current_path() / "nonexistent-launcher";
    require(!fs::exists(absent), "The missing-launcher fixture already exists.");
    const bool wasSet = std::getenv("LD_LIBRARY_PATH") != nullptr;
    const auto inherited = environment("LD_LIBRARY_PATH");
    bool failed = false;
    try {
        search::initialize(absent, libraries, arguments);
    } catch (const std::system_error& error) {
        require(error.code().value() == ENOENT, "Unexpected restart error.");
        failed = true;
    }
    require(failed, "Missing executable did not fail.");
    require(wasSet == (std::getenv("LD_LIBRARY_PATH") != nullptr)
            && inherited == environment("LD_LIBRARY_PATH"),
        "Failed restart did not restore the original environment.");
}

void
checkBootstrap(int argc, char* argv[])
{
    require(argc == 6 && std::string(argv[3]).empty()
            && std::string(argv[4]) == "argument with spaces"
            && std::string(argv[5]) == "--literal=$value",
        "Bootstrap changed the argument vector.");
    const std::string mode = argv[1];
    const auto executable = fs::canonical(argv[0]);
    const auto library = fs::canonical(argv[2]);
    const auto libraries = library.parent_path();
    const std::vector<std::string> arguments(argv + 1, argv + argc);
    if (mode == "failed-exec" || mode == "failed-exec-empty" || mode == "failed-exec-inherited") {
        checkFailedExec(libraries, arguments);
        return;
    }
    const auto stage = environment("NELSONC_SEARCH_PROBE_STAGE");
    if (stage.empty()) {
        const auto expected
            = search::linuxLibraryPath(libraries.native(), environment("LD_LIBRARY_PATH"));
        setEnvironment("NELSONC_SEARCH_PROBE_CWD", fs::current_path().native());
        setEnvironment("NELSONC_SEARCH_PROBE_PATH", expected);
        setEnvironment(
            "NELSONC_SEARCH_PROBE_STAGE", mode == "already" ? "must-not-restart" : "once");
        if (mode != "already") {
            void* handle = dlopen(library.c_str(), RTLD_NOW | RTLD_LOCAL);
            if (handle != nullptr) {
                dlclose(handle);
                throw std::runtime_error(
                    "Native dependency was already discoverable before bootstrap.");
            }
        }
        search::initialize(executable, libraries, arguments);
        require(mode == "already", "Native library search did not restart the process.");
    } else {
        require(stage == "once", "Native library bootstrap restarted more than once.");
        setEnvironment("NELSONC_SEARCH_PROBE_STAGE", "must-not-restart");
        search::initialize(executable, libraries, arguments);
    }
    require(fs::current_path().native() == environment("NELSONC_SEARCH_PROBE_CWD"),
        "Bootstrap changed the invoking directory.");
    require(environment("LD_LIBRARY_PATH") == environment("NELSONC_SEARCH_PROBE_PATH"),
        "Bootstrap lost the inherited native search path.");
    checkLibrary(library);
}
}

int
main(int argc, char* argv[])
{
    try {
        checkBootstrap(argc, argv);
        std::cout << "NATIVE_RUNTIME_SEARCH_OK\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
