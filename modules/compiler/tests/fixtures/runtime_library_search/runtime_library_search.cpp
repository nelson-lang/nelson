// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "RuntimeLibrarySearch.hpp"
#include <iostream>

namespace search = Nelson::RuntimeLibrarySearch;

namespace {
void
require(bool condition, const char* message)
{
    if (!condition) {
        throw std::runtime_error(message);
    }
}

void
checkPath(const std::string& inherited, const std::string& expected)
{
    const auto result = search::linuxLibraryPath("/runtime/lib", inherited);
    require(result == expected, "Unexpected native library path.");
    require(search::linuxLibraryPath("/runtime/lib", result) == result,
        "Native library preparation must be idempotent.");
}

void
checkInvalidPaths()
{
    for (const auto& directory :
        std::vector<std::string> { "", "relative", "C:/runtime", "/runtime:other", "/runtime;other",
            "/runtime/$ORIGIN", "/runtime/${LIB}", std::string("/runtime\0suffix", 15) }) {
        bool rejected = false;
        try {
            search::linuxLibraryPath(directory, "");
        } catch (const std::runtime_error&) {
            rejected = true;
        }
        require(rejected, "An unrepresentable native directory was accepted.");
    }
    bool rejected = false;
    try {
        search::linuxLibraryPath("/runtime/lib", std::string("/existing\0suffix", 16));
    } catch (const std::runtime_error&) {
        rejected = true;
    }
    require(rejected, "An inherited native path with an embedded null was accepted.");
}

void
checkArguments()
{
    std::vector<std::string> arguments { "/app path/app", "", "a b", "'quoted'", "--",
        "dollar$colon:semi;", "line\nvalue", "utf8-\xc3\xa9" };
    const auto pointers = search::argumentPointers(arguments);
    require(pointers.size() == arguments.size() + 1 && pointers.back() == nullptr,
        "Missing native argument sentinel.");
    for (size_t i = 0; i < arguments.size(); ++i) {
        require(pointers[i] != nullptr && pointers[i] == arguments[i].data()
                && std::string(pointers[i]) == arguments[i],
            "Native argument was changed.");
    }
    arguments.clear();
    const auto empty = search::argumentPointers(arguments);
    require(
        empty.size() == 1 && empty[0] == nullptr, "Empty native argument list is not terminated.");
}
}

int
main()
{
    try {
        checkPath("", "/runtime/lib");
        checkPath("/existing", "/runtime/lib:/existing");
        checkPath("/runtime/lib", "/runtime/lib");
        checkPath("/runtime/lib:/existing", "/runtime/lib:/existing");
        checkPath("/runtime/lib;/existing", "/runtime/lib;/existing");
        checkPath("/runtime/lib-extra", "/runtime/lib:/runtime/lib-extra");
        checkPath(":/existing:", "/runtime/lib::/existing:");
        checkPath("/existing;/another", "/runtime/lib:/existing;/another");
        checkPath("$ORIGIN/existing", "/runtime/lib:$ORIGIN/existing");
        require(search::linuxLibraryPath("/runtime space/utf8-\xc3\xa9", "")
                == "/runtime space/utf8-\xc3\xa9",
            "Spaces or UTF-8 bytes were changed.");
        checkInvalidPaths();
        checkArguments();
        std::cout << "RUNTIME_LIBRARY_SEARCH_OK\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
