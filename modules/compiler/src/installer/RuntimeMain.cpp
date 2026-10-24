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
#include <iostream>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif
namespace {
Nelson::RuntimeInstaller::fs::path
runtimeRoot(const char* value)
{
    namespace R = Nelson::RuntimeInstaller;
    const auto path = R::fs::u8path(value);
    R::safePath(path);
    auto result = path.lexically_normal();
    if (result.has_relative_path() && result.filename().empty()) {
        result = result.parent_path();
    }
    return result;
}
//=============================================================================
Nelson::RuntimeInstaller::fs::path
selfPath()
{
#ifdef __APPLE__
    uint32_t size = 0;
    _NSGetExecutablePath(nullptr, &size);
    std::string value(size, '\0');
    Nelson::RuntimeInstaller::require(
        _NSGetExecutablePath(value.data(), &size) == 0, "Cannot locate the runtime uninstaller.");
    return Nelson::RuntimeInstaller::fs::canonical(value.c_str());
#else
    return Nelson::RuntimeInstaller::fs::read_symlink("/proc/self/exe");
#endif
}
//=============================================================================
} // namespace
//=============================================================================
int
main(int argc, char** argv)
{
    namespace R = Nelson::RuntimeInstaller;
    try {
        if (argc == 3 && std::string(argv[1]) == "--prepare") {
            R::prepare(R::fs::u8path(argv[2]));
        } else if (argc == 5 && std::string(argv[1]) == "--install") {
            R::install(R::fs::u8path(argv[2]), runtimeRoot(argv[3]), R::fs::u8path(argv[4]));
        } else if (argc == 3 && std::string(argv[1]) == "--uninstall") {
            R::uninstall(runtimeRoot(argv[2]));
        } else if (argc == 1) {
            const auto self = selfPath();
            R::require(
                self.filename() == "uninstall" && self.parent_path().filename() == R::metadata,
                "Invoke the installed runtime uninstaller without arguments.");
            R::uninstall(self.parent_path().parent_path());
        } else {
            throw std::runtime_error("Invalid internal runtime installer command.");
        }
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "Nelson runtime installer: " << error.what() << '\n';
        return 1;
    }
}
//=============================================================================
