// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "SharedRuntimeRegistry.hpp"
#include <iostream>

int
main(int argc, char** argv)
{
    if (argc != 2) {
        return 2;
    }
#ifdef __linux__
    if (std::string(argv[1]) == "--directories") {
        for (const auto& directory :
            Nelson::SharedRuntimeRegistry::Posix::configurationDirectories()) {
            std::cout << directory.u8string() << '\n';
        }
        return 0;
    }
#endif
    for (const auto& root : Nelson::SharedRuntimeRegistry::roots(argv[1])) {
        std::cout << root.u8string() << '\n';
    }
    return 0;
}
