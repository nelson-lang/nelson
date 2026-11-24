//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebUiHelpers.hpp"
#include <filesystem>
#include <iostream>
//=============================================================================
int
main()
{
    const std::filesystem::path sourceRoot(NELSON_SOURCE_ROOT);
    const std::filesystem::path expected
        = sourceRoot / "desktop" / "icons" / "hicolor" / "512x512" / "apps" / "nelson.png";
    const std::string resolved = Nelson::findNelsonApplicationIconPath(sourceRoot.wstring());

    if (std::filesystem::path(resolved).lexically_normal() != expected.lexically_normal()) {
        std::cerr << "Unexpected application icon path: " << resolved << '\n';
        return 1;
    }
    if (!Nelson::findNelsonApplicationIconPath(L"").empty()) {
        std::cerr << "An empty root must not resolve an icon.\n";
        return 1;
    }
    if (!Nelson::findNelsonApplicationIconPath((sourceRoot / "missing-application-root").wstring())
            .empty()) {
        std::cerr << "A missing root must not resolve an icon.\n";
        return 1;
    }
    return 0;
}
//=============================================================================
