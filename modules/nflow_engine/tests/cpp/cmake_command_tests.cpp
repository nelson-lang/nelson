//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "NFlowCMakeCommand.hpp"
//=============================================================================
using namespace nflow;
//=============================================================================
// Regression: the bundled cmake path can contain spaces and parentheses, e.g.
// "D:\Program Files\Nelson-2.0.0.0 (64 bits)\tools\cmake\bin\". The command runs
// through cmd.exe, so the executable must be quoted. Unquoted, cmd.exe splits the
// path at the first space and the FMU binary build fails.
//=============================================================================
TEST_CASE("cmake executable is always quoted")
{
    const std::wstring dir = L"D:/Program Files/Nelson-2.0.0.0 (64 bits)/tools/cmake/bin/";
    const std::wstring command = buildCMakeCommand(dir, L"--version");
    CHECK(command
        == L"\"D:/Program Files/Nelson-2.0.0.0 (64 bits)/tools/cmake/bin/cmake\" --version");
    // The quotes wrap directory and executable name, so cmd.exe sees one token.
    CHECK(command.front() == L'"');
    CHECK(command.find(L"cmake\"") != std::wstring::npos);
}
//=============================================================================
TEST_CASE("cmake command quotes the executable even with no bin directory")
{
    // Empty directory (use PATH): still quoted.
    const std::wstring command = buildCMakeCommand(L"", L"--build \"C:/tmp/build\"");
    CHECK(command == L"\"cmake\" --build \"C:/tmp/build\"");
}
//=============================================================================
TEST_CASE("cmake command with no arguments has no trailing space")
{
    const std::wstring command = buildCMakeCommand(L"/opt/nelson/tools/cmake/bin/", L"");
    CHECK(command == L"\"/opt/nelson/tools/cmake/bin/cmake\"");
}
//=============================================================================
