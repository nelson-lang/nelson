//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
namespace nflow {
//=============================================================================
// Builds the invocation line for the (possibly bundled) cmake executable.
// cmakeBinDir ends with a separator (empty = use PATH). The bundled path can
// contain spaces and parentheses, e.g. "...\Nelson-2.0.0.0 (64 bits)\tools\cmake\bin\".
// The command runs through cmd.exe, so the executable is quoted. Unquoted, the
// call fails and NFlow falls back to a source-only FMU that cannot be instantiated.
// args is appended verbatim after one space and must already quote its paths.
//=============================================================================
inline std::wstring
buildCMakeCommand(const std::wstring& cmakeBinDir, const std::wstring& args)
{
    std::wstring command = L"\"" + cmakeBinDir + L"cmake\"";
    if (!args.empty()) {
        command += L" " + args;
    }
    return command;
}
//=============================================================================
} // namespace nflow
//=============================================================================
