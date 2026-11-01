//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::DebuggerGateway {
//=============================================================================
// A dbstop/dbclear/dbstatus target: "name", "main>local" or a (quoted) file path.
struct BreakpointTarget
{
    // full path of the file holding the code
    std::wstring filename;
    // function owning the breakpoint ("" for scripts and classdef files)
    std::string functionName;
    // name accepted by Evaluator::adjustBreakpointLine ("main>local" or the file)
    std::wstring adjustName;
    bool isLocalFunction = false;
};
//=============================================================================
// Resolves a target name; errorMessage holds the text on failure.
bool
resolveBreakpointTarget(Evaluator* eval, const std::wstring& nameOrFile, BreakpointTarget& target,
    std::wstring& errorMessage);
//=============================================================================
// sets a breakpoint; position 0 means the first executable line; created tells if it is new
bool
setBreakpointAt(Evaluator* eval, const std::wstring& nameOrFile, size_t position,
    bool explicitPosition, size_t& adjustedLine, bool& created, std::wstring& errorMessage);
//=============================================================================
// Command-syntax arguments as text tokens (numbers are converted).
wstringVector
breakpointCommandTokens(const ArrayOfVector& argIn);
//=============================================================================
// Parses a line number token; returns false when the token is not a number.
bool
parseBreakpointLineToken(const std::wstring& token, size_t& line);
//=============================================================================
// Same file test as the evaluator (slashes normalized, case kept on POSIX).
bool
sameBreakpointFile(const std::wstring& path1, const std::wstring& path2);
//=============================================================================
} // namespace Nelson::DebuggerGateway
//=============================================================================
