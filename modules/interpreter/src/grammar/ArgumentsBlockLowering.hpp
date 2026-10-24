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
#include <string>
#include <vector>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Result of lowering a function's arguments validation blocks to plain Nelson
// statements. This is the compile-time counterpart of the historical pre-lexer
// text rewrite: it produces the exact same validation code, byte-for-byte, but
// from the arguments blocks captured natively by the grammar instead of from a
// source-to-source rewrite of the whole buffer.
//=============================================================================
struct ArgumentsLoweringResult
{
    // Input/structural validation statements, single-lined, ready to be parsed
    // and spliced at the top of the function body (they run at call time, before
    // the body executes).
    std::string generatedCode;
    // Output (arguments (Output)) validation statements, appended at the end of
    // the function body so they run after the outputs have been assigned.
    std::string outputValidationCode;
    // Input parameter list after the varargin signature patch (only meaningful
    // when paramsPatched is true).
    std::vector<std::string> patchedParams;
    bool paramsPatched = false;
};
//=============================================================================
// Lower the arguments blocks of a single function.
//   blockTexts:      raw source text of each "arguments ... end" block, in the
//                    order they appear in the function body. Each string starts
//                    with the "arguments" keyword and ends with the block "end".
//   signatureParams: the declared input parameter names of the function.
// The returned generatedCode preserves the runtime-timing invariant of the old
// text rewrite (every structural violation is reported at call time, never at
// parse/load time) and the exact error strings.
//=============================================================================
NLSINTERPRETER_IMPEXP ArgumentsLoweringResult
lowerArgumentsBlocks(
    const std::vector<std::string>& blockTexts, const std::vector<std::string>& signatureParams);
//=============================================================================
} // namespace Nelson
//=============================================================================
