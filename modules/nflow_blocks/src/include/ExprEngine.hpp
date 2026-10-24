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
#include <unordered_map>
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    /**
     * Evaluate a mathematical expression.
     *
     * Input JSON: { "expr": "2 + x", "variables": { "x": 3.0 } }
     * Returns JSON:
     *   { "ok": true,  "value": 5.0 }
     *   { "ok": false, "error": "reason" }
     */
    NLSNFLOW_BLOCKS_IMPEXP std::string
    evalExpressionJson(const std::string& json);
    //=============================================================================
    /**
     * Direct C++ entry point of the same evaluator (single expression engine
     * for the whole module: UI binding, simulation parameters and codegen).
     * Throws std::runtime_error on invalid expressions.
     */
    NLSNFLOW_BLOCKS_IMPEXP double
    evalExpressionValue(
        const std::string& expr, const std::unordered_map<std::string, double>& variables);
    //=============================================================================
    /**
     * Emit a (validated) math expression as an equivalent scalar C or Rust
     * expression, reconstructed from the parsed form so it matches the
     * interpreter's semantics - notably "^" becomes pow() (it is XOR in C/Rust)
     * and functions/constants map to their C/Rust forms. `lang` is "c" or
     * "rust"; every occurrence of the identifier "u" is replaced by
     * `uReplacement`. Returns "" when the expression cannot be emitted safely
     * (unknown function, wrong arity), so callers fall back to a safe default.
     */
    NLSNFLOW_BLOCKS_IMPEXP std::string
    emitMathExpressionCodegen(
        const std::string& expr, const std::string& lang, const std::string& uReplacement);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
