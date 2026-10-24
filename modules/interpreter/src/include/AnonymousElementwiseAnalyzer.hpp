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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class AnonymousMacroFunctionDef;
//=============================================================================
/**
 * Test whether the body of an anonymous function is a provably element-wise
 * expression.
 *
 * A callback such as `@(x,y) sqrt(x.^2 + y.^2)` applied element by element
 * (e.g. by arrayfun/bsxfun) sees scalar arguments. Evaluating a provably
 * element-wise body ONCE on the full same-size input arrays gives the same
 * result and skips the per-element VM dispatch.
 *
 * This is valid only when every operator and function in the body behaves the
 * same on a whole array as on a scalar. The predicate is STRICT and rejects
 * anything whose array semantics differ from its scalar semantics, e.g.:
 *   - matrix operators `*` `/` `\` `^`, transpose `'` `.'`, ranges `:`
 *   - indexing a parameter/capture, concatenation `[ ]`, field access
 *   - reductions and any non element-wise function (sum, prod, sort, ...)
 *   - short-circuit `&&` / `||` (require scalar operands)
 *   - captured variables that are not real numeric/logical scalars
 * Only `+ - .* ./ .\ .^`, relational/element-wise logical operators, and a
 * whitelist of strictly scalar->scalar functions are accepted.
 *
 * Returns false for named function handles (no inspectable body) and for any
 * body that is not provably element-wise. Callers then use their per-element
 * path. The predicate never gives a false positive.
 */
NLSINTERPRETER_IMPEXP bool
isAnonymousBodyElementwiseVectorizable(AnonymousMacroFunctionDef* def);
//=============================================================================
/**
 * Stricter variant of isAnonymousBodyElementwiseVectorizable for callers that
 * rely on singleton-dimension broadcasting (e.g. bsxfun), where operands may
 * differ in size. It additionally rejects the element-wise functions that do
 * NOT broadcast in Nelson (min, max, mod, rem), so a one-shot broadcast
 * evaluation of the body cannot newly fail where a same-size expansion would
 * have succeeded.
 */
NLSINTERPRETER_IMPEXP bool
isAnonymousBodyElementwiseBroadcastSafe(AnonymousMacroFunctionDef* def);
//=============================================================================
/**
 * Test whether a NAMED function handle (e.g. @sin, @plus) is a standard
 * element-wise builtin whose result has the same shape as its operands when
 * they are the same size (as arrayfun guarantees). numArgs is the number of
 * array inputs: 1 selects the unary element-wise functions, 2 the binary
 * element-wise builtins (including max/min/mod/rem/xor, which are element-wise
 * on same-size operands). Returns false for anonymous bodies (use the body
 * predicate above), for any other arity, and for non element-wise builtins.
 */
NLSINTERPRETER_IMPEXP bool
isNamedHandleElementwiseSameSize(AnonymousMacroFunctionDef* def, size_t numArgs);
//=============================================================================
} // namespace Nelson
//=============================================================================
