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
#include <unordered_set>
#include <utility>
#include <vector>
#include "FunctionDef.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Names the body of an anonymous function may read from the workspace it is
 * created in: every identifier of content (quoted text excluded) that is not
 * one of its arguments. A handle captures only those variables, so it never
 * keeps the rest of the workspace alive; a name that is not a variable is
 * resolved as a function at call time.
 *
 * The scan errs on the side of reporting too many names: an extra capture is
 * harmless, a missing one is not.
 */
NLSINTERPRETER_IMPEXP std::unordered_set<std::string>
anonymousReferencedNames(const std::string& content, const stringVector& arguments);
//=============================================================================
/**
 * True when the body refers to a name that is not one of the captured
 * variables, so it may need the local functions and file of the defining
 * scope to resolve it.
 */
NLSINTERPRETER_IMPEXP bool
anonymousContentMayNeedFunctionScope(
    const std::unordered_set<std::string>& referencedNames, const stringVector& captures);
//=============================================================================
/**
 * True when the body refers to a nested function of the defining scope. A
 * nested function reads the workspace of its parent, so a handle calling one
 * must capture that workspace whole, not only the names of its own body.
 */
NLSINTERPRETER_IMPEXP bool
anonymousReferencesNestedFunction(const std::unordered_set<std::string>& referencedNames,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions);
//=============================================================================
} // namespace Nelson
//=============================================================================
