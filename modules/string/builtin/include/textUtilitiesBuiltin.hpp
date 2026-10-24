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
#include "nlsString_builtin_exports.h"
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::StringGateway {
//=============================================================================
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
findstrBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
splitBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
splitlinesBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strjoinBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strmatchBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strreadBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strsplitBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strtokBuiltin(int nLhs, const ArrayOfVector& argIn);
NLSSTRING_BUILTIN_IMPEXP ArrayOfVector
strvcatBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::StringGateway
//=============================================================================
