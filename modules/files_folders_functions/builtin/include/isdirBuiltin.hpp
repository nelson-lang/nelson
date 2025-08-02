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
#include "nlsFiles_folders_functions_builtin_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::FilesFoldersGateway {
//=============================================================================
NLSFILES_FOLDERS_FUNCTIONS_BUILTIN_IMPEXP
ArrayOfVector
isdirBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
