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
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP bool
printfFunction(const ArrayOfVector& args, std::wstring& errorMessage, std::wstring& result);
} // namespace Nelson
//=============================================================================
