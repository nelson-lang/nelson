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
#include "nlsGui_builtin_exports.h"
#include <string>
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
NLSGUI_BUILTIN_IMPEXP ArrayOfVector
examplesBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
NLSGUI_BUILTIN_IMPEXP void
closeExamplesWindow();
//=============================================================================
NLSGUI_BUILTIN_IMPEXP void
getExamplesWindowState(size_t& window, size_t& generation, std::string& catalogJson);
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
