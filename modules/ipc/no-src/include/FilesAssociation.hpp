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
#include "nlsIpc_exports.h"
#include "Types.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSIPC_IMPEXP
bool
OpenFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC);
//=============================================================================
NLSIPC_IMPEXP
bool
LoadFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC);
//=============================================================================
NLSIPC_IMPEXP
bool
ExecuteFilesAssociated(
    NELSON_ENGINE_MODE currentMode, const wstringVector& filesToOpen, bool sendByIPC);
//=============================================================================
}
//=============================================================================
