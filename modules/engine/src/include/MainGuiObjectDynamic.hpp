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
//===================================================================================
#include <stddef.h>
#include "NelSon_engine_mode.h"
//===================================================================================
namespace Nelson {
void
InitGuiObjectsDynamic();
void*
CreateGuiEvaluatorDynamic(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow, size_t ID);
void
DestroyMainGuiObjectDynamic(void* term);
void*
GetMainGuiObjectDynamic();
} // namespace Nelson
//===================================================================================
