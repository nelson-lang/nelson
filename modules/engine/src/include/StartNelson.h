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
#include "NelSon_engine_mode.h"
#include "nlsEngine_exports.h"
//=============================================================================
extern "C"
{
    NLSENGINE_IMPEXP int
    StartNelsonW(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode);
    NLSENGINE_IMPEXP int
    StartNelsonA(int argc, char* argv[], NELSON_ENGINE_MODE _mode);
};
//=============================================================================
