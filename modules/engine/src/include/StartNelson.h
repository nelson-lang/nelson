//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
NLSENGINE_IMPEXP int
StartNelson(int argc, char* argv[], NELSON_ENGINE_MODE _mode);
NLSENGINE_IMPEXP int
StartNelson(int argc, wchar_t* argv[], NELSON_ENGINE_MODE _mode);
//=============================================================================
