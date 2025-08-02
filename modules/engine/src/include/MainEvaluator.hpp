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
#include "Evaluator.hpp"
#include "NelSon_engine_mode.h"
#include "nlsEngine_exports.h"
//=============================================================================
namespace Nelson {
NLSENGINE_IMPEXP Evaluator*
createMainEvaluator(NELSON_ENGINE_MODE _mode, const std::wstring& lang, bool minimizeWindow);
//=============================================================================
NLSENGINE_IMPEXP bool
destroyMainEvaluator();
//=============================================================================
} // namespace Nelson
//=============================================================================
