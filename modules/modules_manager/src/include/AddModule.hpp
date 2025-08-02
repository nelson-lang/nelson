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
#include "nlsModules_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSMODULES_MANAGER_IMPEXP void
AddModule(Evaluator* eval, const std::wstring& modulerootpath, const std::wstring& moduleshortname);
}
//=============================================================================
