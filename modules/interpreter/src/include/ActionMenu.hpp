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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
NLSINTERPRETER_IMPEXP void
doExit();
NLSINTERPRETER_IMPEXP void
doHelp();
NLSINTERPRETER_IMPEXP void
doPause();
NLSINTERPRETER_IMPEXP void
doStop();
}; // namespace Nelson
//=============================================================================
