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
#include "nlsPython_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP void
initializePythonEngine();
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP bool
PyIsInitializedInterpreter(void);
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP bool
PyInitializeInterpreter(void);
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP bool
PyFinalizeInterpreter(void);
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP void*
getPythonInterpreter(void);
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP std::wstring
getPythonStandardOutput();
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP std::wstring
getPythonStandardError();
//=============================================================================
}
//=============================================================================
