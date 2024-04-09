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
#include <string>
#include "nlsPython_engine_exports.h"
#include "ArrayOf.hpp"
#include "Interface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP ArrayOfVector
PyRun(Interface* io, bool haveEventsLoop, const void* voidPythonObjectHandle,
    const wstringVector& commands, const wstringVector& outputs, const wstringVector& names,
    const ArrayOfVector& values);
//=============================================================================
}
//=============================================================================
