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
#include "nlsEngine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Writes the cause and the call stack on stderr when the process is about to
// stop on a fatal error. Windows release builds only, a no-op elsewhere.
NLSENGINE_IMPEXP void
installFatalErrorReport();
//=============================================================================
} // namespace Nelson
//=============================================================================
