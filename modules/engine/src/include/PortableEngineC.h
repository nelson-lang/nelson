//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
#ifdef __cplusplus
extern "C"
{
#endif
    NLSENGINE_IMPEXP int
    nlsPortableStart();
    NLSENGINE_IMPEXP int
    nlsPortableLoadUserModules();
    NLSENGINE_IMPEXP int
    nlsPortableEvaluate(const char* command);
    NLSENGINE_IMPEXP const char*
    nlsPortableAnalyzeCode(const char* path, const char* source);
    NLSENGINE_IMPEXP const char*
    nlsPortableDebuggerState();
    NLSENGINE_IMPEXP const char*
    nlsPortableDebuggerGetBreakpoints(const char* file);
    NLSENGINE_IMPEXP const char*
    nlsPortableDebuggerToggleBreakpoint(const char* file, int line);
    NLSENGINE_IMPEXP const char*
    nlsPortableStdout();
    NLSENGINE_IMPEXP const char*
    nlsPortableStderr();
    NLSENGINE_IMPEXP const char*
    nlsPortableErrorIdentifier();
    NLSENGINE_IMPEXP const char*
    nlsPortableErrorMessage();
    NLSENGINE_IMPEXP int
    nlsPortableInterrupted();
    NLSENGINE_IMPEXP void
    nlsPortableRequestInterrupt();
    NLSENGINE_IMPEXP void
    nlsPortablePump();
    NLSENGINE_IMPEXP void
    nlsPortableReset();
    NLSENGINE_IMPEXP void
    nlsPortableStop();
#ifdef __cplusplus
}
#endif
//=============================================================================
