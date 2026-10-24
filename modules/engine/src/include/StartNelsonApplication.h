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
#include <stdint.h>
#include <stddef.h>
#include "nlsEngine_exports.h"
#define NELSON_APPLICATION_STARTUP_VERSION 1U
#define NELSON_APPLICATION_COMPILER 1U
#define NELSON_APPLICATION_GUI 2U
#ifdef _WIN32
typedef wchar_t NelsonApplicationChar;
#else
typedef char NelsonApplicationChar;
#endif
// Borrowed native strings remain valid until the call returns. No allocator or
// C++ object crosses the launcher/runtime boundary. argv excludes the executable.
typedef struct NelsonApplicationStartup
{
    uint32_t size;
    uint32_t version;
    uint32_t flags;
    int argc;
    const NelsonApplicationChar* const* argv;
    const NelsonApplicationChar* executable;
    const NelsonApplicationChar* root;
    const NelsonApplicationChar* libraries;
    const NelsonApplicationChar* binaries;
    const NelsonApplicationChar* invokingDirectory;
} NelsonApplicationStartup;
//=============================================================================
typedef int (*NelsonApplicationStartFunction)(const NelsonApplicationStartup*);
#ifdef _WIN32
typedef int (*NelsonApplicationOutputFunction)(intptr_t, intptr_t);
#endif
#ifdef __cplusplus
extern "C"
{
#endif
    NLSENGINE_IMPEXP int
    StartNelsonApplicationV1(const NelsonApplicationStartup* startup);
#ifdef _WIN32
    NLSENGINE_IMPEXP int
    ConfigureNelsonApplicationOutputV1(intptr_t output, intptr_t error);
#endif
#ifdef __cplusplus
}
#endif
//=============================================================================
