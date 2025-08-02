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
#ifdef _MSC_VER
#ifdef NLSI18N_BUILTIN_EXPORTS
#define NLSI18N_BUILTIN_IMPEXP __declspec(dllexport)
#else
#define NLSI18N_BUILTIN_IMPEXP __declspec(dllimport)
#endif
#else
#define NLSI18N_BUILTIN_IMPEXP __attribute__((visibility("default")))
#endif
//=============================================================================
