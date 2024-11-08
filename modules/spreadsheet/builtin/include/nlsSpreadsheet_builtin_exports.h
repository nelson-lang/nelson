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
#ifdef _MSC_VER
#ifdef NLSSPREADSHEET_BUILTIN_EXPORTS
#define NLSSPREADSHEET_BUILTIN_IMPEXP __declspec(dllexport)
#else
#define NLSSPREADSHEET_BUILTIN_IMPEXP __declspec(dllimport)
#endif
#else
#define NLSSPREADSHEET_BUILTIN_IMPEXP __attribute__((visibility("default")))
#endif
//=============================================================================
