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
#include <stdbool.h>
#include <stddef.h>
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    bool
    NelsonRenderHtmlWithWebKit(const char* html, char* output, size_t outputSize);
#ifdef __cplusplus
}
#endif
//=============================================================================
