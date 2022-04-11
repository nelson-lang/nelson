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
#include "nlsHistory_manager_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSHISTORY_MANAGER_IMPEXP void
    NelsonHistorySetToken(const char* token);
    NLSHISTORY_MANAGER_IMPEXP const char*
    NelsonHistoryGetNextLine(void);
    NLSHISTORY_MANAGER_IMPEXP const char*
    NelsonHistoryGetPreviousLine(void);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
