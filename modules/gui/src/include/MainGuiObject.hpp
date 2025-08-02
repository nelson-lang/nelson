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
//===================================================================================
#if _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelSon_engine_mode.h"
#include "nlsGui_exports.h"
//===================================================================================
extern "C"
{
    NLSGUI_IMPEXP void
    InitGuiObjects(void);
    NLSGUI_IMPEXP void*
    CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow, size_t ID);
    NLSGUI_IMPEXP void
    DestroyMainGuiObject(void* term);
    NLSGUI_IMPEXP void*
    GetMainGuiObject(void);
    NLSGUI_IMPEXP void
    QtMessageVerbose(bool bVerbose);
    NLSGUI_IMPEXP bool
    IsQtMessageVerbose();
    NLSGUI_IMPEXP bool
    QtSetLookAndFeel(const std::wstring& lf);
    NLSGUI_IMPEXP std::wstring
    QtGetLookAndFeel();
    NLSGUI_IMPEXP std::wstring
    QtGetStyleSheet();
    NLSGUI_IMPEXP void
    QtSetStyleSheet(const std::wstring& styleSheet);
};
//===================================================================================
