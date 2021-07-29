//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    CreateGuiEvaluator(void* vcontext, NELSON_ENGINE_MODE _mode, bool minimizeWindow);
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
