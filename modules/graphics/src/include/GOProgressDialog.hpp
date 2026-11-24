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
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
#define GO_PROPERTY_VALUE_PROGRESSDIALOG_STR L"ProgressDialog"
//=============================================================================
namespace Nelson {
//=============================================================================
// Non-blocking progress dialog graphics object returned by uiprogressdlg. It is
// not drawn on a figure canvas: its state is served to the desktop through
// the dialog provider (kind == "progress"). Setting a property triggers
// updateState(), which re-publishes the state.
//=============================================================================
class NLSGRAPHICS_IMPEXP GOProgressDialog : public GraphicsObject
{
public:
    GOProgressDialog();
    ~GOProgressDialog() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return "ProgressDialog";
    }
    void
    constructProperties();
    void
    setupDefaults();
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;

private:
    long long m_id;
    bool m_hidden = false;
    void
    publish(bool visible);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
