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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOAxesToolbar : public GraphicsObject
{
public:
    GOAxesToolbar();
    ~GOAxesToolbar() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_AXES_TOOLBAR_STR;
    }
    void
    paintMe(RenderInterface& gc) override;

private:
    void
    constructProperties();
    void
    setupDefaults();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
