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
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOMenu : public GraphicsObject
{
public:
    GOMenu();
    ~GOMenu() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_MENU_STR;
    }
    void
    paintMe(RenderInterface& gc) override;
    void
    updateState() override;

private:
    void
    constructProperties();
    void
    setupDefaults();
    void
    reorderParentChildren();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
