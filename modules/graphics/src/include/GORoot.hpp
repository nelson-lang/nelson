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
class NLSGRAPHICS_IMPEXP GORoot : public GraphicsObject
{
private:
    void
    refreshCurrentFigureProperty();
    void
    refreshChildrenProperty();
    void
    refreshScreenSizeProperty();
    void
    refreshScreenDepthProperty();
    void
    refreshScreenPixelsPerInchProperty();

public:
    GORoot();
    ~GORoot() override = default;
    virtual std::wstring
    getType() override;
    virtual void
    constructProperties();
    void
    updateState() override;
    void
    setupDefaults();
    void
    loadDefaultColorMap();
    void
    loadDefaultAlphaMap();
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
NLSGRAPHICS_IMPEXP int64
graphicsRootObject();
//=============================================================================
NLSGRAPHICS_IMPEXP GORoot*
getGraphicsRootObject();
//=============================================================================
}
//=============================================================================
