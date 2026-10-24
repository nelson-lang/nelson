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
#include "GOGroup.hpp"
//=============================================================================
namespace Nelson {
// A transform group (hgtransform). It behaves like a group but owns a 4x4
// homogeneous Matrix property that geometrically transforms its children (and,
// through nesting, every enclosing transform) at render time: paintMe composes
// the Matrix into the renderer modelview around the painting of the children.
class NLSGRAPHICS_IMPEXP GOTransform : public GOGroup
{
public:
    GOTransform();
    ~GOTransform() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    paintMe(RenderInterface& gc) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
