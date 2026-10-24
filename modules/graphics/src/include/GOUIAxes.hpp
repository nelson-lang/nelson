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
#include <string>
#include "nlsGraphics_exports.h"
#include "GOAxis.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Axes for uifigure-based apps. Reuses GOAxis (projection, limits, children,
// painting, pixel InnerPosition/OuterPosition sync) and only reports its own
// class name. getType() stays the inherited "axes" so isType() and the Type
// property still treat it as an axes.
class NLSGRAPHICS_IMPEXP GOUIAxes : public GOAxis
{
public:
    //=============================================================================
    GOUIAxes() = default;
    //=============================================================================
    ~GOUIAxes() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
