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
#include <vector>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Reference line (xline / yline). A ConstantLine spans the whole axes along one
// data direction at a constant coordinate on the perpendicular InterceptAxis.
class NLSGRAPHICS_IMPEXP GOConstantLine : public GraphicsObject
{
    //=============================================================================
private:
    void
    paintLabel(RenderInterface& gc, bool vertical, double mappedX, double mappedY,
        const std::vector<double>& xLimits, const std::vector<double>& yLimits);
    //=============================================================================
public:
    GOConstantLine();
    //=============================================================================
    ~GOConstantLine() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_CONSTANT_LINE_STR;
    }
    //=============================================================================
    void
    constructProperties();
    //=============================================================================
    void
    setupDefaults();
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    std::vector<double>
    getLimits() override;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
