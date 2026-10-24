//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
#include "GOPropertyValues.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Stairstep chart primitive. XData/YData are already expanded into the staircase
// vertices by stairs.m, so painting is a plain 2-D polyline. A dedicated class is
// needed so class(h) reports nelson.graphics.chart.primitive.Stair (not Line) and
// the properties stay 2-D only (no Z/Theta/R data, no line-join/marker-indices
// extras).
class NLSGRAPHICS_IMPEXP GOStair : public GraphicsObject
{
public:
    GOStair();
    ~GOStair() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_STAIR_STR;
    }
    void
    constructProperties();
    void
    setupDefaults();
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;

private:
    std::vector<double> limits;
    mutable bool limitsDirty = true;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
