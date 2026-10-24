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
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOViolinPlot : public GraphicsObject
{
public:
    // One violin: its samples and their (unscaled) kernel density.
    struct Group
    {
        double position { 0. };
        std::vector<double> values;
        std::vector<double> centers;
        std::vector<double> densities;
        double bandwidth { 0. };
        double maxDensity { 0. };
    };

private:
    std::vector<Group> groups;
    bool shapesDirty { true };
    bool manualPoints { false };
    void
    refreshShapes();

public:
    GOViolinPlot();
    ~GOViolinPlot() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_VIOLIN_PLOT_STR;
    }
    void
    paintMe(RenderInterface& gc) override;
    void
    updateState() override;
    std::vector<double>
    getLimits() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
