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
class NLSGRAPHICS_IMPEXP GORainCloudPlot : public GraphicsObject
{
public:
    // One group: the cloud (half violin) and the rain (jittered samples).
    struct Group
    {
        double position { 0. };
        std::vector<double> values;
        std::vector<double> centers;
        std::vector<double> widths;
        std::vector<double> rainOffsets;
    };

private:
    std::vector<Group> groups;
    bool shapesDirty { true };
    std::vector<double> limits;
    void
    updateShapes();
    void
    resolveColors();

public:
    GORainCloudPlot();
    ~GORainCloudPlot() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_RAIN_CLOUD_PLOT_STR;
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
