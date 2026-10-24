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
struct BoxChartStats
{
    bool valid { false };
    double q1 { 0. };
    double median { 0. };
    double q3 { 0. };
    double whiskerLow { 0. };
    double whiskerHigh { 0. };
    double notchLow { 0. };
    double notchHigh { 0. };
    std::vector<double> outliers;
};
//=============================================================================
struct BoxChartItem
{
    double position { 0. };
    BoxChartStats stats;
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOBoxChart : public GraphicsObject
{
private:
    std::vector<double> limits;
    std::vector<BoxChartItem> items;
    bool limitsDirty { true };

public:
    GOBoxChart();
    ~GOBoxChart() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_BOX_CHART_STR;
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
