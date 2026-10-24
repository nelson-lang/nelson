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
struct PieDisplayWedge
{
    double value { 0 };
    double proportion { 0 };
    std::wstring name;
    std::wstring label;
    size_t originalIndex { 0 };
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOPieChart : public GraphicsObject
{
public:
    GOPieChart();
    ~GOPieChart() override = default;

    std::wstring
    getType() override;

    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_PIE_CHART_STR;
    }

    void
    updateState() override;

    void
    paintMe(RenderInterface& gc) override;

protected:
    virtual double
    innerRadiusRatio();

    virtual void
    registerExtraProperties();

    virtual void
    setupExtraDefaults();

    virtual void
    paintCenterLabel(RenderInterface& gc, double centerX, double centerY, double figureHeight,
        double innerRadius);

private:
    std::vector<PieDisplayWedge> m_wedges;
    FontSpec m_font;

    void
    constructProperties();

    void
    setupDefaults();

    void
    updateModes();

    void
    updateDerivedProperties();
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GODonutChart : public GOPieChart
{
public:
    GODonutChart();
    ~GODonutChart() override = default;

    std::wstring
    getType() override;

    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_DONUT_CHART_STR;
    }

protected:
    double
    innerRadiusRatio() override;

    void
    registerExtraProperties() override;

    void
    setupExtraDefaults() override;

    void
    paintCenterLabel(RenderInterface& gc, double centerX, double centerY, double figureHeight,
        double innerRadius) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
