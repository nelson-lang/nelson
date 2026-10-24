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
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOBubbleChart : public GraphicsObject
{
private:
    mutable bool cDataDirty = true;
    mutable double cachedMinC = 0.0;
    mutable double cachedMaxC = 1.0;
    std::vector<double> limits;
    mutable bool limitsDirty = true;

public:
    GOBubbleChart();
    ~GOBubbleChart() override;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_BUBBLE_CHART_STR;
    }
    void
    paintMe(RenderInterface& gc) override;
    void
    updateState() override;
    std::vector<double>
    getLimits() override;
    bool
    preservesAutoDataLimits() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
