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
#include "GOLineSeries.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
class NLSGRAPHICS_IMPEXP GOCompassPlot : public GOLineSeries
{
public:
    GOCompassPlot();
    ~GOCompassPlot() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_COMPASS_PLOT_STR;
    }
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
