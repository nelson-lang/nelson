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
#include "GOGroup.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOHeatmapChart : public GOGroup
{
public:
    GOHeatmapChart();
    ~GOHeatmapChart() override = default;
    std::wstring
    getType() override;
    void
    updateState() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_HEATMAP_STR;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
