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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include <unordered_map>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOStackedLineChart : public GraphicsObject
{
private:
    std::unordered_map<GraphicsObject*, std::pair<ArrayOf, ArrayOf>> m_originalLineData;

public:
    GOStackedLineChart();
    ~GOStackedLineChart() override = default;

    std::wstring
    getType() override;

    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_STACKED_LINE_CHART_STR;
    }

    void
    registerProperties() override;

    void
    updateState() override;

    void
    paintMe(RenderInterface& gc) override;

    void
    preserveLineData(GraphicsObject* line);

    std::pair<ArrayOf, ArrayOf>
    originalLineData(GraphicsObject* line);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
