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
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOAnimatedLine : public GraphicsObject
{
public:
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_ANIMATED_LINE_STR;
    }

    GOAnimatedLine();
    ~GOAnimatedLine() override;
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

    void
    addPoints(
        const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& z);
    void
    clearPoints();
    void
    setPoints(
        const std::vector<double>& x, const std::vector<double>& y, const std::vector<double>& z);
    std::vector<double>
    xData() const;
    std::vector<double>
    yData() const;
    std::vector<double>
    zData() const;

private:
    std::vector<double> m_xData;
    std::vector<double> m_yData;
    std::vector<double> m_zData;
    std::vector<double> m_limits;
    mutable bool m_limitsDirty = true;

    void
    trimToMaximumNumPoints();
    void
    applyAutomaticStyle();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
