//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtGui/QPainter>
#include "GraphicRenderer.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class QtRenderer : public GraphicRenderer
{
private:
    QPainter* _painter;
    double m_x1;
    double m_y1;
    double m_width;
    double m_height;

public:
    QtRenderer(QPainter* painter, double x1, double y1, double width, double height);
    ~QtRenderer() override;
    void
    clear(std::vector<double> colors);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
