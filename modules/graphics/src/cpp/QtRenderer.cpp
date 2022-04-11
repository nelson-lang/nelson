//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtRenderer.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QtRenderer::QtRenderer(QPainter* painter, double x1, double y1, double width, double height)
{
    m_x1 = x1;
    m_y1 = y1;
    m_width = width;
    m_height = height;

    _painter = painter;
    if (_painter != nullptr) {
        _painter->setRenderHint(QPainter::TextAntialiasing);
    }
}
//=============================================================================
QtRenderer::~QtRenderer() = default;
//=============================================================================
void
QtRenderer::clear(std::vector<double> color)
{
    _painter->save();
    QColor backgroundColor;
    backgroundColor.setRgbF(color[0], color[1], color[2]);
    _painter->setPen(Qt::NoPen);
    _painter->setBrush(backgroundColor);
    _painter->drawRect(m_x1, m_y1, m_width, m_height);
    _painter->restore();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
