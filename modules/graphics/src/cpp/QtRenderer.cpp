//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
