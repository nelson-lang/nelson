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
