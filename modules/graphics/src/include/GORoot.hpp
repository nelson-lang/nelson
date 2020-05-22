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
#include "GraphicObject.hpp"
#include "GraphicRenderer.hpp"
#include "GOWindow.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define ROOT_TYPE_STR "root"
//=============================================================================
class GORoot : public GraphicObject
{
    bool m_resized;
    void* m_qWindowPtr;

public:
    GORoot(void* qWindowPtr);
    ~GORoot() override = default;
    void
    registerProperties() override;
    bool
    resized();
    int
    getWidth();
    int
    getHeight();
    void
    refreshProperties() override;
    virtual void
    paintMe(GraphicRenderer& gc);
    virtual void
    resizeGL(int width, int height);
    void
    initializeProperties();
    void
    repaint();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
