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
