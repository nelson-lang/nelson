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
#include <string>
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class GOAnnotationKind
{
    Line,
    Arrow,
    DoubleArrow,
    TextArrow,
    Rectangle,
    Ellipse,
    TextBox
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOAnnotation : public GraphicsObject
{
private:
    GOAnnotationKind m_kind;
    bool m_updatingPosition = false;
    std::wstring m_units = L"normalized";

    void
    constructProperties();
    void
    setupDefaults();
    void
    updateUnits();
    void
    syncLinePosition();
    std::vector<double>
    positionInPixels(RenderInterface& gc);
    void
    paintLineLike(RenderInterface& gc);
    void
    paintShape(RenderInterface& gc);
    void
    paintTextBox(RenderInterface& gc);

public:
    explicit GOAnnotation(GOAnnotationKind kind);
    ~GOAnnotation() override = default;
    std::wstring
    getType() override;
    std::string
    getClassName() override;
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    GOAnnotationKind
    getKind() const
    {
        return m_kind;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
