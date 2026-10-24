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
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Which axis a NumericRuler views. X and Z are single-sided; Y may host two
// rulers (yyaxis left/right).
enum RulerAxisKind
{
    RULER_AXIS_X = 0,
    RULER_AXIS_Y = 1,
    RULER_AXIS_Z = 2
};
//=============================================================================
// The kind of coordinate a ruler represents. A ruler is normally numeric; when
// datetime data has been plotted the parent axes flips the matching side into
// datetime mode, which changes the ruler's Type / class and exposes a date
// TickLabelFormat. The numeric coordinate pipeline is unchanged: only the
// persistent label semantics differ.
enum RulerValueKind
{
    RULER_VALUE_NUMERIC = 0,
    RULER_VALUE_DATETIME = 1
};
//=============================================================================
// The numeric ruler exposed through Axes.XAxis / YAxis(1) / YAxis(2) / ZAxis. It
// is a thin view over one axis (and, for Y, one side) of the parent axes: its
// properties mirror that axis' state, and edits are pushed back to the axes. It
// never renders itself; the axes draws the rulers.
class NLSGRAPHICS_IMPEXP GORuler : public GraphicsObject
{
public:
    //=============================================================================
    GORuler();
    //=============================================================================
    ~GORuler() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    constructProperties();
    //=============================================================================
    void
    setupDefaults();
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    // Binds this ruler to one side (1 = left, 2 = right) of its parent axes and
    // to one axis kind (X, Y or Z). Y is the default for backward compatibility.
    void
    linkToAxis(int64 axisHandle, int side, int axisKind = RULER_AXIS_Y);
    //=============================================================================
    int
    getSide() const
    {
        return m_side;
    }
    //=============================================================================
    int
    getAxisKind() const
    {
        return m_axisKind;
    }
    //=============================================================================
    // Persistent value kind (numeric vs datetime). The parent axes pushes this on
    // every sync so the exposure survives limit changes and other updateState
    // re-runs. Setting it keeps the Type property string in step.
    void
    setValueKind(int valueKind);
    //=============================================================================
    int
    getValueKind() const
    {
        return m_valueKind;
    }
    //=============================================================================
private:
    int64 m_axisHandle { -1 };
    int m_side { 1 };
    int m_axisKind { RULER_AXIS_Y };
    int m_valueKind { RULER_VALUE_NUMERIC };
};
//=============================================================================
} // namespace Nelson
//=============================================================================
