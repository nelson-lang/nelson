//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonColors.hpp"
#include "NelsonPalette.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
QColor
getWarningColor()
{
    return QColor(QColor(255, 175, 0));
}
//===================================================================================
QColor
getInputColor()
{
    return isDarkPalette() ? QColor(Qt::cyan) : QColor(Qt::blue);
}
//===================================================================================
QColor
getErrorColor()
{
    return { Qt::red };
}
//===================================================================================
QColor
getOutputColor()
{
    return isDarkPalette() ? QColor(Qt::white) : QColor(Qt::black);
}
//===================================================================================
}
//===================================================================================
