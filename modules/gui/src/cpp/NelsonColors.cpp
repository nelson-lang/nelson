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
    return isDarkPalette() ? QColor(QStringLiteral("#b78620")) : QColor(QColor(255, 175, 0));
}
//===================================================================================
QColor
getInputColor()
{
    return isDarkPalette() ? QColor(QStringLiteral("#007ACC")) : QColor(Qt::blue);
}
//===================================================================================
QColor
getErrorColor()
{
    return isDarkPalette() ? QColor(QStringLiteral("#f44747")) : QColor(Qt::red);
}
//===================================================================================
QColor
getOutputColor()
{
    return isDarkPalette() ? QColor(QStringLiteral("#D4D4D4")) : QColor(Qt::black);
}
//===================================================================================
}
//===================================================================================
