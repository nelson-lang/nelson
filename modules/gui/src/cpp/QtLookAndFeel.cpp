//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QStyleFactory>
#include <QtCore/QStringList>
#include "QtLookAndFeel.hpp"
#include "QStringConverter.hpp"
#include "MainGuiObject.hpp"
#include "NelsonPalette.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
GetLookAndFeelAvailable()
{
    wstringVector lfs;
    QStringList qtLfs = QStyleFactory::keys();
    for (auto& qtLf : qtLfs) {
        lfs.push_back(QStringTowstring(qtLf));
    }
    return lfs;
}
//=============================================================================
std::wstring
GetCurrentLookAndFeel()
{
    return QtGetLookAndFeel();
}
//=============================================================================
bool
SetCurrentLookAndFeel(const std::wstring& lf)
{
    return QtSetLookAndFeel(lf);
}
//=============================================================================
void
SetCurrentStyleSheet(const std::wstring& styleSheet)
{
    QtSetStyleSheet(styleSheet);
}
//=============================================================================
std::wstring
GetCurrentStyleSheet()
{
    return QtGetStyleSheet();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
