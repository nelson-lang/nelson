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
#include <QtCore/QSettings>
#include "NelsonPalette.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
static QPalette initialePalette;
static QPalette nelsonPalette;
static bool isDarkMode = false;
//===================================================================================
void
createNelsonPalette(QPalette qDefaultPalette)
{
    // Currently, only macos can correctly manage dark mode with Qt without tricks ...
    initialePalette = qDefaultPalette;
    nelsonPalette = initialePalette;
#ifdef _MSC_VER
    /*
    QSettings settings(
        "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
        QSettings::NativeFormat);
    isDarkMode = settings.value("AppsUseLightTheme") == 0;
    */
#else
#ifdef __APPLE__
    QColor baseActiveColor = initialePalette.color(QPalette::Active, QPalette::Base);
    QColor defaultDarkColorMacos(30, 30, 30, 255);
    isDarkMode = baseActiveColor == defaultDarkColorMacos;
#else
#endif
#endif
}
//===================================================================================
QPalette
getNelsonPalette()
{
    return nelsonPalette;
}
//===================================================================================
bool
isDarkPalette()
{
    return isDarkMode;
}
//===================================================================================
}
//===================================================================================
