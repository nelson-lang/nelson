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
#include <QtWidgets/QApplication>
#if _MSC_VER
#include <QtCore/QSettings>
#endif
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QStyleFactory>
#include "NelsonPalette.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
static QPalette nelsonPalette;
static bool isQtDarkMode = false;
//===================================================================================
static void
changeToDarkTheme()
{
    nelsonPalette.setColor(QPalette::Window, QColor(53, 53, 53));
    nelsonPalette.setColor(QPalette::WindowText, Qt::white);
    nelsonPalette.setColor(QPalette::Base, QColor(30, 30, 30));
    nelsonPalette.setColor(QPalette::AlternateBase, QColor(53, 53, 53));
    nelsonPalette.setColor(QPalette::ToolTipBase, QColor(53, 53, 53));
    nelsonPalette.setColor(QPalette::ToolTipText, Qt::white);
    nelsonPalette.setColor(QPalette::Text, Qt::white);
    nelsonPalette.setColor(QPalette::Button, QColor(53, 53, 53));
    nelsonPalette.setColor(QPalette::ButtonText, Qt::white);
    nelsonPalette.setColor(QPalette::BrightText, Qt::red);
    nelsonPalette.setColor(QPalette::Link, QColor(42, 130, 218));
    nelsonPalette.setColor(QPalette::Highlight, QColor(42, 130, 218));
    nelsonPalette.setColor(QPalette::HighlightedText, Qt::black);
    nelsonPalette.setColor(QPalette::Disabled, QPalette::Text, QColor(164, 166, 168));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::WindowText, QColor(164, 166, 168));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::ButtonText, QColor(164, 166, 168));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::HighlightedText, QColor(164, 166, 168));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::Base, QColor(68, 68, 68));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::Window, QColor(68, 68, 68));
    nelsonPalette.setColor(QPalette::Disabled, QPalette::Highlight, QColor(68, 68, 68));
    qApp->setStyle(QStyleFactory::create("Fusion"));
    QToolTip::setPalette(nelsonPalette);
    qApp->setPalette(nelsonPalette);
    qApp->setStyleSheet(
        "QToolTip { color: #ffffff; background-color: #2a82da; border: 1px solid white; }");
}
//===================================================================================
void
createNelsonPalette()
{
    nelsonPalette = qApp->palette();
    QColor windowTextColor =nelsonPalette.color(QPalette::WindowText);
    QColor windowColor = nelsonPalette.color(QPalette::Window);
    isQtDarkMode = windowTextColor.toHsl().value() > windowColor.toHsl().value();
    // Qt 5 and Qt6 currently does not manage natevily dark theme on Windows ...
    if (!isQtDarkMode) {
#if _MSC_VER
        QSettings settings(
            "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
            QSettings::NativeFormat);
        isQtDarkMode = (settings.value("AppsUseLightTheme") == 0);
        if (isQtDarkMode) {
            changeToDarkTheme();
        }
#endif
#ifdef __APPLE__
        QColor baseActiveColor = nelsonPalette.color(QPalette::Active, QPalette::Base);
        QColor defaultDarkColorMacos(30, 30, 30, 255);
        isQtDarkMode = baseActiveColor == defaultDarkColorMacos;
#endif
    }
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
    return isQtDarkMode;
}
//===================================================================================
}
//===================================================================================
