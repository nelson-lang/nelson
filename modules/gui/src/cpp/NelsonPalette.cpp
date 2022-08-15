//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtCore/QProcess>
#if _MSC_VER
#include <QtCore/QSettings>
#endif
#include <QtCore/QFile>
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QStyleFactory>
#include "NelsonPalette.hpp"
#include "GetNelsonPath.hpp"
#include "QStringConverter.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
static QPalette nelsonPalette;
static bool isQtDarkMode = false;
//===================================================================================
static void
changeToDarkTheme()
{
    QString nelsonPath = Nelson::wstringToQString(Nelson::GetRootPath());
    QString darkThemeFullFilename = nelsonPath + "/resources/qss/Combinear.qss";
    QFile file(darkThemeFullFilename);
    if (file.exists()) {
        file.open(QFile::ReadOnly);
        QString styleSheet { file.readAll() };
        qApp->setStyleSheet(styleSheet);
    }
}
//===================================================================================
void
createNelsonPalette()
{
    nelsonPalette = qApp->palette();
    QColor windowTextColor = nelsonPalette.color(QPalette::WindowText);
    QColor windowColor = nelsonPalette.color(QPalette::Window);
    isQtDarkMode = windowTextColor.toHsl().value() > windowColor.toHsl().value();
    // Qt 5 and Qt6 currently does not manage natevily dark theme on Windows ...
    if (!isQtDarkMode) {
#if _MSC_VER
        QSettings settings(
            "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
            QSettings::NativeFormat);
        isQtDarkMode = (settings.value("AppsUseLightTheme") == 0);
#else
#ifdef __APPLE__
        QColor baseActiveColor = nelsonPalette.color(QPalette::Active, QPalette::Base);
        QColor defaultDarkColorMacos(30, 30, 30, 255);
        isQtDarkMode = baseActiveColor == defaultDarkColorMacos;
#else
        QProcess process;
        process.start("gsettings get org.gnome.desktop.interface gtk-theme");
        process.waitForFinished();
        QString output = process.readAllStandardOutput();
        isQtDarkMode = output.contains("dark", Qt::CaseInsensitive);
#endif
#endif
        if (isQtDarkMode) {
            changeToDarkTheme();
        }
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
