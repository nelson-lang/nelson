//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtGui/QGuiApplication>
#include <QtGui/QStyleHints>
#include <QtCore/QProcess>
#include <QtCore/QtGlobal>
#if _MSC_VER
#include <QtCore/QSettings>
#endif
#include <QtCore/QFile>
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QStyleFactory>
#include "NelsonPalette.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//===================================================================================
namespace Nelson {
//===================================================================================
static QPalette nelsonPalette;
static bool isQtDarkMode = false;
//===================================================================================
static void
changeToDarkTheme()
{
    QString nelsonPath
        = Nelson::wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory());
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
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0) && (defined(Q_OS_WIN) || defined(Q_OS_LINUX)))
    QStyleHints* currentStyle = QGuiApplication::styleHints();
    if (currentStyle) {
        isQtDarkMode = currentStyle->colorScheme() == Qt::ColorScheme::Dark;
    }
#else
    nelsonPalette = qApp->palette();
    QColor windowTextColor = nelsonPalette.color(QPalette::WindowText);
    QColor windowColor = nelsonPalette.color(QPalette::Window);
    isQtDarkMode = windowTextColor.toHsl().value() > windowColor.toHsl().value();
    // Qt 5 and Qt6.4 currently does not manage natevily dark theme on Windows ...
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
    }
#endif
    if (isQtDarkMode) {
        changeToDarkTheme();
    } else {
        qApp->setStyleSheet("QTextEdit { background-color: rgb(255, 255, 255); }");
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
