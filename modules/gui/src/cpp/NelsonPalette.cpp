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
#if _MSC_VER
static bool
isWindowsDarkMode()
{
    QSettings settings(
        "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
        QSettings::NativeFormat);
    QVariant value = settings.value("AppsUseLightTheme");
    return value.isValid() && (value.toInt() == 0);
}
#endif
//===================================================================================
#ifdef __APPLE__
static bool
isMacDarkMode(const QPalette& palette)
{
    QColor baseActiveColor = palette.color(QPalette::Active, QPalette::Base);
    QColor defaultDarkColorMacos(30, 30, 30, 255);
    return baseActiveColor == defaultDarkColorMacos;
}
#endif
//===================================================================================
#if !defined(_MSC_VER) && !defined(__APPLE__)
static bool
isLinuxDarkMode()
{
    QProcess process;
    process.start("gsettings get org.gnome.desktop.interface gtk-theme");
    if (process.waitForFinished(500)) {
        QString output = process.readAllStandardOutput();
        return output.contains("dark", Qt::CaseInsensitive);
    }
    return false;
}
#endif
//===================================================================================
static bool
detectDarkMode()
{
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0) && (defined(Q_OS_WIN) || defined(Q_OS_LINUX)))
    // Qt 6.5+
    if (QGuiApplication::instance()) {
        QStyleHints* styleHints = QGuiApplication::styleHints();
        if (styleHints && styleHints->colorScheme() == Qt::ColorScheme::Dark) {
            return true;
        }
    }
// Fallback Windows
#if _MSC_VER
    return isWindowsDarkMode();
#else
    return false;
#endif
#else
    QPalette palette = qApp ? qApp->palette() : QPalette();
    QColor windowTextColor = palette.color(QPalette::WindowText);
    QColor windowColor = palette.color(QPalette::Window);
    bool dark = windowTextColor.toHsl().value() > windowColor.toHsl().value();

    if (dark) {
        return true;
    }

// Fallbacks
#if _MSC_VER
    return isWindowsDarkMode();
#elif defined(__APPLE__)
    return isMacDarkMode(palette);
#else
    return isLinuxDarkMode();
#endif
#endif
}
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
    isQtDarkMode = detectDarkMode();
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
