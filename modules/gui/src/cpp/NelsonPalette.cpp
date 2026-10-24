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
#include <QtCore/QSettings>
#include <QtCore/QFile>
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolTip>
#include <QtWidgets/QStyleFactory>
#include "NelsonPalette.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonSettingsScope.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QPalette nelsonPalette;
static bool isQtDarkMode = false;
static const QString SETTING_APPEARANCE_THEME = "appearance_theme";
//=============================================================================
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
//=============================================================================
#ifdef __APPLE__
static bool
isMacDarkMode(const QPalette& palette)
{
    QColor baseActiveColor = palette.color(QPalette::Active, QPalette::Base);
    QColor defaultDarkColorMacos(30, 30, 30, 255);
    return baseActiveColor == defaultDarkColorMacos;
}
#endif
//=============================================================================
#if !defined(_MSC_VER) && !defined(__APPLE__)
static bool
isLinuxDarkMode()
{
    // Cached for the session: spawning 'gsettings' is expensive, and a short timeout keeps a
    // missing or hanging gsettings from delaying startup.
    static bool cached = false;
    static bool cachedValue = false;
    if (cached) {
        return cachedValue;
    }
    cached = true;
    QProcess process;
    process.start("gsettings get org.gnome.desktop.interface gtk-theme");
    if (process.waitForFinished(100)) {
        QString output = process.readAllStandardOutput();
        cachedValue = output.contains("dark", Qt::CaseInsensitive);
    } else {
        process.kill();
        process.waitForFinished(50);
        cachedValue = false;
    }
    return cachedValue;
}
#endif
//=============================================================================
static bool
detectDarkMode()
{
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0) && (defined(Q_OS_WIN) || defined(Q_OS_LINUX)))
    if (QGuiApplication::instance()) {
        QStyleHints* styleHints = QGuiApplication::styleHints();
        if (styleHints && styleHints->colorScheme() == Qt::ColorScheme::Dark) {
            return true;
        }
    }
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

#if _MSC_VER
    return isWindowsDarkMode();
#elif defined(__APPLE__)
    return isMacDarkMode(palette);
#else
    return isLinuxDarkMode();
#endif
#endif
}
//=============================================================================
static void
changeToDarkTheme()
{
    QString nelsonPath
        = Nelson::wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory());
    QString darkThemeFullFilename = nelsonPath + "/resources/qss/Combinear.qss";
    QFile file(darkThemeFullFilename);
    if (file.exists() && file.open(QFile::ReadOnly)) {
        QString styleSheet { file.readAll() };
        qApp->setStyleSheet(styleSheet);
    }
}
//=============================================================================
void
applyNelsonTheme(bool dark)
{
    if (!qApp) {
        return;
    }
    isQtDarkMode = dark;
    NelsonConfiguration::getInstance()->setDarkMode(isQtDarkMode);
    if (isQtDarkMode) {
        changeToDarkTheme();
    } else {
        qApp->setStyleSheet("QTextEdit { background-color: rgb(255, 255, 255); }");
    }
    nelsonPalette = qApp->palette();
#ifdef _MSC_VER
    // windows already created keep the title bar of the former theme otherwise
    refreshWindowsTitleBarsTheme();
#endif
}
//=============================================================================
void
refreshNelsonTheme()
{
    applyNelsonTheme(isQtDarkMode);
}
//=============================================================================
std::wstring
getThemePreference()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    QString value = settings.value(SETTING_APPEARANCE_THEME, "system").toString().toLower();
    if (value == "light" || value == "dark") {
        return QStringTowstring(value);
    }
    return L"system";
}
//=============================================================================
void
setThemePreference(const std::wstring& preference)
{
    std::wstring value = preference;
    if (value != L"light" && value != L"dark") {
        value = L"system";
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_APPEARANCE_THEME, wstringToQString(value));
    bool dark = (value == L"dark") || (value == L"system" && detectDarkMode());
    applyNelsonTheme(dark);
}
//=============================================================================
void
createNelsonPalette()
{
    std::wstring preference = getThemePreference();
    bool dark = (preference == L"dark") || (preference == L"system" && detectDarkMode());
    applyNelsonTheme(dark);
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0))
    static bool colorSchemeConnected = false;
    if (!colorSchemeConnected && QGuiApplication::instance()) {
        QStyleHints* styleHints = QGuiApplication::styleHints();
        if (styleHints) {
            colorSchemeConnected = true;
            QObject::connect(
                styleHints, &QStyleHints::colorSchemeChanged, qApp, [](Qt::ColorScheme scheme) {
                    // an explicit light/dark preference wins over the OS theme
                    if (getThemePreference() == L"system") {
                        applyNelsonTheme(scheme == Qt::ColorScheme::Dark);
                    }
                });
        }
    }
#endif
}
//=============================================================================
QPalette
getNelsonPalette()
{
    // Returns the effective application palette so callers get a valid palette that keeps
    // the style/QSS inheritance.
    if (qApp) {
        nelsonPalette = qApp->palette();
    }
    return nelsonPalette;
}
//=============================================================================
bool
isDarkPalette()
{
    return isQtDarkMode;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
