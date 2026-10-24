//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QFontDatabase>
#include <QtCore/QFile>
#include <QtCore/QSettings>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QtGlobal>
#include "DefaultFont.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// User settings overriding the built-in default font. Read once at startup; when absent or
// invalid the bundled 'Hack' font or a platform monospace family is used.
const QString SETTING_DEFAULT_FONT_NAME = "Font/Name";
const QString SETTING_DEFAULT_FONT_SIZE = "Font/Size";
//=============================================================================
#define DEFAULT_FONT_SIZE 10
#define DEFAULT_FONT_MIN_SIZE 5
#define DEFAULT_FONT_MAX_SIZE 72
//=============================================================================
static std::wstring defaultFontName;
static int defaultFontSize = DEFAULT_FONT_SIZE;
//=============================================================================
static void
setPlatformFallbackFontName()
{
#ifdef __APPLE__
    defaultFontName = L"Monaco";
#else
    defaultFontName = L"Monospace";
#endif
}
//=============================================================================
static void
readFontSettings()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    int savedSize = settings.value(SETTING_DEFAULT_FONT_SIZE, DEFAULT_FONT_SIZE).toInt();
    if (savedSize < DEFAULT_FONT_MIN_SIZE || savedSize > DEFAULT_FONT_MAX_SIZE) {
        savedSize = DEFAULT_FONT_SIZE;
    }
    defaultFontSize = savedSize;

    QString savedName = settings.value(SETTING_DEFAULT_FONT_NAME, QString()).toString().trimmed();
    if (!savedName.isEmpty()) {
        QStringList families = QFontDatabase::families();
        if (families.contains(savedName, Qt::CaseInsensitive)) {
            defaultFontName = QStringTowstring(savedName);
        }
    }
}
//=============================================================================
bool
configureDefaultFont()
{
    bool bundledFontLoaded = false;
    std::wstring nelsonPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    std::wstring fontPath = nelsonPath + L"/resources/fonts";
    std::wstring hackFontFullFilename = fontPath + L"/Hack-Regular.ttf";
    QString qFilename = Nelson::wstringToQString(hackFontFullFilename);
    if (QFile::exists(qFilename) && (QFontDatabase::addApplicationFont(qFilename) >= 0)) {
        defaultFontName = L"Hack";
        bundledFontLoaded = true;
    } else {
        setPlatformFallbackFontName();
    }
    // an explicit user preference always wins over the built-in choice
    readFontSettings();
    return bundledFontLoaded;
}
//=============================================================================
void
reloadDefaultFontSettings()
{
    readFontSettings();
}
//=============================================================================
std::wstring
getDefaultFontName()
{
    return defaultFontName;
}
//=============================================================================
int
getDefaultFontSize()
{
    return defaultFontSize;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
