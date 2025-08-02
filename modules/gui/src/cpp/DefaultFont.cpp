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
#include "DefaultFont.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring defaultFontName;
//=============================================================================
bool
configureDefaultFont()
{
    std::wstring nelsonPath = NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    std::wstring fontPath = nelsonPath + L"/resources/fonts";
    std::wstring JuliaMonoFullFilename = fontPath + L"/Hack-Regular.ttf";
    QString qFilename = Nelson::wstringToQString(JuliaMonoFullFilename);
    if (QFile::exists(qFilename)) {
        int appFontId = QFontDatabase::addApplicationFont(qFilename);
        if (appFontId < 0) {
            goto defaultFont;
        }
        defaultFontName = L"Hack";
        return true;
    }
defaultFont:
#ifdef __APPLE__
    defaultFontName = L"Monaco";
#else
    defaultFontName = L"Monospace";
#endif

    return false;
}
//===================================================================================
std::wstring
getDefaultFontName()
{
    return defaultFontName;
}
//===================================================================================
}
//===================================================================================
