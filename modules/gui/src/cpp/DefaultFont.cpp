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
#include <QtGui/QFontDatabase>
#include <QtCore/QFile>
#include "DefaultFont.hpp"
#include "GetNelsonPath.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring defaultFontName;
//=============================================================================
bool
configureDefaultFont()
{
    std::wstring nelsonPath = Nelson::GetRootPath();
    std::wstring fontPath = nelsonPath + L"/resources/fonts";
    std::wstring JuliaMonoFullFilename = fontPath + L"/JuliaMono-Regular.ttf";
    QString qFilename = Nelson::wstringToQString(JuliaMonoFullFilename);
    if (QFile::exists(qFilename)) {
        int appFontId = QFontDatabase::addApplicationFont(qFilename);
        if (appFontId < 0) {
            goto defaultFont;
        }
        defaultFontName = L"JuliaMono";
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
