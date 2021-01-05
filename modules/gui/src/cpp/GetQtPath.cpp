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
#include <QtCore/QLibraryInfo>
#include <QtCore/QtGlobal>
#include "GetQtPath.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#define getPathLocation QLibraryInfo::path
#else
#define getPathLocation QLibraryInfo::location
#endif
#define getPathLocation QLibraryInfo::location
//=============================================================================
std::wstring
GetQtPath(const std::wstring& libraryLocation)
{
    QString path;
    std::wstring wpath;
    if (libraryLocation == L"PrefixPath") {
        path = getPathLocation(QLibraryInfo::PrefixPath);
    }
    if (libraryLocation == L"DocumentationPath") {
        path = getPathLocation(QLibraryInfo::DocumentationPath);
    }
    if (libraryLocation == L"HeadersPath") {
        path = getPathLocation(QLibraryInfo::HeadersPath);
    }
    if (libraryLocation == L"LibrariesPath") {
        path = getPathLocation(QLibraryInfo::LibrariesPath);
    }
    if (libraryLocation == L"LibraryExecutablesPath") {
        path = getPathLocation(QLibraryInfo::LibraryExecutablesPath);
    }
    if (libraryLocation == L"BinariesPath") {
        path = getPathLocation(QLibraryInfo::BinariesPath);
    }
    if (libraryLocation == L"PluginsPath") {
        path = getPathLocation(QLibraryInfo::PluginsPath);
    }
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
    if (libraryLocation == L"ImportsPath") {
        path = getPathLocation(QLibraryInfo::ImportsPath);
    }
#endif
    if (libraryLocation == L"Qml2ImportsPath") {
        path = getPathLocation(QLibraryInfo::Qml2ImportsPath);
    }
    if (libraryLocation == L"ArchDataPath") {
        path = getPathLocation(QLibraryInfo::ArchDataPath);
    }
    if (libraryLocation == L"DataPath") {
        path = getPathLocation(QLibraryInfo::DataPath);
    }
    if (libraryLocation == L"TranslationsPath") {
        path = getPathLocation(QLibraryInfo::TranslationsPath);
    }
    if (libraryLocation == L"ExamplesPath") {
        path = getPathLocation(QLibraryInfo::ExamplesPath);
    }
    if (libraryLocation == L"TestsPath") {
        path = getPathLocation(QLibraryInfo::TestsPath);
    }
    if (!path.isEmpty()) {
        wpath = QStringTowstring(path);
    }
    return wpath;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
