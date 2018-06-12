//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GetQtPath.hpp"
#include "QStringConverter.hpp"
#include <QtCore/QLibraryInfo>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GetQtPath(const std::wstring& libraryLocation)
{
    QString path;
    std::wstring wpath = L"";
    if (libraryLocation == L"PrefixPath") {
        path = QLibraryInfo::location(QLibraryInfo::PrefixPath);
    }
    if (libraryLocation == L"DocumentationPath") {
        path = QLibraryInfo::location(QLibraryInfo::DocumentationPath);
    }
    if (libraryLocation == L"HeadersPath") {
        path = QLibraryInfo::location(QLibraryInfo::HeadersPath);
    }
    if (libraryLocation == L"LibrariesPath") {
        path = QLibraryInfo::location(QLibraryInfo::LibrariesPath);
    }
    if (libraryLocation == L"LibraryExecutablesPath") {
        path = QLibraryInfo::location(QLibraryInfo::LibraryExecutablesPath);
    }
    if (libraryLocation == L"BinariesPath") {
        path = QLibraryInfo::location(QLibraryInfo::BinariesPath);
    }
    if (libraryLocation == L"PluginsPath") {
        path = QLibraryInfo::location(QLibraryInfo::PluginsPath);
    }
    if (libraryLocation == L"ImportsPath") {
        path = QLibraryInfo::location(QLibraryInfo::ImportsPath);
    }
    if (libraryLocation == L"Qml2ImportsPath") {
        path = QLibraryInfo::location(QLibraryInfo::Qml2ImportsPath);
    }
    if (libraryLocation == L"ArchDataPath") {
        path = QLibraryInfo::location(QLibraryInfo::ArchDataPath);
    }
    if (libraryLocation == L"DataPath") {
        path = QLibraryInfo::location(QLibraryInfo::DataPath);
    }
    if (libraryLocation == L"TranslationsPath") {
        path = QLibraryInfo::location(QLibraryInfo::TranslationsPath);
    }
    if (libraryLocation == L"ExamplesPath") {
        path = QLibraryInfo::location(QLibraryInfo::ExamplesPath);
    }
    if (libraryLocation == L"TestsPath") {
        path = QLibraryInfo::location(QLibraryInfo::TestsPath);
    }
    if (!path.isEmpty()) {
        wpath = QStringTowstring(path);
    }
    return wpath;
}
//=============================================================================
}
//=============================================================================
