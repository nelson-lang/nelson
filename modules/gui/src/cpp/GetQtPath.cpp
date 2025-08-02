//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
