//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtFileBrowserIconProvider.h"
#include "QtTranslation.hpp"
#include <QtCore/QFileInfo>
//=============================================================================
using namespace Nelson;
//=============================================================================
QtFileBrowserIconProvider::QtFileBrowserIconProvider()
{
    setOptions(QFileIconProvider::DontUseCustomDirectoryIcons);
    folderIcon = QFileIconProvider::icon(QFileIconProvider::Folder);
    fileIcon = QFileIconProvider::icon(QFileIconProvider::File);
}
//=============================================================================
QIcon
QtFileBrowserIconProvider::icon(IconType type) const
{
    return QFileIconProvider::icon(type);
}
//=============================================================================
QIcon
QtFileBrowserIconProvider::icon(const QFileInfo& info) const
{
    if (info.isDir()) {
        return folderIcon;
    }
    const QString suffix = info.suffix().toLower();
    if (suffix.isEmpty()) {
        return fileIcon;
    }
    const auto found = iconsBySuffix.constFind(suffix);
    if (found != iconsBySuffix.constEnd()) {
        return found.value();
    }
    // One shell lookup per suffix, not one per file.
    QIcon suffixIcon = QFileIconProvider::icon(info);
    if (suffixIcon.isNull()) {
        suffixIcon = fileIcon;
    }
    iconsBySuffix.insert(suffix, suffixIcon);
    return suffixIcon;
}
//=============================================================================
QString
QtFileBrowserIconProvider::type(const QFileInfo& info) const
{
    if (info.isDir()) {
        return TR("Folder");
    }
    const QString suffix = info.suffix().toLower();
    if (suffix == "m") {
        return TR("Nelson script");
    }
    if (suffix == "nh5") {
        return TR("Nelson data");
    }
    if (suffix.isEmpty()) {
        return TR("File");
    }
    return TR("%1 file").arg(suffix.toUpper());
}
//=============================================================================
