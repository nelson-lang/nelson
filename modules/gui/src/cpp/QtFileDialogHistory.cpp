//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtFileDialogHistory.hpp"
#include "DialogDirectories.hpp"
#include "QStringConverter.hpp"
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
//=============================================================================
namespace Nelson {
//=============================================================================
QString
qtDialogInitialDirectory(
    const std::wstring& key, const QString& explicitPath, const QString& fallbackPath)
{
    const QString fallback = fallbackPath.isEmpty() ? QDir::currentPath() : fallbackPath;
    return wstringToQString(
        resolveDialogDirectory(key, QStringTowstring(explicitPath), QStringTowstring(fallback)));
}
//=============================================================================
QString
qtDialogInitialSavePath(const std::wstring& key, const QString& explicitPath,
    const QString& suggestedName, const QString& fallbackPath)
{
    QString explicitDirectory;
    QString name = suggestedName;
    if (!explicitPath.isEmpty() && explicitPath != QStringLiteral(".")) {
        QFileInfo info(explicitPath);
        if (info.isDir()) {
            explicitDirectory = info.absoluteFilePath();
        } else {
            if (name.isEmpty()) {
                name = info.fileName();
            }
            if (!info.path().isEmpty() && info.path() != QStringLiteral(".")) {
                explicitDirectory = info.absolutePath();
            }
        }
    }
    const QString directory = qtDialogInitialDirectory(key, explicitDirectory, fallbackPath);
    return name.isEmpty() ? directory : QDir(directory).filePath(name);
}
//=============================================================================
void
qtRecordDialogSelection(const std::wstring& key, const QString& mode, const QString& selectedPath)
{
    if (selectedPath.isEmpty()) {
        return;
    }
    const QFileInfo selected(selectedPath);
    const QString directory
        = mode == QStringLiteral("dir") ? selected.absoluteFilePath() : selected.absolutePath();
    setDialogDirectory(key, QStringTowstring(directory));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
