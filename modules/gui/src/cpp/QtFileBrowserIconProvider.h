//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtCore/QHash>
#include <QtCore/QString>
#include <QtGui/QIcon>
#include <QtWidgets/QFileIconProvider>
//=============================================================================
// QFileSystemModel asks its icon provider for the icon and the type of every
// entry on the GUI thread, and again for the whole directory each time the
// watched directory changes. The default provider resolves both through the
// MIME database and the shell, which freezes the window on a large directory
// where files keep appearing (a test campaign writing into the current
// directory). This provider answers from the file suffix and caches the icons.
//=============================================================================
class QtFileBrowserIconProvider : public QFileIconProvider
{
public:
    QtFileBrowserIconProvider();

    QIcon
    icon(IconType type) const override;
    QIcon
    icon(const QFileInfo& info) const override;
    QString
    type(const QFileInfo& info) const override;

private:
    QIcon folderIcon;
    QIcon fileIcon;
    mutable QHash<QString, QIcon> iconsBySuffix;
};
//=============================================================================
