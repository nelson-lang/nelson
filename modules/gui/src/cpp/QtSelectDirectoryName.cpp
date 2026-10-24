//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QApplication>
#include <QtWidgets/QLineEdit>
#include <QtCore/QMetaObject>
#include <QtCore/QThread>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QString>
#ifdef _WIN32
#include <windows.h>
#endif
#include "QtSelectDirectoryName.hpp"
#include "QStringConverter.hpp"
#include "QtFileDialogHistory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
QtSelectDirectoryName(
    void* parent, const std::wstring& title, const std::wstring& defaultPath, bool& error)
{
    error = false;

    if (!qApp) {
        error = true;
        return std::wstring();
    }

    QString resultPath;

    auto dialogFunc = [&]() {
        QWidget* parentWidget = nullptr;

#ifdef _WIN32
        if (parent) {
            HWND hwnd = reinterpret_cast<HWND>(parent);
            parentWidget = QWidget::find(reinterpret_cast<WId>(hwnd));
        }
#endif

        QString lastDir
            = qtDialogInitialDirectory(L"nflow.directory", wstringToQString(defaultPath));

        resultPath = QFileDialog::getExistingDirectory(parentWidget, wstringToQString(title),
            lastDir, QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);

        if (!resultPath.isEmpty()) {
            qtRecordDialogSelection(L"nflow.directory", QStringLiteral("dir"), resultPath);
        }
    };

    if (QThread::currentThread() == qApp->thread()) {
        dialogFunc();
    } else {
        QMetaObject::invokeMethod(
            QApplication::instance(), dialogFunc, Qt::BlockingQueuedConnection);
    }

    if (resultPath.isEmpty()) {
        error = true;
        return std::wstring();
    }

    return QStringTowstring(resultPath);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
