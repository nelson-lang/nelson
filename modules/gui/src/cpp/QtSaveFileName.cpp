//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QApplication>
#include <QtCore/QMetaObject>
#include <QtCore/QThread>
#include <QtCore/QFileInfo>
#ifdef _WIN32
#include <windows.h>
#endif
#include "QtSaveFileName.hpp"
#include "QStringConverter.hpp"
#include "QtFileDialogHistory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
QtSaveFileName(void* parent, const std::wstring& title, const std::wstring& defaultName,
    const std::wstring& defaultDir, const std::wstring& filter, bool& error)
{
    error = false;

    QString resultPath;

    auto dialogFunc = [&]() {
        QWidget* parentWidget = nullptr;

#ifdef _WIN32
        if (parent) {
            HWND hwnd = reinterpret_cast<HWND>(parent);
            parentWidget = QWidget::find(reinterpret_cast<WId>(hwnd));
        }
#endif
        QString startPath = qtDialogInitialSavePath(L"nflow.save", wstringToQString(defaultDir),
            QFileInfo(wstringToQString(defaultName)).fileName());

        resultPath = QFileDialog::getSaveFileName(
            parentWidget, wstringToQString(title), startPath, wstringToQString(filter));

        if (!resultPath.isEmpty()) {
            qtRecordDialogSelection(L"nflow.save", QStringLiteral("save"), resultPath);
        }
    };

    if (QThread::currentThread() == qApp->thread()) {
        dialogFunc();
    } else {
        QMetaObject::invokeMethod(qApp, dialogFunc, Qt::BlockingQueuedConnection);
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
