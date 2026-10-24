//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonApplication.hpp"
#include <utility>
#include <QtCore/QFileInfo>
#include <QtCore/QTimer>
#include <QtGui/QFileOpenEvent>
//=============================================================================
NelsonApplication::NelsonApplication(int& argc, char** argv) : QApplication(argc, argv) { }
//=============================================================================
void
NelsonApplication::setFileOpenHandler(FileOpenHandler handler)
{
    fileOpenHandler = std::move(handler);
    dispatchPendingFiles();
}
//=============================================================================
size_t
NelsonApplication::pendingFileCount() const
{
    return pendingFiles.size();
}
//=============================================================================
bool
NelsonApplication::event(QEvent* event)
{
    if (event->type() == QEvent::FileOpen) {
        auto* fileOpenEvent = static_cast<QFileOpenEvent*>(event);
        QString path = fileOpenEvent->file();
        if (path.isEmpty() && fileOpenEvent->url().isLocalFile()) {
            path = fileOpenEvent->url().toLocalFile();
        }
        if (!path.isEmpty()) {
            pendingFiles.push_back(QFileInfo(path).absoluteFilePath().toStdWString());
            dispatchPendingFiles();
            return true;
        }
    }
    return QApplication::event(event);
}
//=============================================================================
void
NelsonApplication::dispatchPendingFiles()
{
    if (!fileOpenHandler || pendingFiles.empty()) {
        return;
    }

    auto current = pendingFiles.begin();
    while (current != pendingFiles.end()) {
        if (fileOpenHandler(*current)) {
            current = pendingFiles.erase(current);
        } else {
            ++current;
        }
    }

    if (!pendingFiles.empty() && !retryScheduled) {
        retryScheduled = true;
        QTimer::singleShot(100, this, [this]() {
            retryScheduled = false;
            dispatchPendingFiles();
        });
    }
}
//=============================================================================
