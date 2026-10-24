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
#include <functional>
#include <string>
#include <vector>
#include <QtWidgets/QApplication>
#include "nlsGui_exports.h"
//=============================================================================
class NLSGUI_IMPEXP NelsonApplication : public QApplication
{
public:
    using FileOpenHandler = std::function<bool(const std::wstring&)>;

    NelsonApplication(int& argc, char** argv);

    void
    setFileOpenHandler(FileOpenHandler handler);

    size_t
    pendingFileCount() const;

protected:
    bool
    event(QEvent* event) override;

private:
    void
    dispatchPendingFiles();

    FileOpenHandler fileOpenHandler;
    std::vector<std::wstring> pendingFiles;
    bool retryScheduled = false;
};
//=============================================================================
