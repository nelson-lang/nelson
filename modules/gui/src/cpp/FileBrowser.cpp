//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileBrowser.hpp"
#include "QtFileBrowser.h"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::FileBrowser {
//=============================================================================
static QtFileBrowser* qtFileBrowser = nullptr;
//=============================================================================
void*
getFileBrowser()
{
    return qtFileBrowser;
}
//=============================================================================
void
showFileBrowser()
{
    if (qtFileBrowser) {
        qtFileBrowser->show();
        qtFileBrowser->activateWindow();
        qtFileBrowser->raise();
    }
}
//=============================================================================
bool
isFileBrowserVisible()
{
    if (qtFileBrowser) {
        return qtFileBrowser->isVisible();
    }
    return false;
}
//=============================================================================
void
hideFileBrowser()
{
    if (qtFileBrowser) {
        qtFileBrowser->hide();
    }
}
//=============================================================================
void
toggleVisibilityFileBrowser()
{
    if (qtFileBrowser) {
        if (qtFileBrowser->isVisible()) {
            qtFileBrowser->hide();
        } else {
            qtFileBrowser->show();
        }
    }
}
//=============================================================================
bool
createFileBrowser()
{
    if (!qtFileBrowser) {
        qtFileBrowser = new QtFileBrowser(nullptr);
        qtFileBrowser->restorePosition();
        return true;
    }
    return false;
}
//=============================================================================
bool
destroyFileBrowser()
{
    if (qtFileBrowser) {
        qtFileBrowser->savePositionAndVisibility();
        qtFileBrowser->deleteLater();
        qtFileBrowser = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
updateFileBrowser()
{
    if (qtFileBrowser) {
        qtFileBrowser->updateCurrentPath();
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
