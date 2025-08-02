//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WorkspaceBrowser.hpp"
#include "QtWorkspaceBrowser.h"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson::WorkspaceBrowser {
//=============================================================================
static QtWorkspaceBrowser* qtWorkspaceBrowser = nullptr;
//=============================================================================
void*
getWorkspaceBrowser()
{
    return qtWorkspaceBrowser;
}
//=============================================================================
bool
isWorkspaceBrowserVisible()
{
    if (qtWorkspaceBrowser) {
        return qtWorkspaceBrowser->isVisible();
    }
    return false;
}
//=============================================================================
void
showWorkspaceBrowser()
{
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->show();
        qtWorkspaceBrowser->activateWindow();
        qtWorkspaceBrowser->raise();
    }
}
//=============================================================================
void
hideWorkspaceBrowser()
{
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->hide();
    }
}
//=============================================================================
void
toggleVisibilityWorkspaceBrowser()
{
    if (qtWorkspaceBrowser) {
        if (qtWorkspaceBrowser->isVisible()) {
            qtWorkspaceBrowser->hide();
        } else {
            qtWorkspaceBrowser->show();
        }
    }
}
//=============================================================================
bool
createWorkspaceBrowser(Context* context)
{
    if (!qtWorkspaceBrowser) {
        qtWorkspaceBrowser = new QtWorkspaceBrowser(nullptr);
        qtWorkspaceBrowser->setContext(context);
        qtWorkspaceBrowser->restorePosition();
        return true;
    }
    return false;
}
//=============================================================================
bool
destroyWorkspaceBrowser()
{
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->savePositionAndVisibility();
        qtWorkspaceBrowser->deleteLater();
        qtWorkspaceBrowser = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
updateWorkspaceBrowser()
{
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->updateVariables();
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
