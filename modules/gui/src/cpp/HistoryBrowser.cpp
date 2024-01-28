//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HistoryBrowser.hpp"
#include "QtHistoryBrowser.h"
#include "QStringConverter.hpp"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include <mutex>
//=============================================================================
namespace Nelson::HistoryBrowser {
//=============================================================================
static QtHistoryBrowser* qtHistoryBrowser = nullptr;
static std::wstring textForEditor;
static std::mutex m_mutex_sendToTextEditor;
//=============================================================================
void*
getHistoryBrowser()
{
    return qtHistoryBrowser;
}
//=============================================================================
void
showHistoryBrowser()
{
    if (qtHistoryBrowser) {
        qtHistoryBrowser->show();
        qtHistoryBrowser->activateWindow();
        qtHistoryBrowser->raise();
    }
}
//=============================================================================
void
hideHistoryBrowser()
{
    if (qtHistoryBrowser) {
        qtHistoryBrowser->hide();
    }
}
//=============================================================================
bool
isHistoryBrowserVisible()
{
    if (qtHistoryBrowser) {
        return qtHistoryBrowser->isVisible();
    }
    return false;
}
//=============================================================================
void
toggleVisibilityHistoryBrowser()
{
    if (qtHistoryBrowser) {
        if (qtHistoryBrowser->isVisible()) {
            qtHistoryBrowser->hide();
        } else {
            qtHistoryBrowser->show();
        }
    }
}
//=============================================================================
bool
createHistoryBrowser()
{
    if (!qtHistoryBrowser) {
        wstringVector lines;
        qtHistoryBrowser = new QtHistoryBrowser(nullptr);
        textForEditor.clear();
        qtHistoryBrowser->restorePosition();
        return synchronizeHistoryBrowser();
    }
    return false;
}
//=============================================================================
bool
destroyHistoryBrowser()
{
    if (qtHistoryBrowser) {
        textForEditor.clear();
        qtHistoryBrowser->savePositionAndVisibility();
        qtHistoryBrowser->deleteLater();
        qtHistoryBrowser = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
synchronizeHistoryBrowser()
{
    if (qtHistoryBrowser) {
        return qtHistoryBrowser->synchronizeHistoryManager(
            QtHistoryBrowser::SYNC_DIRECTION::SYNC_HM_TO_HB);
    }
    return false;
}
//=============================================================================
bool
addLine(const std::wstring& line)
{
    if (qtHistoryBrowser) {
        qtHistoryBrowser->addCommand(wstringToQString(line));
    }
    return false;
}
//=============================================================================
void
setTextForTextEditor(const std::wstring& text)
{
    std::scoped_lock<std::mutex> _lock { m_mutex_sendToTextEditor };
    textForEditor = text;
}
//=============================================================================
NLSGUI_IMPEXP std::wstring
getTextForTextEditor(bool withClear)
{
    std::scoped_lock<std::mutex> _lock { m_mutex_sendToTextEditor };
    std::wstring _text = textForEditor;
    if (withClear) {
        textForEditor.clear();
    }
    return _text;
}
//=============================================================================
}
