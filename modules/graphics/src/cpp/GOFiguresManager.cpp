//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "NonClosableWidget.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::map<int64, GOWindow*> hFigures;
static int64 GOCurrentFig = NO_FIGURE;
static QWidget* _currentFocus = nullptr;
static NonClosableWidget* wid = nullptr;
static bool NonGUIModeHack = false;
//=============================================================================
void
saveFocus()
{
    _currentFocus = qApp->focusWidget();
}
//=============================================================================
void
restoreFocus()
{
    if (_currentFocus) {
        _currentFocus->setFocus();
    }
}
//=============================================================================
std::map<int64, GOWindow*>
getFigureList()
{
    return hFigures;
}
//=============================================================================
int64
findAvailableFigureId()
{
    int64 figureNumber = 0;
    bool figureAvailable = false;
    while ((figureNumber < MAX_FIGS) && !figureAvailable) {
        figureAvailable = (hFigures[figureNumber] == nullptr);
        if (!figureAvailable) {
            figureNumber++;
        } else {
            return figureNumber;
        }
    }
    Error(_("Please close some graphical windows."));
    return NO_FIGURE;
}
//=============================================================================
int64
createNewFigure()
{
    int64 figNum = findAvailableFigureId();
    checkIdValidity(figNum);
    hFigures[figNum] = new GOWindow(figNum);
    saveFocus();
    hFigures[figNum]->show();
    restoreFocus();
    GOCurrentFig = figNum;
    return GOCurrentFig;
}
//=============================================================================
int64
selectFigure(int64 fignum)
{
    checkIdValidity(fignum);
    if (hFigures.count(fignum) == 0
        || ((hFigures.count(fignum) == 1) && getFigure(fignum) == nullptr)) {
        hFigures[fignum] = new GOWindow(fignum);
    }
    GOCurrentFig = fignum;
    saveFocus();
    hFigures[fignum]->show();
    hFigures[fignum]->raise();
    restoreFocus();
    return GOCurrentFig;
}
//=============================================================================
bool
closeFigure(go_handle fignum)
{
    GOWindow* win = getFigure(fignum);
    if (win) {
        win->hide();
        delete win;
        win = nullptr;
        hFigures[fignum] = nullptr;
        if (GOCurrentFig == fignum) {
            std::vector<int64> nums = getFigureGraphicsObjects();
            if (nums.empty()) {
                GOCurrentFig = NO_FIGURE;
            } else {
                GOCurrentFig = nums.back();
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
GOFigure*
getCurrentGOFigure()
{
    if (GOCurrentFig == NO_FIGURE) {
        createNewFigure();
    }
    return hFigures[GOCurrentFig]->getGOFigure();
}
//=============================================================================
GOWindow*
getFigure(int64 id)
{
    if (hFigures.count(id) == 1 && hFigures[id] != nullptr) {
        return hFigures[id];
    }
    return nullptr;
}
//=============================================================================
std::vector<int64>
getFigureGraphicsObjects()
{
    std::map<int64, GOWindow*>::iterator it;
    std::vector<int64> gos;
    for (it = hFigures.begin(); it != hFigures.end(); ++it) {
        if (it->second) {
            gos.push_back(it->first);
        }
    }
    return gos;
}
//=============================================================================
int64
getCurrentFigure()
{
    return GOCurrentFig;
}
//=============================================================================
void
notifyCurrentFigureChanged(int64 figNum)
{
    // Currently commented, need a function to disable notification
    // without this, tests are unpredictable ...
    // GOCurrentFig = figNum;
}
//=============================================================================
void
notifyFigureClosed(int64 figNum)
{
    hFigures[figNum] = nullptr;
    if (figNum == GOCurrentFig) {
        GOCurrentFig = NO_FIGURE;
    }

    // Check for all figures closed
    /*
    bool allClosed = true;

    std::map<int64, GOWindow*>::iterator it;
    for (it = hFigures.begin(); allClosed && it != hFigures.end(); ++it) {
        allClosed = (it->second == nullptr);
    }
    if (allClosed && NonGUIModeHack) {
        if (!wid) {
            wid = new NonClosableWidget;
            wid->setGeometry(0, 0, 1, 1);
            wid->setWindowIcon(QIcon(":/images/Nelson_small_mod_64.png"));
            wid->show();
        } else {
            wid->show();
        }
    }*/
}
//=============================================================================
GOWindow*
getHandleWindow(int64 fignum)
{
    if (hFigures.count(fignum) == 1) {
        return hFigures[fignum];
    }
    return nullptr;
}
//=============================================================================
void
initializeGraphic()
{
    hFigures.clear();
    GOCurrentFig = NO_FIGURE;
}
//=============================================================================
void
shutdownGraphic()
{
    std::map<int64, GOWindow*>::iterator it;
    for (it = hFigures.begin(); it != hFigures.end(); ++it) {
        closeFigure(it->first);
    }
}
//=============================================================================
}
//=============================================================================
