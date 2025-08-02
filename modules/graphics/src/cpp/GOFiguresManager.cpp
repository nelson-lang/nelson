//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOHelpers.hpp"
#include "NonClosableWidget.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::map<int64, GOWindow*> hFigures;
static int64 GOCurrentFig = NO_FIGURE;
static QWidget* _currentFocus = nullptr;
// static NonClosableWidget* wid = nullptr;
// static bool NonGUIModeHack = false;
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
createNewFigure(bool show)
{
    int64 figNum = findAvailableFigureId();
    checkIdValidity(figNum);
    hFigures[figNum] = new GOWindow(figNum);
    if (show) {
        saveFocus();
        hFigures[figNum]->show();
        restoreFocus();
    }
    GOCurrentFig = figNum;
    return GOCurrentFig;
}
//=============================================================================
int64
selectFigure(int64 fignum, bool show)
{
    checkIdValidity(fignum);
    if (hFigures.count(fignum) == 0
        || ((hFigures.count(fignum) == 1) && getFigure(fignum) == nullptr)) {
        hFigures[fignum] = new GOWindow(fignum);
    }
    GOCurrentFig = fignum;
    if (show) {
        saveFocus();
        hFigures[fignum]->show();
        hFigures[fignum]->raise();
        restoreFocus();
    }
    return GOCurrentFig;
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
    GOCurrentFig = figNum;
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
        if (it->second) {
            closeFigure(it->first);
        }
    }
}
//=============================================================================
bool
closeFigure(go_handle fignum, bool forceClose)
{
    GOWindow* win = getFigure(fignum);
    if (win) {
        if (forceClose) {
            win->hide();
        }
        GOFigure* fig = win->getGOFigure();
        if (fig) {
            fig->setRenderingStateInvalid(true);
            GOGObjectsProperty* hp
                = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
            if (hp) {
                std::vector<int64> children(hp->data());
                if (!children.empty()) {
                    for (auto c : children) {
                        deleteGraphicsObject(c, false, false);
                    }
                    children.clear();
                    hp->data(children);
                }
            }
        }
        if (forceClose) {
            delete win;
        }
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
}
//=============================================================================
