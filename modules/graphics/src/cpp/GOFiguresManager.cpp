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
class GOFiguresManagerSingleton
{
private:
    std::map<int64, GOWindow*> hFigures;
    int64 GOCurrentFig = NO_FIGURE;
    QWidget* _currentFocus = nullptr;

    GOFiguresManagerSingleton() = default;

public:
    GOFiguresManagerSingleton(const GOFiguresManagerSingleton&) = delete;
    GOFiguresManagerSingleton&
    operator=(const GOFiguresManagerSingleton&)
        = delete;

    static GOFiguresManagerSingleton&
    getInstance()
    {
        static GOFiguresManagerSingleton instance;
        return instance;
    }

    std::map<int64, GOWindow*>&
    getFigures()
    {
        return hFigures;
    }
    const std::map<int64, GOWindow*>&
    getFigures() const
    {
        return hFigures;
    }

    int64
    getCurrentFig() const
    {
        return GOCurrentFig;
    }
    void
    setCurrentFig(int64 fig)
    {
        GOCurrentFig = fig;
    }

    QWidget*
    getCurrentFocus() const
    {
        return _currentFocus;
    }
    void
    setCurrentFocus(QWidget* focus)
    {
        _currentFocus = focus;
    }

    void
    clear()
    {
        hFigures.clear();
        GOCurrentFig = NO_FIGURE;
        _currentFocus = nullptr;
    }
};
//=============================================================================
void
saveFocus()
{
    GOFiguresManagerSingleton::getInstance().setCurrentFocus(qApp->focusWidget());
}
//=============================================================================
void
restoreFocus()
{
    QWidget* currentFocus = GOFiguresManagerSingleton::getInstance().getCurrentFocus();
    if (currentFocus) {
        currentFocus->setFocus();
    }
}
//=============================================================================
std::map<int64, GOWindow*>
getFigureList()
{
    return GOFiguresManagerSingleton::getInstance().getFigures();
}
//=============================================================================
int64
findAvailableFigureId()
{
    auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
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
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    int64 figNum = findAvailableFigureId();
    checkIdValidity(figNum);
    hFigures[figNum] = new GOWindow(figNum);
    if (show) {
        saveFocus();
        hFigures[figNum]->show();
        restoreFocus();
    }
    manager.setCurrentFig(figNum);
    return manager.getCurrentFig();
}
//=============================================================================
int64
selectFigure(int64 fignum, bool show)
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    checkIdValidity(fignum);
    if (hFigures.count(fignum) == 0
        || ((hFigures.count(fignum) == 1) && getFigure(fignum) == nullptr)) {
        hFigures[fignum] = new GOWindow(fignum);
    }
    manager.setCurrentFig(fignum);
    if (show) {
        saveFocus();
        hFigures[fignum]->show();
        hFigures[fignum]->raise();
        restoreFocus();
    }
    return manager.getCurrentFig();
}
//=============================================================================
GOFigure*
getCurrentGOFigure()
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

    if (manager.getCurrentFig() == NO_FIGURE) {
        createNewFigure();
    }
    return hFigures[manager.getCurrentFig()]->getGOFigure();
}
//=============================================================================
GOWindow*
getFigure(int64 id)
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    if (hFigures.count(id) == 1 && hFigures.at(id) != nullptr) {
        return hFigures.at(id);
    }
    return nullptr;
}
//=============================================================================
std::vector<int64>
getFigureGraphicsObjects()
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    std::vector<int64> gos;
    for (const auto& it : hFigures) {
        if (it.second) {
            gos.push_back(it.first);
        }
    }
    return gos;
}
//=============================================================================
int64
getCurrentFigure()
{
    return GOFiguresManagerSingleton::getInstance().getCurrentFig();
}
//=============================================================================
void
notifyCurrentFigureChanged(int64 figNum)
{
    GOFiguresManagerSingleton::getInstance().setCurrentFig(figNum);
}
//=============================================================================
GOWindow*
getHandleWindow(int64 fignum)
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    if (hFigures.count(fignum) == 1) {
        return hFigures.at(fignum);
    }
    return nullptr;
}
//=============================================================================
void
initializeGraphic()
{
    GOFiguresManagerSingleton::getInstance().clear();
}
//=============================================================================
void
shutdownGraphic()
{
    const auto& hFigures = GOFiguresManagerSingleton::getInstance().getFigures();
    for (const auto& it : hFigures) {
        if (it.second) {
            closeFigure(it.first);
        }
    }
}
//=============================================================================
bool
closeFigure(go_handle fignum, bool forceClose)
{
    auto& manager = GOFiguresManagerSingleton::getInstance();
    auto& hFigures = manager.getFigures();

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
        if (manager.getCurrentFig() == fignum) {
            std::vector<int64> nums = getFigureGraphicsObjects();
            if (nums.empty()) {
                manager.setCurrentFig(NO_FIGURE);
            } else {
                manager.setCurrentFig(nums.back());
            }
        }
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
