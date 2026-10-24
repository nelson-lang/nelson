//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DesktopPanelRail.h"
#include "DesktopUiStyle.h"
#include "QtTranslation.hpp"
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QMenu>
#include <QtWidgets/QStyle>
#include <QtWidgets/QToolButton>
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
refreshActionButton(QToolButton* button, QAction* action)
{
    if (!button || !action) {
        return;
    }
    button->setAccessibleName(desktopAccessibleActionText(action));
    button->setToolTip(action->toolTip());
    button->setStatusTip(action->statusTip());
}
//=============================================================================
DesktopPanelRail::DesktopPanelRail(QWidget* parent) : QToolBar(TR("Panels sidebar"), parent)
{
    setObjectName("NelsonPanelsSidebar");
    setAccessibleName(TR("Panels sidebar"));
    setMovable(false);
    setFloatable(false);
    setAllowedAreas(Qt::LeftToolBarArea);
#ifdef Q_OS_MACOS
    setIconSize(QSize(20, 20));
#else
    setIconSize(QSize(18, 18));
#endif
    setToolButtonStyle(Qt::ToolButtonIconOnly);
    m_moreMenu = new QMenu(this);
}
//=============================================================================
QAction*
DesktopPanelRail::createProxyAction(QAction* sourceAction, bool panelAction)
{
    if (!sourceAction) {
        return nullptr;
    }
    QAction* proxy = new QAction(sourceAction->icon(), sourceAction->text(), this);
    proxy->setToolTip(sourceAction->toolTip());
    proxy->setStatusTip(sourceAction->statusTip());
    proxy->setCheckable(panelAction);
    proxy->setChecked(panelAction && sourceAction->isChecked());
    proxy->setEnabled(sourceAction->isEnabled());
    connect(sourceAction, &QAction::changed, this,
        [this, sourceAction]() { refreshAction(sourceAction); });
    if (panelAction) {
        connect(proxy, &QAction::triggered, this, [this, sourceAction]() {
            if (QAction* proxy = m_proxies.value(sourceAction)) {
                bool visible = sourceAction->isChecked();
                if (QDockWidget* dock = m_docks.value(sourceAction)) {
                    visible = dock->isVisible();
                }
                proxy->setChecked(visible);
            }
            emit panelRequested(sourceAction);
        });
    } else {
        connect(proxy, &QAction::triggered, sourceAction, &QAction::trigger);
    }
    m_proxies.insert(sourceAction, proxy);
    return proxy;
}
//=============================================================================
void
DesktopPanelRail::addPanelAction(QAction* sourceAction)
{
    QAction* proxy = createProxyAction(sourceAction, true);
    if (proxy) {
        addAction(proxy);
        refreshActionButton(qobject_cast<QToolButton*>(widgetForAction(proxy)), proxy);
        applyDesktopToolbarMetrics(this);
        m_moreMenu->addAction(sourceAction);
    }
}
//=============================================================================
void
DesktopPanelRail::addCommandAction(QAction* sourceAction)
{
    QAction* proxy = createProxyAction(sourceAction, false);
    if (proxy) {
        addAction(proxy);
        refreshActionButton(qobject_cast<QToolButton*>(widgetForAction(proxy)), proxy);
        applyDesktopToolbarMetrics(this);
    }
}
//=============================================================================
void
DesktopPanelRail::addRailSeparator()
{
    addSeparator();
    if (!m_moreMenu->actions().isEmpty() && !m_moreMenu->actions().last()->isSeparator()) {
        m_moreMenu->addSeparator();
    }
}
//=============================================================================
void
DesktopPanelRail::bindDock(QAction* sourceAction, QDockWidget* dock)
{
    if (!sourceAction || !dock) {
        return;
    }
    m_docks.insert(sourceAction, dock);
    connect(dock, &QDockWidget::visibilityChanged, this, [this, sourceAction](bool visible) {
        if (QAction* proxy = m_proxies.value(sourceAction)) {
            proxy->setChecked(visible);
        }
        refreshButtonProperties();
    });
    refreshAction(sourceAction);
}
//=============================================================================
void
DesktopPanelRail::setActiveDock(QDockWidget* dock)
{
    if (m_activeDock == dock) {
        return;
    }
    m_activeDock = dock;
    refreshButtonProperties();
}
//=============================================================================
void
DesktopPanelRail::complete()
{
    QToolButton* moreButton = new QToolButton(this);
    moreButton->setObjectName("NelsonPanelsMoreButton");
    moreButton->setText(QString::fromUtf8("⋯"));
    moreButton->setToolTip(TR("More panels"));
    moreButton->setAccessibleName(TR("More panels"));
    moreButton->setPopupMode(QToolButton::InstantPopup);
    moreButton->setMenu(m_moreMenu);
    addWidget(moreButton);
    applyDesktopToolbarMetrics(this);
    refreshButtonProperties();
}
//=============================================================================
void
DesktopPanelRail::refreshAction(QAction* sourceAction)
{
    QAction* proxy = m_proxies.value(sourceAction);
    if (!proxy || !sourceAction) {
        return;
    }
    proxy->setIcon(sourceAction->icon());
    proxy->setText(sourceAction->text());
    proxy->setToolTip(sourceAction->toolTip());
    proxy->setStatusTip(sourceAction->statusTip());
    proxy->setEnabled(sourceAction->isEnabled());
    if (proxy->isCheckable()) {
        proxy->setChecked(sourceAction->isChecked());
    }
    refreshActionButton(qobject_cast<QToolButton*>(widgetForAction(proxy)), proxy);
    refreshButtonProperties();
}
//=============================================================================
void
DesktopPanelRail::refreshButtonProperties()
{
    for (auto it = m_docks.cbegin(); it != m_docks.cend(); ++it) {
        QAction* proxy = m_proxies.value(it.key());
        QToolButton* button = proxy ? qobject_cast<QToolButton*>(widgetForAction(proxy)) : nullptr;
        if (!button) {
            continue;
        }
        const bool active = it.value() && it.value() == m_activeDock && it.value()->isVisible();
        if (button->property("activePanel").toBool() != active) {
            button->setProperty("activePanel", active);
            button->style()->unpolish(button);
            button->style()->polish(button);
            button->update();
        }
    }
}
//=============================================================================
