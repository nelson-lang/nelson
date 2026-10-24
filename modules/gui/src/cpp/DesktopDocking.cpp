//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QEvent>
#include <QtCore/QPointer>
#include <QtGui/QCloseEvent>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLayout>
#include "DesktopDocking.hpp"
#include "ScreenInfo.hpp"
#include <QtCore/QSettings>
#include "NelsonSettingsScope.hpp"
#include "Nelson_VERSION.h"
#include "MainGuiObject.hpp"
#include "QtDockTitleBar.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// marks the docks created here, so they can be told apart from the desktop's
// own docks (command window, browsers, ...) when grouping them as tabs.
static const char* CLIENT_DOCK_PROPERTY = "nelsonClientWindowDock";
// top-level geometry saved at dock time, restored on undock.
static const char* UNDOCKED_GEOMETRY_PROPERTY = "nelsonUndockedGeometry";
//=============================================================================
static QMainWindow*
desktopMainWindow()
{
    return static_cast<QMainWindow*>(GetMainGuiObject());
}
//=============================================================================
static QDockWidget*
clientDock(const QWidget* client)
{
    if (client == nullptr) {
        return nullptr;
    }
    QDockWidget* dock = qobject_cast<QDockWidget*>(client->parentWidget());
    if (dock && dock->property(CLIENT_DOCK_PROPERTY).toBool()) {
        return dock;
    }
    return nullptr;
}
//=============================================================================
static const char* CLIENT_DOCK_BRIDGE_NAME = "nelsonClientDockBridge";
//=============================================================================
// Ties the dock and its hosted client window together:
// - closing the dock is forwarded to the client, so the client's own close
//   logic (unsaved files prompt, CloseRequestFcn, ...) stays in charge and
//   the dock only closes if the client accepted;
// - the client hiding/showing itself (quit action, `close`, ...) drives the
//   dock visibility, so no empty dock frame is left behind.
class ClientDockBridge : public QObject
{
public:
    ClientDockBridge(QDockWidget* dock, QMainWindow* client)
        : QObject(dock), m_dock(dock), m_client(client)
    {
        setObjectName(CLIENT_DOCK_BRIDGE_NAME);
        dock->installEventFilter(this);
        client->installEventFilter(this);
    }

    void
    detachFromClient()
    {
        if (m_client) {
            m_client->removeEventFilter(this);
        }
        m_client = nullptr;
    }

protected:
    bool
    eventFilter(QObject* watched, QEvent* event) override
    {
        if (m_client && m_dock) {
            if (watched == m_dock && event->type() == QEvent::Close) {
                QPointer<QMainWindow> client(m_client);
                if (client && client->isVisible() && !client->close()) {
                    event->ignore();
                    return true;
                }
            } else if (watched == m_client) {
                // HideToParent/ShowToParent are only sent for hide()/show()
                // calls on the client itself, never when the dock drives the
                // visibility: no recursion possible.
                if (event->type() == QEvent::HideToParent && !m_dock->isHidden()) {
                    m_dock->hide();
                } else if (event->type() == QEvent::ShowToParent && m_dock->isHidden()) {
                    m_dock->show();
                }
            }
        }
        return QObject::eventFilter(watched, event);
    }

private:
    QPointer<QDockWidget> m_dock;
    QPointer<QMainWindow> m_client;
};
//=============================================================================
static void
detachClientDockBridge(QDockWidget* dock)
{
    for (QObject* child : dock->children()) {
        if (child->objectName() == CLIENT_DOCK_BRIDGE_NAME) {
            static_cast<ClientDockBridge*>(child)->detachFromClient();
        }
    }
}
//=============================================================================
bool
DesktopDockingAvailable()
{
    return desktopMainWindow() != nullptr;
}
//=============================================================================
bool
DesktopIsWindowDocked(const QWidget* client)
{
    return clientDock(client) != nullptr;
}
//=============================================================================
bool
DesktopDockWindow(QMainWindow* client, const QString& dockObjectName)
{
    QMainWindow* desktop = desktopMainWindow();
    if (desktop == nullptr || client == nullptr || DesktopIsWindowDocked(client)) {
        return false;
    }
    client->setProperty(UNDOCKED_GEOMETRY_PROPERTY, client->saveGeometry());

    QDockWidget* dock = new QDockWidget(client->windowTitle(), desktop);
    dock->setObjectName(dockObjectName);
    dock->setProperty(CLIENT_DOCK_PROPERTY, true);
    // floating stays disabled on purpose: leaving the desktop must produce a
    // real native window (undock callback below), never a Qt tool panel.
    dock->setFeatures(QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable);
    QPointer<QMainWindow> clientPointer(client);
    dock->setTitleBarWidget(new QtDockTitleBar(dock, QString(), [clientPointer]() {
        if (clientPointer) {
            DesktopUndockWindow(clientPointer);
        }
    }));
    dock->setWidget(client);
    QObject::connect(client, &QWidget::windowTitleChanged, dock, &QWidget::setWindowTitle);
    new ClientDockBridge(dock, client);
    // if the client dies while docked (deleted by the interpreter), the dock
    // must not survive as an empty panel.
    QPointer<QDockWidget> dockPointer(dock);
    QObject::connect(client, &QObject::destroyed, dock, [dockPointer]() {
        if (dockPointer) {
            dockPointer->deleteLater();
        }
    });

    // an already docked client window to group the new one with, as tabs,
    // rather than stacking a new column.
    QDockWidget* siblingClient = nullptr;
    for (QDockWidget* other : desktop->findChildren<QDockWidget*>()) {
        if (other != dock && other->property(CLIENT_DOCK_PROPERTY).toBool() && !other->isFloating()
            && other->isVisible()) {
            siblingClient = other;
            break;
        }
    }

    if (siblingClient != nullptr) {
        desktop->tabifyDockWidget(siblingClient, dock);
    } else {
        // first docked client window: place it next to the command window so it
        // takes the large central area, not the narrow browser column on the
        // right. Splitting the command window keeps figures and the console
        // side by side.
        QDockWidget* commandWindow = desktop->findChild<QDockWidget*>("commandWindowDock");
        if (commandWindow != nullptr && !commandWindow->isFloating()
            && commandWindow->isVisible()) {
            desktop->splitDockWidget(commandWindow, dock, Qt::Horizontal);
        } else {
            desktop->addDockWidget(Qt::RightDockWidgetArea, dock);
        }
    }
    client->show();
    dock->show();
    dock->raise();
    return true;
}
//=============================================================================
bool
DesktopUndockWindow(QMainWindow* client, bool showAsWindow)
{
    QDockWidget* dock = clientDock(client);
    if (dock == nullptr) {
        return false;
    }
    const QByteArray clientLayout = client->saveState();
    QMainWindow* desktop = qobject_cast<QMainWindow*>(dock->parentWidget());
    QObject::disconnect(client, nullptr, dock, nullptr);
    detachClientDockBridge(dock);
    dock->setWidget(nullptr);
    client->setParent(nullptr);
    if (desktop != nullptr) {
        desktop->removeDockWidget(dock);
    }
    dock->deleteLater();

    client->restoreState(clientLayout);
    QByteArray geometry = client->property(UNDOCKED_GEOMETRY_PROPERTY).toByteArray();
    if (!geometry.isEmpty()) {
        client->restoreGeometry(geometry);
    }
    client->setProperty(UNDOCKED_GEOMETRY_PROPERTY, QVariant());
    if (showAsWindow) {
        client->show();
        client->raise();
        client->activateWindow();
    }
    if (client->layout()) {
        client->layout()->invalidate();
        client->layout()->activate();
    }
    return true;
}
//=============================================================================
bool
DesktopShowDockedWindow(QMainWindow* client)
{
    QDockWidget* dock = clientDock(client);
    if (dock == nullptr) {
        return false;
    }
    client->show();
    dock->show();
    dock->raise();
    QMainWindow* desktop = qobject_cast<QMainWindow*>(dock->parentWidget());
    if (desktop != nullptr) {
        if (desktop->isMinimized()) {
            desktop->showNormal();
        }
        desktop->raise();
        desktop->activateWindow();
    }
    return true;
}
//=============================================================================
bool
DesktopHideDockedWindow(QMainWindow* client)
{
    QDockWidget* dock = clientDock(client);
    if (dock == nullptr) {
        return false;
    }
    dock->hide();
    return true;
}
//=============================================================================
QList<QDockWidget*>
DesktopDockedClientWindows()
{
    QList<QDockWidget*> docks;
    QMainWindow* desktop = desktopMainWindow();
    if (desktop != nullptr) {
        for (QDockWidget* dock : desktop->findChildren<QDockWidget*>()) {
            if (dock->property(CLIENT_DOCK_PROPERTY).toBool()) {
                docks.append(dock);
            }
        }
    }
    return docks;
}
//=============================================================================
// client docks that take part in tiling/tabbing: not floating, and hosting a
// client that wants to be shown. A background tab is only hidden by the tab
// stack (not explicitly), while a closed editor or an invisible figure hides
// its client window itself: isHidden() tells both cases apart.
static QList<QDockWidget*>
openClientDocks()
{
    QList<QDockWidget*> docks;
    for (QDockWidget* dock : DesktopDockedClientWindows()) {
        QWidget* client = dock->widget();
        if (!dock->isFloating() && client != nullptr && !client->isHidden()) {
            docks.append(dock);
        }
    }
    return docks;
}
//=============================================================================
int
DesktopOpenClientWindowCount()
{
    return openClientDocks().size();
}
//=============================================================================
bool
DesktopTileClientWindows()
{
    QMainWindow* desktop = desktopMainWindow();
    if (desktop == nullptr) {
        return false;
    }
    QList<QDockWidget*> docks = openClientDocks();
    int n = docks.size();
    if (n < 2) {
        return false;
    }
    int cols = 1;
    while (cols * cols < n) {
        cols++;
    }
    int rows = (n + cols - 1) / cols;

    // detach every dock from its current tab stack first, then rebuild the
    // grid: row anchors are split vertically, row members horizontally.
    for (QDockWidget* dock : docks) {
        desktop->removeDockWidget(dock);
    }
    QDockWidget* previousRowStart = nullptr;
    for (int r = 0; r < rows; ++r) {
        QDockWidget* rowStart = docks[r * cols];
        if (previousRowStart == nullptr) {
            desktop->addDockWidget(Qt::RightDockWidgetArea, rowStart);
        } else {
            desktop->splitDockWidget(previousRowStart, rowStart, Qt::Vertical);
        }
        rowStart->show();
        previousRowStart = rowStart;
        for (int c = 1; c < cols; ++c) {
            int index = r * cols + c;
            if (index >= n) {
                break;
            }
            desktop->splitDockWidget(docks[index - 1], docks[index], Qt::Horizontal);
            docks[index]->show();
        }
    }
    // even cells: without this the split gives most of the space to the first
    // dock of each split pair.
    QList<int> widths;
    QList<int> heights;
    for (int i = 0; i < n; ++i) {
        widths.append(qMax(1, desktop->width() / cols));
        heights.append(qMax(1, desktop->height() / rows));
    }
    desktop->resizeDocks(docks, widths, Qt::Horizontal);
    desktop->resizeDocks(docks, heights, Qt::Vertical);
    return true;
}
//=============================================================================
bool
DesktopTabClientWindows()
{
    QMainWindow* desktop = desktopMainWindow();
    if (desktop == nullptr) {
        return false;
    }
    QList<QDockWidget*> docks = openClientDocks();
    if (docks.size() < 2) {
        return false;
    }
    QDockWidget* first = docks[0];
    desktop->addDockWidget(Qt::RightDockWidgetArea, first);
    first->show();
    for (int i = 1; i < docks.size(); ++i) {
        desktop->tabifyDockWidget(first, docks[i]);
        docks[i]->show();
    }
    first->show();
    first->raise();
    return true;
}
//=============================================================================
bool
DesktopPrefersDockedFigures()
{
    if (!DesktopDockingAvailable()) {
        return false;
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    return settings.value("figure_DockNewInDesktop", false).toBool();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
