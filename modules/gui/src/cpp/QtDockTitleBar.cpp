//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "NelsonSettingsScope.hpp"
#include <QtCore/QSettings>
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <QtCore/QPointF>
#include <QtGui/QAction>
#include <QtGui/QColor>
#include <QtGui/QIcon>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>
#include <QtGui/QPalette>
#include <QtGui/QPen>
#include <QtGui/QPixmap>
#include <QtGui/QPolygonF>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
//=============================================================================
using namespace Nelson;
//=============================================================================
const QString SETTING_DOCK_COLLAPSED_SUFFIX = "_Collapsed";
const QString SETTING_DOCK_EXPANDED_HEIGHT_SUFFIX = "_ExpandedHeight";
const QSize DOCK_TITLE_BUTTON_SIZE(32, 32);
const QSize DOCK_TITLE_ICON_SIZE(16, 16);
//=============================================================================
static void
configureDockTitleButton(QToolButton* button)
{
    if (!button) {
        return;
    }
    button->setMinimumSize(DOCK_TITLE_BUTTON_SIZE);
    button->setIconSize(DOCK_TITLE_ICON_SIZE);
}
//=============================================================================
// A crisp chevron drawn at the icon size: large enough to read, aligned with
// the title label, and sharp on high-DPI screens via the device pixel ratio.
static QIcon
makeChevronIcon(bool down, const QWidget* reference)
{
    const qreal dpr = reference ? reference->devicePixelRatioF() : 1.0;
    const int side = DOCK_TITLE_ICON_SIZE.width();
    QPixmap pixmap(QSize(side, side) * dpr);
    pixmap.setDevicePixelRatio(dpr);
    pixmap.fill(Qt::transparent);

    QPainter painter(&pixmap);
    painter.setRenderHint(QPainter::Antialiasing, true);
    QColor color = reference ? reference->palette().color(QPalette::ButtonText) : QColor(Qt::black);
    QPen pen(color);
    pen.setWidthF(1.6);
    pen.setCapStyle(Qt::RoundCap);
    pen.setJoinStyle(Qt::RoundJoin);
    painter.setPen(pen);

    // chevron ~10 x 6 px inside the 16 px box, centered.
    const qreal near = side * 0.30;
    const qreal far = side * 0.70;
    const qreal mid = side * 0.50;
    const qreal top = side * 0.38;
    const qreal bottom = side * 0.62;
    if (down) {
        painter.drawPolyline(
            QPolygonF({ QPointF(near, top), QPointF(mid, bottom), QPointF(far, top) }));
    } else {
        painter.drawPolyline(
            QPolygonF({ QPointF(top, near), QPointF(bottom, mid), QPointF(top, far) }));
    }
    painter.end();
    return QIcon(pixmap);
}
//=============================================================================
QtDockTitleBar::QtDockTitleBar(
    QDockWidget* dock, const QString& settingsPrefix, std::function<void()> undockCallback)
    : QWidget(dock)
    , m_dock(dock)
    , m_settingsPrefix(settingsPrefix)
    , m_undockCallback(std::move(undockCallback))
    , m_collapsed(false)
    , m_expandedHeight(-1)
    , m_expandedMinimumHeight(-1)
{
    setObjectName("NelsonDockTitleBar");

    QHBoxLayout* layout = new QHBoxLayout(this);
    layout->setContentsMargins(4, 2, 4, 2);
    layout->setSpacing(2);

    m_collapseButton = new QToolButton(this);
    m_collapseButton->setObjectName("NelsonDockToolButton");
    m_collapseButton->setAutoRaise(true);
    m_collapseButton->setFocusPolicy(Qt::StrongFocus);
    configureDockTitleButton(m_collapseButton);
    connect(m_collapseButton, &QToolButton::clicked, this, &QtDockTitleBar::toggleCollapsed);
    layout->addWidget(m_collapseButton);

    m_titleLabel = new QLabel(dock->windowTitle(), this);
    m_titleLabel->setObjectName("NelsonDockTitleLabel");
    connect(dock, &QWidget::windowTitleChanged, m_titleLabel, &QLabel::setText);
    layout->addWidget(m_titleLabel);
    layout->addStretch();

    m_actionsButton = new QToolButton(this);
    m_actionsButton->setObjectName("NelsonDockToolButton");
    m_actionsButton->setAutoRaise(true);
    m_actionsButton->setFocusPolicy(Qt::StrongFocus);
    configureDockTitleButton(m_actionsButton);
    m_actionsButton->setText(QString::fromUtf8("⋯"));
    m_actionsButton->setToolTip(TR("Panel actions"));
    m_actionsButton->setAccessibleName(TR("Panel actions"));
    m_actionsButton->setPopupMode(QToolButton::InstantPopup);
    QMenu* actionsMenu = new QMenu(m_actionsButton);
    connect(actionsMenu, &QMenu::aboutToShow, this, &QtDockTitleBar::rebuildActionsMenu);
    m_actionsButton->setMenu(actionsMenu);
    layout->addWidget(m_actionsButton);

    connect(m_dock, &QDockWidget::topLevelChanged, this, &QtDockTitleBar::onTopLevelChanged);

    updateCollapseButton();
    if (m_undockCallback) {
        // a docked client window is a full window on its own: collapsing it to
        // a bare title bar makes no sense, undocking is the way out.
        m_collapseButton->hide();
    }
}
//=============================================================================
bool
QtDockTitleBar::isCollapsed() const
{
    return m_collapsed;
}
//=============================================================================
void
QtDockTitleBar::setCollapsed(bool collapsed)
{
    if (m_collapsed == collapsed) {
        return;
    }
    // a floating dock is a plain window: it cannot be collapsed
    if (collapsed && m_dock->isFloating()) {
        return;
    }
    applyCollapsed(collapsed);
    saveState();
}
//=============================================================================
void
QtDockTitleBar::applyCollapsed(bool collapsed)
{
    QWidget* content = m_dock->widget();
    if (content == nullptr) {
        return;
    }
    if (collapsed) {
        if (m_dock->height() > this->sizeHint().height()) {
            m_expandedHeight = m_dock->height();
        }
        m_expandedMinimumHeight = m_dock->minimumHeight();
        content->hide();
        // the dock minimum height (50 in the browsers) would win over a
        // smaller maximum height: both bounds are pinned to the title bar.
        int collapsedHeight = this->sizeHint().height() + 4;
        m_dock->setMinimumHeight(collapsedHeight);
        m_dock->setMaximumHeight(collapsedHeight);
    } else {
        content->show();
        if (m_expandedMinimumHeight >= 0) {
            m_dock->setMinimumHeight(m_expandedMinimumHeight);
        } else {
            m_dock->setMinimumHeight(0);
        }
        m_dock->setMaximumHeight(QWIDGETSIZE_MAX);
        if (m_expandedHeight > 0) {
            QMainWindow* mainWindow = qobject_cast<QMainWindow*>(m_dock->parentWidget());
            if (mainWindow != nullptr) {
                mainWindow->resizeDocks({ m_dock }, { m_expandedHeight }, Qt::Vertical);
            }
        }
    }
    m_collapsed = collapsed;
    updateCollapseButton();
}
//=============================================================================
void
QtDockTitleBar::updateCollapseButton()
{
    // A painter-drawn chevron icon is sized exactly by setIconSize(). The
    // native setArrowType() paints the arrow at the button rect size and
    // ignores setIconSize(), making the chevron far larger than the title.
    m_collapseButton->setArrowType(Qt::NoArrow);
    m_collapseButton->setIcon(makeChevronIcon(!m_collapsed, m_collapseButton));
    QString text = m_collapsed ? TR("Expand panel") : TR("Collapse panel");
    m_collapseButton->setToolTip(text);
    m_collapseButton->setAccessibleName(text);
}
//=============================================================================
void
QtDockTitleBar::toggleCollapsed()
{
    setCollapsed(!m_collapsed);
}
//=============================================================================
void
QtDockTitleBar::toggleFloating()
{
    if (!m_dock->features().testFlag(QDockWidget::DockWidgetFloatable)) {
        return;
    }
    m_dock->setFloating(!m_dock->isFloating());
}
//=============================================================================
void
QtDockTitleBar::onTopLevelChanged(bool floating)
{
    if (floating) {
        setCollapsed(false);
    }
    m_collapseButton->setVisible(!floating);
}
//=============================================================================
void
QtDockTitleBar::moveDock(Qt::DockWidgetArea area)
{
    QMainWindow* mainWindow = qobject_cast<QMainWindow*>(m_dock->parentWidget());
    if (!mainWindow || m_dock->isFloating()) {
        return;
    }
    mainWindow->addDockWidget(area, m_dock);
    m_dock->show();
    m_dock->raise();
}
//=============================================================================
void
QtDockTitleBar::rebuildActionsMenu()
{
    QMenu* menu = m_actionsButton->menu();
    if (!menu) {
        return;
    }
    menu->clear();
    if (!m_undockCallback && !m_dock->isFloating()) {
        QAction* collapseAction
            = menu->addAction(m_collapsed ? TR("Expand panel") : TR("Collapse panel"));
        connect(collapseAction, &QAction::triggered, this, &QtDockTitleBar::toggleCollapsed);
    }
    if (m_undockCallback) {
        QAction* undockAction = menu->addAction(TR("Undock"));
        undockAction->setObjectName("nelsonUndockAction");
        connect(undockAction, &QAction::triggered, this, [this]() { m_undockCallback(); });
    } else if (m_dock->features().testFlag(QDockWidget::DockWidgetFloatable)) {
        QAction* floatAction
            = menu->addAction(m_dock->isFloating() ? TR("Dock panel") : TR("Float panel"));
        connect(floatAction, &QAction::triggered, this, &QtDockTitleBar::toggleFloating);
    }
    if (!m_dock->isFloating() && m_dock->features().testFlag(QDockWidget::DockWidgetMovable)) {
        connect(menu->addAction(TR("Move Left")), &QAction::triggered, this,
            [this]() { moveDock(Qt::LeftDockWidgetArea); });
        connect(menu->addAction(TR("Move Right")), &QAction::triggered, this,
            [this]() { moveDock(Qt::RightDockWidgetArea); });
        connect(menu->addAction(TR("Move Bottom")), &QAction::triggered, this,
            [this]() { moveDock(Qt::BottomDockWidgetArea); });
    }
    if (m_dock->features().testFlag(QDockWidget::DockWidgetClosable)) {
        if (!menu->isEmpty()) {
            menu->addSeparator();
        }
        connect(menu->addAction(TR("Close panel")), &QAction::triggered, m_dock, &QWidget::close);
    }
}
//=============================================================================
void
QtDockTitleBar::mouseDoubleClickEvent(QMouseEvent* event)
{
    if (m_undockCallback) {
        m_undockCallback();
        event->accept();
        return;
    }
    // the native title bar toggles the floating state on double-click: a
    // custom title bar widget has to bring that behavior back itself.
    toggleFloating();
    QWidget::mouseDoubleClickEvent(event);
}
//=============================================================================
void
QtDockTitleBar::restoreState()
{
    // an empty prefix disables persistence (dynamic docks such as terminals)
    if (m_settingsPrefix.isEmpty()) {
        return;
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    int savedExpandedHeight
        = settings.value(m_settingsPrefix + SETTING_DOCK_EXPANDED_HEIGHT_SUFFIX, -1).toInt();
    bool collapsed
        = settings.value(m_settingsPrefix + SETTING_DOCK_COLLAPSED_SUFFIX, false).toBool();
    if (collapsed && !m_dock->isFloating()) {
        applyCollapsed(true);
    }
    // applied after the collapse: at startup the pre-layout dock height is
    // meaningless, the persisted expanded height must win for the next expand.
    if (savedExpandedHeight > 0) {
        m_expandedHeight = savedExpandedHeight;
    }
}
//=============================================================================
void
QtDockTitleBar::saveState()
{
    if (m_settingsPrefix.isEmpty()) {
        return;
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(m_settingsPrefix + SETTING_DOCK_COLLAPSED_SUFFIX, m_collapsed);
    if (m_expandedHeight > 0) {
        settings.setValue(m_settingsPrefix + SETTING_DOCK_EXPANDED_HEIGHT_SUFFIX, m_expandedHeight);
    }
}
//=============================================================================
