//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DesktopUiStyle.h"
#include <algorithm>
#include <QtCore/QFileInfo>
#include <QtCore/QHash>
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtGui/QActionGroup>
#include <QtGui/QPainter>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QWidget>
//=============================================================================
DesktopUiColors
desktopUiColors(const QPalette& palette)
{
    const bool dark = palette.color(QPalette::Window).lightness() < 128;
    DesktopUiColors colors;
    colors.window = dark ? QColor("#1e1e1e") : QColor("#ffffff");
    colors.panel = dark ? QColor("#252526") : QColor("#ffffff");
    colors.title = dark ? QColor("#252526") : QColor("#f3f3f3");
    colors.border = dark ? QColor("#333333") : QColor("#d4d4d4");
    colors.tab = dark ? QColor("#2d2d2d") : QColor("#ececec");
    colors.hover = dark ? QColor("#2a2d2e") : QColor("#e8e8e8");
    colors.sidebar = dark ? QColor("#333333") : QColor("#e8e8e8");
    colors.visible = dark ? QColor("#264f78") : QColor("#add6ff");
    colors.accent = dark ? QColor("#007acc") : QColor("#0072bd");
    colors.text = dark ? QColor("#d4d4d4") : QColor("#1f1f1f");
    colors.muted = dark ? QColor("#858585") : QColor("#6a6a6a");
    return colors;
}
//=============================================================================
static QString
colorName(const QColor& color)
{
    return color.name(QColor::HexArgb);
}
//=============================================================================
QString
desktopShellStyleSheet(const QPalette& palette)
{
    const DesktopUiColors color = desktopUiColors(palette);
    QString style(
        "QMainWindow#NelsonMainWindow::separator { width: 6px; height: 6px; }"
        "QMainWindow#NelsonMainWindow::separator:hover { background: %9; }"
        "QMainWindow#NelsonMainWindow { background: %1; }"
#ifndef Q_OS_MACOS
        "QMenuBar#NelsonMenuBar { min-height: 32px; background: %3; color: %10;"
        " border-bottom: 1px solid %4; }"
#endif
        "QStatusBar#NelsonStatusBar { min-height: 24px; max-height: 24px; background: %3;"
        " color: %10; border-top: 1px solid %4; }"
        "QStatusBar#NelsonStatusBar QLabel { color: %10; }"
        "QMainWindow#NelsonMainWindow QDockWidget { background: %2; border: 1px solid %4; }"
        "QWidget#NelsonDockTitleBar { background: %3; border-bottom: 1px solid %4; }"
        "QLabel#NelsonDockTitleLabel { color: %10; font-weight: 600; padding-left: 4px; }"
        "QToolButton#NelsonDockToolButton { border: 0; border-radius: 4px; padding: 3px; }"
        // the "⋯" button already signals its menu: drop the extra menu arrow.
        "QToolButton#NelsonDockToolButton::menu-indicator { image: none; width: 0; }"
        "QToolButton#NelsonDockToolButton:hover, QToolButton#NelsonDockToolButton:focus {"
        " background: %6; }"
        "QToolBar#NelsonPanelsSidebar { min-width: 40px; max-width: 40px; background: %7; border: "
        "0;"
        " border-right: 1px solid %4; padding: 6px 4px; spacing: 3px; }"
        "QToolBar#NelsonPanelsSidebar QToolButton { background: transparent; border: 0;"
        " border-left: 2px solid transparent; border-radius: 4px; margin: 0; padding: 0;"
        " min-width: 32px; min-height: 32px; }"
        "QToolBar#NelsonPanelsSidebar QToolButton:hover,"
        " QToolBar#NelsonPanelsSidebar QToolButton:focus { background: %6; }"
        "QToolBar#NelsonPanelsSidebar QToolButton:checked {"
        " background: %8; border-left-color: %9; }"
        "QToolBar#NelsonPanelsSidebar QToolButton[activePanel=\"true\"] {"
        " background: %9; border-left-color: %9; }"
        "QToolBar#NelsonPanelsSidebar::separator { background: %4; width: 20px; height: 1px;"
        " margin: 5px 6px; }"
        "QToolBar#NelsonMainToolBar { min-height: 38px; border: 0; border-bottom: 1px solid %4;"
        " spacing: 4px; }"
        "QToolBar#NelsonMainToolBar QToolButton { border: 0; border-radius: 4px;"
        " padding: 0; min-width: 32px; min-height: 32px; }"
        "QToolBar#NelsonMainToolBar QToolButton:hover,"
        " QToolBar#NelsonMainToolBar QToolButton:focus { background: %6; }"
        "QLineEdit#NelsonDirectoryField, QLineEdit#NelsonCommandSearch {"
        " border: 1px solid %4; border-radius: 6px; padding: 4px 7px; background: %2; }"
        "QLineEdit#NelsonDirectoryField:focus, QLineEdit#NelsonCommandSearch:focus {"
        " border-color: %9; }"
        "QMainWindow#NelsonMainWindow QTabBar::tab { background: %5; color: %11; border: 1px solid "
        "%4;"
        " padding: 5px 12px; margin-right: 1px; min-height: 22px; }"
        "QMainWindow#NelsonMainWindow QTabBar::tab:selected { background: %2; color: %10;"
        " border-bottom-color: %2; }"
        "QMainWindow#NelsonMainWindow QTabBar::tab:hover { background: %6; color: %10; }");
    const QString replacements[] = { colorName(color.window), colorName(color.panel),
        colorName(color.title), colorName(color.border), colorName(color.tab),
        colorName(color.hover), colorName(color.sidebar), colorName(color.visible),
        colorName(color.accent), colorName(color.text), colorName(color.muted) };
    for (int index = 11; index >= 1; --index) {
        style.replace(QString("%") + QString::number(index), replacements[index - 1]);
    }
    return style;
}
//=============================================================================
static QPixmap
tintedPixmap(const QIcon& source, const QColor& tint, const QSize& size, qreal dpr)
{
    const QSize physicalSize(
        qMax(1, qRound(size.width() * dpr)), qMax(1, qRound(size.height() * dpr)));
    QPixmap pixmap = source.pixmap(physicalSize);
    pixmap.setDevicePixelRatio(dpr);
    if (pixmap.isNull()) {
        return pixmap;
    }
    QPainter painter(&pixmap);
    painter.setCompositionMode(QPainter::CompositionMode_SourceIn);
    painter.fillRect(pixmap.rect(), tint);
    return pixmap;
}
//=============================================================================
QList<qreal>
desktopIconDevicePixelRatios()
{
    QList<qreal> ratios = { 1.0, 1.25, 1.5, 2.0, 3.0 };
    const qreal currentDpr = qApp ? qApp->devicePixelRatio() : 1.0;
    bool alreadyIncluded = false;
    for (qreal ratio : ratios) {
        if (qAbs(ratio - currentDpr) < 0.001) {
            alreadyIncluded = true;
            break;
        }
    }
    if (!alreadyIncluded) {
        ratios.append(currentDpr);
        std::sort(ratios.begin(), ratios.end());
    }
    return ratios;
}
//=============================================================================
QString
desktopAccessibleActionText(const QAction* action)
{
    const QString text = action ? action->text() : QString();
    QString accessibleText;
    accessibleText.reserve(text.size());
    for (int index = 0; index < text.size(); ++index) {
        if (text.at(index) == QLatin1Char('&')) {
            if (index + 1 < text.size() && text.at(index + 1) == QLatin1Char('&')) {
                accessibleText += QLatin1Char('&');
                ++index;
            }
            continue;
        }
        accessibleText += text.at(index);
    }
    return accessibleText.trimmed();
}
//=============================================================================
QKeySequence
desktopCommandShortcut(int key, bool shift)
{
    int modifiers = 0;
#ifdef Q_OS_MACOS
    modifiers |= Qt::META;
#else
    modifiers |= Qt::CTRL;
#endif
    if (shift) {
        modifiers |= Qt::SHIFT;
    }
    return QKeySequence(modifiers | key);
}
//=============================================================================
void
desktopClearExclusiveActionSelection(QActionGroup* group, const QList<QAction*>& actions)
{
    if (!group) {
        return;
    }
    const bool wasExclusive = group->isExclusive();
    if (wasExclusive) {
        group->setExclusive(false);
    }
    for (QAction* action : actions) {
        if (action && action->isCheckable()) {
            action->setChecked(false);
        }
    }
    group->setExclusive(wasExclusive);
}
//=============================================================================
QIcon
desktopTintedIcon(const QString& filename, const QPalette& palette)
{
    if (!QFileInfo::exists(filename)) {
        return QIcon();
    }
    const QList<qreal> devicePixelRatios = desktopIconDevicePixelRatios();
    const QColor activeTint = desktopUiColors(palette).accent;
    QString ratioKey;
    for (qreal dpr : devicePixelRatios) {
        ratioKey += QString("|%1").arg(QString::number(dpr, 'f', 3));
    }
    const QString cacheKey = filename
        + QString("|%1|%2|%3|%4")
              .arg(ratioKey)
              .arg(colorName(palette.color(QPalette::ButtonText)))
              .arg(colorName(palette.color(QPalette::Disabled, QPalette::ButtonText)))
              .arg(colorName(activeTint))
        + QString("|%1").arg(colorName(palette.color(QPalette::HighlightedText)));
    static QHash<QString, QIcon> cache;
    const auto cached = cache.constFind(cacheKey);
    if (cached != cache.cend()) {
        return cached.value();
    }

    const QIcon source(filename);
    QIcon result;
    const QList<QSize> sizes = { QSize(16, 16), QSize(20, 20), QSize(24, 24), QSize(32, 32) };
    for (const QSize& size : sizes) {
        for (qreal dpr : devicePixelRatios) {
            result.addPixmap(tintedPixmap(source, palette.color(QPalette::ButtonText), size, dpr),
                QIcon::Normal, QIcon::Off);
            result.addPixmap(
                tintedPixmap(
                    source, palette.color(QPalette::Disabled, QPalette::ButtonText), size, dpr),
                QIcon::Disabled, QIcon::Off);
            result.addPixmap(
                tintedPixmap(source, activeTint, size, dpr), QIcon::Active, QIcon::Off);
            result.addPixmap(
                tintedPixmap(source, palette.color(QPalette::HighlightedText), size, dpr),
                QIcon::Selected, QIcon::Off);
        }
    }
    cache.insert(cacheKey, result);
    return result;
}
//=============================================================================
void
applyDesktopToolbarMetrics(QToolBar* toolbar)
{
    if (!toolbar) {
        return;
    }
    toolbar->setMinimumHeight(38);
    for (QToolButton* button : toolbar->findChildren<QToolButton*>()) {
        button->setMinimumSize(QSize(32, 32));
        QAction* action = button->defaultAction();
        if (action) {
            button->setAccessibleName(desktopAccessibleActionText(action));
        }
        if (action && button->toolTip().isEmpty()) {
            button->setToolTip(action->toolTip());
        }
    }
}
//=============================================================================
void
applyDesktopWindowChrome(QMainWindow* window, QToolBar* toolbar)
{
    if (!window) {
        return;
    }
#ifdef Q_OS_MACOS
    window->setWindowTitle(QString());
    window->setUnifiedTitleAndToolBarOnMac(true);
#if QT_VERSION >= QT_VERSION_CHECK(6, 9, 0)
    window->setWindowFlags(
        window->windowFlags() | Qt::ExpandedClientAreaHint | Qt::NoTitleBarBackgroundHint);
    window->setAttribute(Qt::WA_ContentsMarginsRespectsSafeArea, false);
    if (toolbar
        && !toolbar->findChild<QWidget*>(
            QStringLiteral("NelsonMacTitleBarSpacer"), Qt::FindDirectChildrenOnly)) {
        QWidget* spacer = new QWidget(toolbar);
        spacer->setObjectName(QStringLiteral("NelsonMacTitleBarSpacer"));
        spacer->setFixedWidth(72);
        spacer->setFocusPolicy(Qt::NoFocus);
        QAction* before = toolbar->actions().isEmpty() ? nullptr : toolbar->actions().first();
        toolbar->insertWidget(before, spacer);
    }
#else
    Q_UNUSED(toolbar);
#endif
#else
    Q_UNUSED(toolbar);
#endif
}
//=============================================================================
