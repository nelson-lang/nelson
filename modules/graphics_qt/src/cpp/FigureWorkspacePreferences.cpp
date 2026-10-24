//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigureWorkspaceQt.hpp"
#include "GOWindow.hpp"
#include "NelsonSettingsScope.hpp"
#include <QtCore/QSettings>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QLayout>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    std::unique_ptr<QSettings>
    workspaceSettings()
    {
        const auto path = qEnvironmentVariable("NELSON_QT_FIGURE_SETTINGS");
        return path.isEmpty()
            ? std::make_unique<QSettings>(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION)
            : std::make_unique<QSettings>(path, QSettings::IniFormat);
    }
    //=============================================================================
} // namespace
//=============================================================================
void
FigureWorkspaceQt::createEnhancements(QToolBar* primary)
{
    for (const auto& kind : { QString("undo"), QString("redo") }) {
        auto* control = primary->addAction(kind == "undo" ? tr("Undo") : tr("Redo"));
        control->setObjectName("figure" + kind + "Action");
        control->setIcon(window->sharedIcon(
            kind == "undo" ? L"/resources/action-undo.svg" : L"/resources/action-redo.svg"));
        control->setToolTip(kind == "undo" ? tr("Undo figure edit (Ctrl/Cmd+Z)")
                                           : tr("Redo figure edit (Ctrl/Cmd+Shift+Z)"));
        control->setEnabled(false);
        connect(
            control, &QAction::triggered, this, [this, kind]() { action({ { "action", kind } }); });
    }
    preferencesTimer.setParent(this);
    preferencesTimer.setSingleShot(true);
    preferencesTimer.setInterval(300);
    connect(&preferencesTimer, &QTimer::timeout, this, &FigureWorkspaceQt::savePreferences);
    inspector->installEventFilter(this);
    connect(inspector, &QDockWidget::visibilityChanged, this, [this]() { schedulePreferences(); });
    QTimer::singleShot(0, this, &FigureWorkspaceQt::restorePreferences);
}
//=============================================================================
void
FigureWorkspaceQt::restorePreferences()
{
    auto settings = workspaceSettings();
    navigationRequested = settings->value("FigureWorkspace/navigation", true).toBool();
    detailsRequested = settings->value("FigureWorkspace/details", false).toBool();
    preferredInspectorWidth
        = std::clamp(settings->value("FigureWorkspace/width", 320).toInt(), 220, 1000);
    // Inspector visibility belongs to one figure window. Do not restore the
    // visibility saved by another figure; every newly created figure starts
    // with its inspector closed.
    inspector->setVisible(false);
    QTimer::singleShot(0, this, &FigureWorkspaceQt::restoreInspectorWidth);
    findChild<QAction*>("figureShowNavigationAction")->setChecked(navigationRequested);
    findChild<QAction*>("figureShowDetailsAction")->setChecked(detailsRequested);
    preferencesReady = true;
    syncVisibility();
}
//=============================================================================
void
FigureWorkspaceQt::restoreInspectorWidth()
{
    if (preferencesReady && window && inspector->isVisible() && canvas && canvas->isVisible()) {
        window->layout()->activate();
        window->resizeDocks({ inspector }, { preferredInspectorWidth }, Qt::Horizontal);
    }
}
//=============================================================================
void
FigureWorkspaceQt::schedulePreferences()
{
    if (preferencesReady) {
        preferencesTimer.start();
    }
}
//=============================================================================
void
FigureWorkspaceQt::savePreferences()
{
    if (!preferencesReady || !window) {
        return;
    }
    auto settings = workspaceSettings();
    // Inspector visibility is intentionally not persisted across figures.
    settings->setValue("FigureWorkspace/properties", false);
    settings->setValue("FigureWorkspace/navigation", navigationRequested);
    settings->setValue("FigureWorkspace/details", detailsRequested);
    preferredInspectorWidth = inspector->width();
    settings->setValue("FigureWorkspace/width", preferredInspectorWidth);
    settings->sync();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
