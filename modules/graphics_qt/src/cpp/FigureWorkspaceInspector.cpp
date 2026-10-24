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
#include "FigureWorkspaceHelpers.hpp"
#include "FigurePropertyDelegate.hpp"
#include "GOWindow.hpp"
#include "GOPropertyNames.hpp"
#include <QtCore/QSignalBlocker>
#include <QtCore/QJsonDocument>
#include <QtGui/QPalette>
#include <QtWidgets/QApplication>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMenu>
#include <QtWidgets/QScrollArea>
#include <QtWidgets/QStyledItemDelegate>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QTreeWidgetItemIterator>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    void
    appendTreeSignature(const QJsonObject& node, QByteArray& sig)
    {
        sig += QByteArray::number(static_cast<qlonglong>(node["handle"].toDouble()));
        sig += ':';
        sig += node["type"].toString().toUtf8();
        sig += '[';
        for (const auto& child : figureObjectArray(node["children"])) {
            appendTreeSignature(child.toObject(), sig);
            sig += ',';
        }
        sig += ']';
    }
    //=============================================================================
    QByteArray
    treeSignature(const QJsonObject& root)
    {
        QByteArray sig;
        appendTreeSignature(root, sig);
        return sig;
    }
    //=============================================================================
    QTreeWidgetItem*
    buildBrowserNode(QTreeWidget* tree, QTreeWidgetItem* parent, const QJsonObject& node)
    {
        const QString label = node["label"].toString();
        const QString type = node["type"].toString();
        const QString text
            = label.isEmpty() || label == type ? type : QString("%1 (%2)").arg(type, label);
        auto* item = parent ? new QTreeWidgetItem(parent) : new QTreeWidgetItem(tree);
        item->setText(0, text);
        item->setData(0, Qt::UserRole, node["handle"].toDouble());
        for (const auto& child : figureObjectArray(node["children"])) {
            buildBrowserNode(tree, item, child.toObject());
        }
        return item;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
FigureWorkspaceQt::createPropertyEditor(QWidget* parent, QVBoxLayout* layout)
{
    auto* browserLabel = new QLabel(tr("Objects"), parent);
    browserLabel->setObjectName("figureObjectBrowserLabel");
    layout->addWidget(browserLabel);
    browser = new QTreeWidget(parent);
    browser->setObjectName("figureObjectBrowser");
    browser->setHeaderHidden(true);
    browser->setMinimumHeight(100);
    browser->setMaximumHeight(220);
    layout->addWidget(browser);
    connect(browser, &QTreeWidget::currentItemChanged, this,
        [this](QTreeWidgetItem* item, QTreeWidgetItem*) {
            if (updating || !item) {
                return;
            }
            const int64 handle = static_cast<int64>(item->data(0, Qt::UserRole).toDouble());
            if (handle != inspectObject) {
                requestInspectModel(handle);
            }
        });

    propertyFilter = new QLineEdit(parent);
    propertyFilter->setObjectName("figurePropertyFilter");
    propertyFilter->setPlaceholderText(tr("Search properties"));
    propertyFilter->setClearButtonEnabled(true);
    layout->addWidget(propertyFilter);
    connect(propertyFilter, &QLineEdit::textChanged, this,
        [this](const QString& text) { applyPropertyFilter(text); });

    properties = new QTreeWidget(parent);
    properties->setObjectName("figureProperties");
    properties->setItemDelegate(new FigurePropertyDelegate(properties));
    properties->setHeaderLabels({ tr("Property"), tr("Value") });
    properties->header()->setSectionResizeMode(QHeaderView::ResizeToContents);
    properties->setRootIsDecorated(false);
    properties->setAlternatingRowColors(true);
    properties->setMinimumWidth(300);
    layout->addWidget(properties, 1);
    errorLabel = new QLabel(parent);
    errorLabel->setObjectName("figureActionStatus");
    errorLabel->setWordWrap(true);
    layout->addWidget(errorLabel);
    connect(properties, &QTreeWidget::itemChanged, this, [this](QTreeWidgetItem* item, int column) {
        if (!updating && column == 1) {
            commitProperty(item);
        }
    });
}
//=============================================================================
void
FigureWorkspaceQt::createTools(QToolBar* primary, QAction* rotate, QAction* pan)
{
    inspector = new QDockWidget(tr("Figure properties"), window);
    inspector->setObjectName("figurePropertiesDock");
    auto* content = new QWidget(inspector);
    auto* layout = new QVBoxLayout(content);
    createViewControls(layout);
    createPropertyEditor(content, layout);
    inspector->setWidget(content);
    window->addDockWidget(Qt::RightDockWidgetArea, inspector);
    inspector->hide();
    auto* inspectAction = inspector->toggleViewAction();
    inspectAction->setText(tr("Properties"));
    inspectAction->setObjectName("figurePropertiesAction");
    createEnhancements(primary);
    connect(inspector, &QDockWidget::visibilityChanged, this, [this](bool visible) {
        if (visible) {
            renderProperties();
            refresh();
        }
    });
    axesSelector = new QComboBox(primary);
    axesSelector->setObjectName("figureAxesSelector");
    axesSelector->setAccessibleName(tr("Selected subplot"));
    axesSelector->setMinimumContentsLength(10);
    axesSelector->setMaximumWidth(220);
    axesSelector->installEventFilter(this);
    // The web backend has no axes picker (it targets the axes under the pointer).
    // Keep the combo (it is still populated and drives selectAxes) but hide it so
    // it is not shown on the toolbar; a child widget left out of the toolbar
    // layout would otherwise float at (0,0) and overlap the other buttons.
    axesSelector->hide();
    connect(axesSelector, &QComboBox::activated, this, [this](int index) {
        release();
        selectAxes(axesSelector->itemData(index).toLongLong());
        showIndicator(selectedAxes());
    });
    navigation = window->addToolBar(tr("Figure navigation"));
    navigation->setObjectName("figureNavigationToolbar");
    navigation->setMovable(false);
    navigation->setIconSize(primary->iconSize());
    // Unified mode group: Inspect (modeless default), Zoom, Pan, Rotate.
    auto* inspect
        = navigation->addAction(window->sharedIcon(L"/resources/crosshair.svg"), tr("Inspect"));
    inspect->setObjectName("figureInspectAction");
    inspect->setToolTip(
        tr("Inspect: hover or click to read a data point; drag to pan (2D) or rotate (3D)"));
    connect(inspect, &QAction::triggered, this, [this]() {
        release();
        window->mouseMode = MOUSE_MODE::NONE;
        window->rotateAction->setChecked(false);
        window->panAction->setChecked(false);
        window->zoomInAction->setChecked(false);
        window->zoomOutAction->setChecked(false);
        window->applyFigurePointerCursor();
        updatePointMarker();
        updateDetails();
    });
    window->zoomInAction->setText(tr("Zoom"));
    window->zoomInAction->setToolTip(
        tr("Zoom (Z): scroll, or drag a box to zoom in; click to zoom in"));
    navigation->addAction(window->zoomInAction);
    navigation->addAction(pan);
    navigation->addAction(rotate);
    pan->setToolTip(
        tr("Pan (P): drag to move the view inside the axes; keep the subplot in place"));
    rotate->setToolTip(tr("Rotate (R): change the 3D orientation of the selected subplot"));
    const auto updateMode = [this]() {
        updatePointMarker();
        updateDetails();
    };
    connect(rotate, &QAction::triggered, this, updateMode);
    connect(pan, &QAction::triggered, this, updateMode);
    connect(window->zoomInAction, &QAction::triggered, this, updateMode);
    navigation->addSeparator();
    auto* fit = navigation->addAction(window->sharedIcon(L"/resources/view-zoom-0.svg"), tr("Fit"));
    fit->setObjectName("figurefitAction");
    fit->setToolTip(
        tr("Fit (F): fit data and keep orientation. Double-click a subplot to fit it."));
    connect(fit, &QAction::triggered, this, [this]() { action({ { "action", "fit" } }); });
    auto* restore = navigation->addAction(
        window->sharedIcon(L"/resources/action-home.svg"), tr("Restore view"));
    restore->setObjectName("figurerestoreAction");
    restore->setToolTip(
        tr("Restore the saved orientation, camera and limits of the selected subplot"));
    connect(restore, &QAction::triggered, this, [this]() { action({ { "action", "restore" } }); });
    auto* presets = new QComboBox(navigation);
    presets->setAccessibleName(tr("View preset"));
    presets->addItems(
        { tr("View…"), tr("Default 3D"), tr("Top · XY"), tr("Front · XZ"), tr("Side · YZ") });
    navigation->addWidget(presets);
    connect(presets, &QComboBox::activated, this, [this, presets](int index) {
        const QJsonArray views[] = { {}, { -37.5, 30 }, { 0, 90 }, { 0, 0 }, { 90, 0 } };
        if (index > 0 && index < 5) {
            action({ { "action", "view" }, { "azimuth", views[index][0] },
                { "elevation", views[index][1] }, { "resetCamera", true } });
        }
        presets->setCurrentIndex(0);
    });
    // Right-hand cluster: undo, redo, Properties, More, Export.
    navigation->addSeparator();
    auto* undo
        = navigation->addAction(window->sharedIcon(L"/resources/action-undo.svg"), tr("Undo"));
    undo->setObjectName("figureUndoAction");
    undo->setToolTip(tr("Undo (Ctrl+Z)"));
    connect(undo, &QAction::triggered, this, [this]() { action({ { "action", "undo" } }); });
    auto* redo
        = navigation->addAction(window->sharedIcon(L"/resources/action-redo.svg"), tr("Redo"));
    redo->setObjectName("figureRedoAction");
    redo->setToolTip(tr("Redo (Ctrl+Shift+Z)"));
    connect(redo, &QAction::triggered, this, [this]() { action({ { "action", "redo" } }); });
    navigation->addAction(inspectAction);
    createLegend();
    details = new QLabel(tr("Select a data point to inspect its coordinates"), window);
    details->setObjectName("figureDetails");
    window->statusBar()->addWidget(details, 1);
    window->statusBar()->hide();
    // The "Navigation toolbar" and "Plot details bar" toggles are intentionally
    // kept out of the More menu: More lives on the navigation toolbar itself, so
    // hiding that toolbar would leave no way to bring it back.
    auto* showNavigation = new QAction(this);
    showNavigation->setObjectName("figureShowNavigationAction");
    showNavigation->setCheckable(true);
    auto* showDetails = new QAction(this);
    showDetails->setObjectName("figureShowDetailsAction");
    showDetails->setCheckable(true);
    auto* more = new QToolButton(primary);
    more->setObjectName("figureMoreActions");
    more->setText(tr("More"));
    more->setPopupMode(QToolButton::InstantPopup);
    auto* menu = new QMenu(more);
    auto* zoomIn = menu->addAction(window->sharedIcon(L"/resources/zoom-in.svg"), tr("Zoom in"));
    zoomIn->setObjectName("figurezoomInAction");
    connect(zoomIn, &QAction::triggered, this,
        [this]() { action({ { "action", "zoom" }, { "factor", 1.25 } }); });
    auto* zoomOut = menu->addAction(window->sharedIcon(L"/resources/zoom-out.svg"), tr("Zoom out"));
    zoomOut->setObjectName("figurezoomOutAction");
    connect(zoomOut, &QAction::triggered, this,
        [this]() { action({ { "action", "zoom" }, { "factor", 0.8 } }); });
    menu->addSeparator();
    auto* save = menu->addAction(tr("Save as…"));
    save->setObjectName("figureSaveAsAction");
    save->setToolTip(tr("Save the figure image to a local PNG, SVG or PDF file"));
    connect(save, &QAction::triggered, this, &FigureWorkspaceQt::saveAs);
    menu->addAction(window->printAction);
    menu->addAction(window->restoreViewAction);
    auto* floor = menu->addAction(tr("Projection floor"));
    floor->setObjectName("figureFloorAction");
    floor->setCheckable(true);
    connect(floor, &QAction::toggled, this,
        [this](bool enabled) { action({ { "action", "floor" }, { "enabled", enabled } }); });
    auto* reference = menu->addAction(
        window->sharedIcon(L"/resources/save-workspace.svg"), tr("Use current view as reference"));
    reference->setObjectName("figurereferenceAction");
    connect(
        reference, &QAction::triggered, this, [this]() { action({ { "action", "reference" } }); });
    more->setMenu(menu);
    navigation->addWidget(more);
    navigation->addAction(window->exportAction);
    navigation->hide();
}
//=============================================================================
void
FigureWorkspaceQt::syncVisibility()
{
    const bool allowed
        = window && window->goFig->findStringProperty(GO_TOOL_BAR_PROPERTY_NAME_STR) != L"none";
    navigation->setVisible(allowed && navigationRequested);
    window->statusBar()->setVisible(allowed && detailsRequested);
    legend->setVisible(allowed && detailsRequested && !legend->actions().empty());
    updatePointMarker();
    updateDetails();
}
//=============================================================================
void
FigureWorkspaceQt::updateDetails()
{
    if (!details || !window) {
        return;
    }
    QString mode = tr("Inspect");
    switch (window->mouseMode) {
    case MOUSE_MODE::ROTATION:
        mode = tr("Rotate");
        break;
    case MOUSE_MODE::PAN:
        mode = tr("Pan");
        break;
    case MOUSE_MODE::ZOOM_IN:
        mode = tr("Zoom in");
        break;
    case MOUSE_MODE::ZOOM_OUT:
        mode = tr("Zoom out");
        break;
    default:
        break;
    }
    const auto position = inspectedPoint["position"].toArray();
    if (window->mouseMode == MOUSE_MODE::NONE && inspectedPoint["found"].toBool()
        && inspectedPoint["axesHandle"].toDouble() == selectedAxes() && position.size() == 3) {
        details->setText(tr("%1 · Point %2 · X %3 · Y %4 · Z %5")
                .arg(mode)
                .arg(inspectedPoint["index"].toInt())
                .arg(QString::number(position[0].toDouble(), 'g', 6))
                .arg(QString::number(position[1].toDouble(), 'g', 6))
                .arg(QString::number(position[2].toDouble(), 'g', 6)));
        return;
    }
    const auto view = axesState()["View"].toArray();
    details->setText(view.size() == 2 ? tr("%1 · Az %2° · El %3°")
                                            .arg(mode)
                                            .arg(QString::number(view[0].toDouble(), 'f', 1))
                                            .arg(QString::number(view[1].toDouble(), 'f', 1))
                                      : mode);
}
//=============================================================================
int64
FigureWorkspaceQt::currentInspectObject() const
{
    // Figure handles are 0-based, so inspectObject < 0 means "not chosen yet".
    if (inspectObject >= 0) {
        return inspectObject;
    }
    return window ? window->getHandle() : -1;
}
//=============================================================================
void
FigureWorkspaceQt::requestInspectModel(int64 handle)
{
    if (handle < 0) {
        return;
    }
    inspectObject = handle;
    action({ { "action", "inspect" }, { "objectHandle", static_cast<double>(handle) } });
}
//=============================================================================
void
FigureWorkspaceQt::renderProperties()
{
    if (!properties || !inspector || !inspector->isVisible()) {
        return;
    }
    if (inspectObject < 0 || inspectorModel.isEmpty()) {
        requestInspectModel(currentInspectObject());
    }
}
//=============================================================================
void
FigureWorkspaceQt::renderObjectModel()
{
    if (!properties) {
        return;
    }
    auto* focused = QApplication::focusWidget();
    if (focused && properties->isAncestorOf(focused)) {
        propertiesDirty = true;
        return;
    }
    propertiesDirty = false;
    const QSignalBlocker blocker(properties);
    updating = true;
    properties->clear();
    const QBrush disabled = properties->palette().brush(QPalette::Disabled, QPalette::Text);
    for (const auto& value : inspectorModel["properties"].toArray()) {
        const auto entry = value.toObject();
        const QString name = entry["name"].toString();
        const QString display = entry["display"].toString();
        auto* item = new QTreeWidgetItem(properties, { name, display });
        item->setToolTip(0, name);
        item->setToolTip(1, display);
        const bool writable = entry["writable"].toBool();
        const QString kind = entry["kind"].toString();
        if (writable && kind != "info") {
            item->setFlags(item->flags() | Qt::ItemIsEditable);
        } else {
            item->setFlags(item->flags() & ~Qt::ItemIsEditable);
            item->setForeground(1, disabled);
        }
        QStringList choices;
        for (const auto& choice : entry["choices"].toArray()) {
            choices << choice.toString();
        }
        item->setData(0, Qt::UserRole, name);
        item->setData(1, RolePropertyKind, kind);
        item->setData(1, RolePropertyChoices, choices);
        item->setData(1, RolePropertyValue, QVariant::fromValue(entry["value"]));
        item->setData(1, RolePropertyWritable, writable);
        item->setData(1, RolePropertyDisplay, display);
    }
    updating = false;
    applyPropertyFilter(propertyFilter ? propertyFilter->text() : QString());
}
//=============================================================================
void
FigureWorkspaceQt::syncObjectValues(const QJsonObject& values)
{
    if (!properties || values.isEmpty()) {
        return;
    }
    auto* focused = QApplication::focusWidget();
    if (focused && properties->isAncestorOf(focused)) {
        propertiesDirty = true;
        return;
    }
    const QSignalBlocker blocker(properties);
    updating = true;
    for (int index = 0; index < properties->topLevelItemCount(); ++index) {
        auto* item = properties->topLevelItem(index);
        const QString name = item->data(0, Qt::UserRole).toString();
        const auto found = values.constFind(name);
        if (found == values.constEnd()) {
            continue;
        }
        const QString display = found->toString();
        if (item->data(1, RolePropertyDisplay).toString() != display) {
            item->setText(1, display);
            item->setToolTip(1, display);
            item->setData(1, RolePropertyDisplay, display);
        }
    }
    updating = false;
}
//=============================================================================
void
FigureWorkspaceQt::populateBrowser(const QJsonObject& root)
{
    if (!browser) {
        return;
    }
    const QByteArray signature = treeSignature(root);
    const QSignalBlocker blocker(browser);
    updating = true;
    if (signature != browserSignature) {
        browserSignature = signature;
        browser->clear();
        buildBrowserNode(browser, nullptr, root);
        browser->expandAll();
    }
    for (QTreeWidgetItemIterator iterator(browser); *iterator; ++iterator) {
        auto* node = *iterator;
        if (static_cast<int64>(node->data(0, Qt::UserRole).toDouble()) == inspectObject) {
            browser->setCurrentItem(node);
            if (node->parent()) {
                node->parent()->setExpanded(true);
            }
            break;
        }
    }
    updating = false;
}
//=============================================================================
void
FigureWorkspaceQt::applyPropertyFilter(const QString& text)
{
    if (!properties) {
        return;
    }
    const QString needle = text.trimmed();
    for (int index = 0; index < properties->topLevelItemCount(); ++index) {
        auto* item = properties->topLevelItem(index);
        const bool match = needle.isEmpty() || item->text(0).contains(needle, Qt::CaseInsensitive)
            || item->text(1).contains(needle, Qt::CaseInsensitive);
        item->setHidden(!match);
    }
}
//=============================================================================
void
FigureWorkspaceQt::commitProperty(QTreeWidgetItem* item)
{
    const QString name = item->data(0, Qt::UserRole).toString();
    if (name.isEmpty()) {
        return;
    }
    const QString kind = item->data(1, RolePropertyKind).toString();
    const QJsonValue previous = item->data(1, RolePropertyValue).value<QJsonValue>();
    const QString text = item->text(1);
    QJsonValue value;
    bool valid = true;
    if (kind == "bool") {
        value = text.compare("true", Qt::CaseInsensitive) == 0 || text == "1"
            || text.compare("on", Qt::CaseInsensitive) == 0;
    } else if (kind == "enum" || kind == "text") {
        value = text;
    } else {
        value = parseFigureProperty(text, previous, valid, kind == "color");
    }
    if (!valid) {
        errorLabel->setText(tr("Enter a valid value for this property."));
        const QSignalBlocker blocker(properties);
        item->setText(1, item->data(1, RolePropertyDisplay).toString());
        return;
    }
    errorLabel->clear();
    QJsonObject request { { "action", "properties" },
        { "objectHandle", static_cast<double>(inspectObject) },
        { "inspectHandle", static_cast<double>(inspectObject) },
        { "properties", QJsonObject { { name, value } } } };
    if (selectedAxes() > 0) {
        request["axesHandle"] = static_cast<double>(selectedAxes());
    }
    action(request);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
