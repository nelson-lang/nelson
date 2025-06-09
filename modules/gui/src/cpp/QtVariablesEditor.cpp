//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QApplication>
#include <QtWidgets/QInputDialog>
#include <QtGui/QClipboard>
#include <QtCore/QSettings>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QScrollBar>

#include "ForceWindowsTitleBarToDark.hpp"
#include "QtTranslation.hpp"
#include "QtVariablesEditor.h"
#include "WorkspaceBrowser.hpp"
#include "VariableDoubleTableModel.h"
#include "VariableStringTableModel.h"
#include "VariableDefaultModel.h"
#include "QtVariableTextEdit.h"
#include "IsValidVariableName.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
//=============================================================================
const QString SETTING_VARIABLES_EDITOR_GEOMETRY = "ve_Geometry";
//=============================================================================
QtVariablesEditor::QtVariablesEditor(QWidget* parent)
    : m_evaluator(nullptr), QDockWidget(TR("Variables Editor"), parent)
{
    setObjectName("VariablesEditorDockWidget");

    setupUI();
    hide();
}
//=============================================================================
QtVariablesEditor::~QtVariablesEditor() { }
//=============================================================================
void
QtVariablesEditor::setEvaluator(Evaluator* eval)
{
    m_evaluator = eval;
}
//=============================================================================
void
QtVariablesEditor::setupUI()
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif

    // Widget central pour le dock
    QWidget* centralWidget = new QWidget(this);
    QVBoxLayout* mainLayout = new QVBoxLayout(centralWidget);

    // Create toolbar
    m_toolBar = new QToolBar(TR("Variables Editor Toolbar"), centralWidget);
    m_toolBar->setMovable(false);
    m_toolBar->setFloatable(false);
    m_toolBar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

    // Create refresh action
    m_refreshAction = new QAction(TR("Refresh"), this);
    m_refreshAction->setIcon(QIcon::fromTheme("view-refresh"));
    m_refreshAction->setShortcut(QKeySequence::Refresh);
    m_refreshAction->setToolTip(TR("Refresh current variable"));
    connect(m_refreshAction, &QAction::triggered, this, &QtVariablesEditor::refreshCurrentVariable);

    m_createVariableAction = new QAction(TR("Create Variable"), this);
    m_createVariableAction->setToolTip(TR("Create new variable from selected items"));
    m_createVariableAction->setIcon(QIcon::fromTheme("edit-copy"));
    connect(m_createVariableAction, &QAction::triggered, this,
        &QtVariablesEditor::createVariableFromSelection);

    // Add actions to toolbar
    m_toolBar->addAction(m_refreshAction);
    m_toolBar->addAction(m_createVariableAction);

    // Add toolbar to layout
    mainLayout->addWidget(m_toolBar);

    m_tabWidget = new QTabWidget(centralWidget);
    m_tabWidget->setTabsClosable(true);
    m_tabWidget->setMovable(true);
    connect(m_tabWidget, &QTabWidget::tabCloseRequested, this, &QtVariablesEditor::closeTab);

    mainLayout->addWidget(m_tabWidget);

    m_sizeGrip = new QSizeGrip(centralWidget);
    mainLayout->addWidget(m_sizeGrip, 0, Qt::AlignBottom | Qt::AlignRight);

    centralWidget->setLayout(mainLayout);
    this->setWidget(centralWidget);
    hide();

    setFocusPolicy(Qt::StrongFocus);

    // Copy & Paste actions
    m_copyAction = new QAction(TR("Copy"), this);
    m_copyAction->setShortcut(QKeySequence::Copy);
    connect(m_copyAction, &QAction::triggered, this, &QtVariablesEditor::copySelectedCells);

    m_pasteAction = new QAction(TR("Paste"), this);
    m_pasteAction->setShortcut(QKeySequence::Paste);
    connect(m_pasteAction, &QAction::triggered, this, &QtVariablesEditor::pasteDataFromClipboard);

    // Handle docking location changes
    connect(this, &QDockWidget::dockLocationChanged, this,
        &QtVariablesEditor::handleDockLocationChanged);
}
//=============================================================================
void
QtVariablesEditor::restorePosition()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    restoreGeometry(settings.value(SETTING_VARIABLES_EDITOR_GEOMETRY).toByteArray());
}
//=============================================================================
void
QtVariablesEditor::savePosition()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_VARIABLES_EDITOR_GEOMETRY, saveGeometry());
}
//=============================================================================
void
QtVariablesEditor::updateVariables()
{
    if (m_openedVariables.isEmpty()) {
        return;
    }
    if (!m_evaluator) {
        return;
    }

    QStringList variablesToClose;

    QStringList variableNames = m_openedVariables.keys();

    for (const QString& variableName : variableNames) {

        ArrayOf value;
        bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
            wstring_to_utf8(QStringTowstring(variableName)), value);

        if (!found) {
            variablesToClose.append(variableName);
        } else {
            updateTabTitle(variableName, value);
            refreshVariable(variableName);
        }
    }

    for (const QString& varName : variablesToClose) {
        closeVariableTab(varName);
    }
}
//=============================================================================
bool
QtVariablesEditor::openVariable(const QString& variableName)
{
    restorePosition();
    setVisible(true);
    if (m_openedVariables.contains(variableName)) {
        m_tabWidget->setCurrentWidget(m_openedVariables[variableName]);
        return true;
    }

    ArrayOf value;
    bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
        wstring_to_utf8(QStringTowstring(variableName)), value);
    if (!found) {
        return false;
    }

    QWidget* viewWidget = nullptr;

    NelsonType variableType = value.getDataClass();
    if (value.isSparse() || !value.is2D()) {
        variableType = NLS_UNKNOWN;
    }

    QString tabTitle = variableName;
    switch (variableType) {
    case NLS_STRING_ARRAY: {
        // Create table view for double and double complex types
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::MultiSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableStringTableModel* model
            = new VariableStringTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableStringTableModel::modelChanged, tableView, [tableView]() {});

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";

    } break;

    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        // Create table view for double and double complex types
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::MultiSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableDoubleTableModel* model
            = new VariableDoubleTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableDoubleTableModel::modelChanged, tableView, [tableView]() {});

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";

    } break;

    default: {
        QtVariableTextEdit* textEdit = new QtVariableTextEdit(variableName, value, m_evaluator);
        textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        textEdit->setReadOnly(true);
        textEdit->setFont(QFont("Courier", 10));
        createTextEditContextMenu(textEdit);
        viewWidget = textEdit;
        tabTitle += " [" + textEdit->getVariableClassAsString() + " "
            + textEdit->getVariableDimensionsAsString() + "]";
        ;
    } break;
    }

    m_tabWidget->addTab(viewWidget, tabTitle);
    m_tabWidget->setCurrentWidget(viewWidget);
    m_openedVariables.insert(variableName, viewWidget);

    return true;
}
//=============================================================================
void
QtVariablesEditor::closeEvent(QCloseEvent* event)
{
    while (m_tabWidget && m_tabWidget->count() > 0) {
        QWidget* widget = m_tabWidget->widget(0);
        m_tabWidget->removeTab(0);

        QString varName;
        for (auto it = m_openedVariables.begin(); it != m_openedVariables.end(); ++it) {
            if (it.value() == widget) {
                varName = it.key();
                break;
            }
        }

        if (!varName.isEmpty()) {
            m_openedVariables.remove(varName);
        }

        delete widget;
    }
    emit closeVariablesEditor();
    QDockWidget::closeEvent(event);
}
//=============================================================================
QString
QtVariablesEditor::getVariableContentAsString(const ArrayOf& variable)
{
    if (m_evaluator && m_evaluator->getContext()) {
        FunctionDefPtr functionDef = nullptr;
        m_evaluator->getContext()->lookupFunction(L"formattedDisplayText", functionDef);
        if (functionDef) {
            ArrayOfVector args;
            args << variable;
            try {
                std::wostringstream woss;
                Interface* io = m_evaluator->getInterface();
                if (io) {
                    ArrayOfVector results = functionDef->evaluateFunction(m_evaluator, args, 1);
                    return wstringToQString(results[0].getContentAsWideString());
                }
            } catch (Exception&) {
            }
        }
    }
    return QString();
}

void
QtVariablesEditor::updateTabTitle(const QString& variableName, const ArrayOf& value)
{
    if (!m_openedVariables.contains(variableName)) {
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

    // Find the tab index
    int tabIndex = -1;
    for (int i = 0; i < m_tabWidget->count(); ++i) {
        if (m_tabWidget->widget(i) == widget) {
            tabIndex = i;
            break;
        }
    }

    if (tabIndex >= 0) {
        QString newTabTitle = variableName;
        QTableView* tableView = qobject_cast<QTableView*>(m_tabWidget->widget(tabIndex));
        if (tableView) {
            VariableDoubleTableModel* model
                = qobject_cast<VariableDoubleTableModel*>(tableView->model());
            if (model) {
                newTabTitle = newTabTitle + " [" + model->getVariableClassAsString() + " "
                    + model->getVariableDimensionsAsString() + "]";
                m_tabWidget->setTabText(tabIndex, newTabTitle);
            }
        }
        QtVariableTextEdit* textEdit = qobject_cast<QtVariableTextEdit*>(widget);
        if (textEdit) {
            VariableDefaultModel* model = textEdit->model();
            if (model) {
                newTabTitle = newTabTitle + " [" + model->getVariableClassAsString() + " "
                    + model->getVariableDimensionsAsString() + "]";
                m_tabWidget->setTabText(tabIndex, newTabTitle);
            }
        }
    }
}

void
QtVariablesEditor::refreshVariable(const QString& variableName)
{
    if (!m_evaluator || !m_openedVariables.contains(variableName)) {
        return;
    }

    ArrayOf value;
    bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
        wstring_to_utf8(QStringTowstring(variableName)), value);

    if (!found) {
        closeVariableTab(variableName);
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

    bool shouldBeTextEdit = shouldDisplayAsText(value);
    bool shouldBeTableView = shouldDisplayAsTable(value);

    QTextEdit* textEdit = qobject_cast<QTextEdit*>(widget);
    QTableView* tableView = qobject_cast<QTableView*>(widget);

    bool isCurrentlyTextEdit = (textEdit != nullptr);
    bool isCurrentlyTableView = (tableView != nullptr);

    if ((shouldBeTextEdit && !isCurrentlyTextEdit)
        || (shouldBeTableView && !isCurrentlyTableView)) {

        closeVariableTab(variableName);
        if (m_evaluator) {
            openVariable(variableName);
        }
        return;
    }

    if (textEdit && shouldBeTextEdit) {
        QString newContent = getVariableContentAsString(value);

        QTextCursor cursor = textEdit->textCursor();
        int cursorPosition = cursor.position();
        int scrollValue = textEdit->verticalScrollBar()->value();

        textEdit->setPlainText(newContent);

        int newLength = textEdit->toPlainText().length();
        cursor.setPosition(qMin(cursorPosition, newLength));
        textEdit->setTextCursor(cursor);
        textEdit->verticalScrollBar()->setValue(scrollValue);

        updateTabTitle(variableName, value);
        return;
    }

    if (tableView && shouldBeTableView) {

        VariableDoubleTableModel* model
            = qobject_cast<VariableDoubleTableModel*>(tableView->model());
        if (model) {
            if (model->isStructureCompatible(value)) {
                model->refreshFromArray(value);
            } else {
                VariableDoubleTableModel* newModel
                    = new VariableDoubleTableModel(variableName, value, m_evaluator, tableView);

                tableView->setModel(newModel);
                delete model;
            }
        } else {
            VariableDoubleTableModel* newModel
                = new VariableDoubleTableModel(variableName, value, m_evaluator, tableView);

            tableView->setModel(newModel);
        }

        VariableStringTableModel* modelString
            = qobject_cast<VariableStringTableModel*>(tableView->model());
        if (modelString) {
            if (modelString->isStructureCompatible(value)) {
                modelString->refreshFromArray(value);
            } else {
                VariableStringTableModel* newModel
                    = new VariableStringTableModel(variableName, value, m_evaluator, tableView);

                tableView->setModel(newModel);
                delete model;
            }
        } else {
            VariableStringTableModel* newModel
                = new VariableStringTableModel(variableName, value, m_evaluator, tableView);

            tableView->setModel(newModel);
        }

        updateTabTitle(variableName, value);
        return;
    }

    // Fallback: if we can't determine the appropriate widget type or something went wrong,
    // close and reopen the tab
    closeVariableTab(variableName);
    openVariable(variableName);
}

void
QtVariablesEditor::createVariableFromSelection()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        QMessageBox::warning(this, TR("Create Variable"), TR("No table is currently active."));
        return;
    }

    VariableDoubleTableModel* modelDouble
        = qobject_cast<VariableDoubleTableModel*>(currentTableView->model());
    VariableStringTableModel* modelString
        = qobject_cast<VariableStringTableModel*>(currentTableView->model());

    if (!modelDouble && !modelString) {
        QMessageBox::warning(
            this, TR("Create Variable"), TR("Current view does not support variable creation."));
        return;
    }

    QModelIndexList selected = currentTableView->selectionModel()->selectedIndexes();
    if (modelDouble && !modelDouble->isValidSelectionForExtraction(selected)) {
        QMessageBox::information(
            this, TR("Create Variable"), TR("Invalid selection for variable creation."));
        return;
    }
    if (modelString && !modelString->isValidSelectionForExtraction(selected)) {
        QMessageBox::information(
            this, TR("Create Variable"), TR("Invalid selection for variable creation."));
        return;
    }

    // Prompt for new variable name
    QString newVarName
        = QInputDialog::getText(this, TR("New Variable"), TR("Enter new variable name:"));
    if (newVarName.isEmpty() || !IsValidVariableName(QStringTowstring(newVarName))) {
        QMessageBox::critical(this, TR("Error"), TR("Valid variable name expected."));
        return;
    }

    // Create new variable using model
    ArrayOf newVar;
    if (modelDouble) {
        newVar = modelDouble->createArrayFromSelection(selected);
    }
    if (modelString) {
        newVar = modelString->createArrayFromSelection(selected);
    }
    if (newVar.isEmpty()) {
        QMessageBox::critical(this, TR("Error"), TR("Failed to create variable from selection."));
        return;
    }

    // Inject new variable into context
    if (m_evaluator && m_evaluator->getContext()) {
        m_evaluator->getContext()->getBaseScope()->insertVariable(
            wstring_to_utf8(QStringTowstring(newVarName)), newVar);
        openVariable(newVarName);
        WorkspaceBrowser::updateWorkspaceBrowser();
    }
}
//=============================================================================
void
QtVariablesEditor::copySelectedCells()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        return;
    }

    VariableDoubleTableModel* modelDouble
        = qobject_cast<VariableDoubleTableModel*>(currentTableView->model());
    VariableStringTableModel* modelString
        = qobject_cast<VariableStringTableModel*>(currentTableView->model());

    if (!modelDouble && !modelString) {
        return;
    }

    QModelIndexList selectedIndexes = currentTableView->selectionModel()->selectedIndexes();
    QString clipboardText;
    if (modelDouble) {
        clipboardText = modelDouble->getSelectedDataAsText(selectedIndexes);
    }
    if (modelString) {
        clipboardText = modelString->getSelectedDataAsText(selectedIndexes);
    }

    if (!clipboardText.isEmpty()) {
        QApplication::clipboard()->setText(clipboardText);
    }
}
//=============================================================================
void
QtVariablesEditor::pasteDataFromClipboard()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        return;
    }

    VariableDoubleTableModel* model
        = qobject_cast<VariableDoubleTableModel*>(currentTableView->model());
    if (!model) {
        return;
    }

    QModelIndexList selectedIndexes = currentTableView->selectionModel()->selectedIndexes();
    if (selectedIndexes.isEmpty()) {
        QMessageBox::information(this, TR("Paste"),
            TR("Please select the top-left cell where you want to paste the data."));
        return;
    }

    int startRow = selectedIndexes.first().row();
    int startCol = selectedIndexes.first().column();
    QString clipboardText = QApplication::clipboard()->text();

    if (model->pasteData(startRow, startCol, clipboardText)) {
        currentTableView->reset();
        currentTableView->update();

        // Update selection to show pasted area
        QSize pasteSize = model->getPasteSize(clipboardText);
        if (pasteSize.isValid()) {
            QModelIndex topLeft = model->index(startRow, startCol);
            QModelIndex bottomRight
                = model->index(startRow + pasteSize.height() - 1, startCol + pasteSize.width() - 1);
            currentTableView->selectionModel()->select(
                QItemSelection(topLeft, bottomRight), QItemSelectionModel::ClearAndSelect);
        }
    } else {
        QMessageBox::warning(this, TR("Paste Error"),
            TR("Could not paste data. Make sure the data format is compatible."));
    }
}
//=============================================================================
void
QtVariablesEditor::refreshCurrentVariable()
{
    // Get currently active tab
    int currentIndex = m_tabWidget->currentIndex();
    if (currentIndex < 0 || m_openedVariables.isEmpty()) {
        return;
    }

    QWidget* currentWidget = m_tabWidget->currentWidget();
    if (!currentWidget) {
        return;
    }

    // Find the variable name for the current widget
    QString currentVariableName;
    for (auto it = m_openedVariables.begin(); it != m_openedVariables.end(); ++it) {
        if (it.value() == currentWidget) {
            currentVariableName = it.key();
            break;
        }
    }

    if (!currentVariableName.isEmpty()) {
        refreshVariable(currentVariableName);
    }
}
//=============================================================================
void
QtVariablesEditor::closeTab(int index)
{
    QWidget* widget = m_tabWidget->widget(index);
    QString varName;

    for (auto it = m_openedVariables.begin(); it != m_openedVariables.end(); ++it) {
        if (it.value() == widget) {
            varName = it.key();
            break;
        }
    }

    if (!varName.isEmpty()) {
        m_openedVariables.remove(varName);
    }

    m_tabWidget->removeTab(index);
    delete widget;
    if (m_openedVariables.isEmpty()) {
        close();
    }
}
//=============================================================================
void
QtVariablesEditor::handleDockLocationChanged(Qt::DockWidgetArea area)
{
    m_sizeGrip->setVisible(area == Qt::NoDockWidgetArea);
}
//=============================================================================
void
QtVariablesEditor::onModelSelectionCreated(const QString& suggestedName, const ArrayOf& newArray)
{
}
//=============================================================================
void
QtVariablesEditor::onModelError(const QString& message)
{
}
//=============================================================================
void
QtVariablesEditor::closeVariableTab(const QString& variableName)
{
    if (!m_openedVariables.contains(variableName)) {
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

    // Find the tab index
    int tabIndex = -1;
    for (int i = 0; i < m_tabWidget->count(); ++i) {
        if (m_tabWidget->widget(i) == widget) {
            tabIndex = i;
            break;
        }
    }

    if (tabIndex >= 0) {
        m_tabWidget->removeTab(tabIndex);
        m_openedVariables.remove(variableName);
        delete widget;
    }
}
//=============================================================================
bool
QtVariablesEditor::shouldDisplayAsText(const ArrayOf& value)
{
    bool asText = false;
    switch (value.getDataClass()) {
    case NLS_DCOMPLEX:
    case NLS_DOUBLE: {
        asText = false;
    } break;
    default: {
        asText = true;
    } break;
    }
    return asText;
}
//=============================================================================
bool
QtVariablesEditor::shouldDisplayAsTable(const ArrayOf& value)
{
    return !shouldDisplayAsText(value);
}
//=============================================================================
void
QtVariablesEditor::insertRowAbove()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView && tableView->currentIndex().isValid()) {
        model->insertRowAt(tableView->currentIndex().row());
    }
}
//=============================================================================
void
QtVariablesEditor::insertRowBelow()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView->currentIndex().isValid()) {
        model->insertRowAt(tableView->currentIndex().row() + 1);
    }
}
//=============================================================================
void
QtVariablesEditor::insertColumnLeft()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView->currentIndex().isValid()) {
        model->insertColumnAt(tableView->currentIndex().column());
    }
}
//=============================================================================
void
QtVariablesEditor::insertColumnRight()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView->currentIndex().isValid()) {
        model->insertColumnAt(tableView->currentIndex().column() + 1);
    }
}
//=============================================================================
void
QtVariablesEditor::deleteRow()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView->currentIndex().isValid()) {
        model->deleteRowAt(tableView->currentIndex().row());
    }
}
//=============================================================================
void
QtVariablesEditor::deleteColumn()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    auto* model = qobject_cast<VariableDoubleTableModel*>(tableView ? tableView->model() : nullptr);
    if (model && tableView->currentIndex().isValid()) {
        model->deleteColumnAt(tableView->currentIndex().column());
    }
}
//=============================================================================
void
QtVariablesEditor::createTableContextMenu(QTableView* tableView)
{
    tableView->setContextMenuPolicy(Qt::ActionsContextMenu);
    tableView->addAction(m_copyAction);
    tableView->addAction(m_pasteAction);

    m_insertRowAboveAction = new QAction(TR("Insert Row Above"), this);
    m_insertRowBelowAction = new QAction(TR("Insert Row Below"), this);
    m_insertColLeftAction = new QAction(TR("Insert Column Left"), this);
    m_insertColRightAction = new QAction(TR("Insert Column Right"), this);
    m_deleteRowAction = new QAction(TR("Delete Row"), this);
    m_deleteColAction = new QAction(TR("Delete Column"), this);

    connect(m_insertRowAboveAction, &QAction::triggered, this, &QtVariablesEditor::insertRowAbove);
    connect(m_insertRowBelowAction, &QAction::triggered, this, &QtVariablesEditor::insertRowBelow);
    connect(m_insertColLeftAction, &QAction::triggered, this, &QtVariablesEditor::insertColumnLeft);
    connect(
        m_insertColRightAction, &QAction::triggered, this, &QtVariablesEditor::insertColumnRight);
    connect(m_deleteRowAction, &QAction::triggered, this, &QtVariablesEditor::deleteRow);
    connect(m_deleteColAction, &QAction::triggered, this, &QtVariablesEditor::deleteColumn);

    tableView->addAction(m_insertRowAboveAction);
    tableView->addAction(m_insertRowBelowAction);
    tableView->addAction(m_insertColLeftAction);
    tableView->addAction(m_insertColRightAction);
    tableView->addAction(m_deleteRowAction);
    tableView->addAction(m_deleteColAction);
}
//=============================================================================
void
QtVariablesEditor::createTextEditContextMenu(QTextEdit* textEdit)
{
    textEdit->setContextMenuPolicy(Qt::ActionsContextMenu);

    QAction* copyTextAction = new QAction(TR("Copy"), textEdit);
    copyTextAction->setShortcut(QKeySequence::Copy);
    connect(copyTextAction, &QAction::triggered, [textEdit]() {
        if (textEdit->textCursor().hasSelection()) {
            QApplication::clipboard()->setText(textEdit->textCursor().selectedText());
        } else {
            QApplication::clipboard()->setText(textEdit->toPlainText());
        }
    });

    QAction* selectAllAction = new QAction(TR("Select All"), textEdit);
    selectAllAction->setShortcut(QKeySequence::SelectAll);
    connect(selectAllAction, &QAction::triggered, [textEdit]() { textEdit->selectAll(); });

    textEdit->addAction(copyTextAction);
    textEdit->addAction(selectAllAction);
}
//=============================================================================
