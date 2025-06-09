//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtVariablesEditor.h"
#include "QtTranslation.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "characters_encoding.hpp"
#include "VariableTableModel.h"
#include "VariableTypeDefaultTableModel.h"
#include <QtCore/QSettings>
#include <QtWidgets/QTableView>
#include <QtWidgets/QApplication> // For QApplication::clipboard()
#include <QtGui/QClipboard> // For QClipboard
#include <QtWidgets/QMessageBox> // For error messages
#include <QtWidgets/QHeaderView> // For setting resize modes
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QInputDialog>

#include "ClassName.hpp"
#include "IsValidVariableName.hpp"

//=============================================================================
const QString SETTING_VARIABLES_EDITOR_VISIBILITY = "ve_Visibility";
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
    if (QMainWindow* mainWindow = qobject_cast<QMainWindow*>(parentWidget())) {
        QByteArray mainWindowState = settings.value("MainWindow_State").toByteArray();
        if (!mainWindowState.isEmpty()) {
            mainWindow->restoreState(mainWindowState);
            return;
        }
    }
}
//=============================================================================
void
QtVariablesEditor::savePosition()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    if (QMainWindow* mainWindow = qobject_cast<QMainWindow*>(parentWidget())) {
        settings.setValue("MainWindow_State", mainWindow->saveState());
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
QtVariablesEditor::createVariableFromSelection()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        QMessageBox::warning(this, TR("Create Variable"), TR("No table is currently active."));
        return;
    }

    VariableTableModel* model = qobject_cast<VariableTableModel*>(currentTableView->model());
    if (!model) {
        QMessageBox::warning(
            this, TR("Create Variable"), TR("Current view does not support variable creation."));
        return;
    }

    QModelIndexList selected = currentTableView->selectionModel()->selectedIndexes();
    if (selected.isEmpty()) {
        QMessageBox::information(this, TR("Create Variable"), TR("No cells selected."));
        return;
    }

    // Determine bounds of selection
    int minRow = selected.first().row(), maxRow = selected.first().row();
    int minCol = selected.first().column(), maxCol = selected.first().column();
    for (const QModelIndex& index : selected) {
        minRow = qMin(minRow, index.row());
        maxRow = qMax(maxRow, index.row());
        minCol = qMin(minCol, index.column());
        maxCol = qMax(maxCol, index.column());
    }

    // Prompt for new variable name
    QString newVarName
        = QInputDialog::getText(this, TR("New Variable"), TR("Enter new variable name:"));
    if (newVarName.isEmpty() || !IsValidVariableName(QStringTowstring(newVarName))) {
        QMessageBox::critical(this, TR("Error"), TR("Valid variable name expected."));
        return;
    }

    // Create new ArrayOf and fill it with data
    Dimensions newDims(maxRow - minRow + 1, maxCol - minCol + 1);
    ArrayOf newVar;
    if (model->array().getDataClass() == NLS_DOUBLE) {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, newDims.getElementCount());
        newVar = ArrayOf(NLS_DOUBLE, newDims, ptr);

        for (int r = minRow; r <= maxRow; ++r) {
            for (int c = minCol; c <= maxCol; ++c) {
                QModelIndex idx = model->index(r, c);
                QVariant val = model->data(idx, Qt::DisplayRole);
                double num = val.toDouble(); // assumes valid conversion
                ptr[(r - minRow) + (c - minCol) * newDims.getRows()] = num;
            }
        }
    } else if (model->array().getDataClass() == NLS_DCOMPLEX) {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, newDims.getElementCount());
        std::complex<double>* complexPtr = reinterpret_cast<std::complex<double>*>(ptr);
        newVar = ArrayOf(NLS_DCOMPLEX, newDims, ptr);
        for (int r = minRow; r <= maxRow; ++r) {
            for (int c = minCol; c <= maxCol; ++c) {
                double* ptrModel = (double*)model->array().getDataPointer();
                std::complex<double>* complexPtrModel
                    = reinterpret_cast<std::complex<double>*>(ptrModel);
                complexPtr[(r - minRow) + (c - minCol) * newDims.getRows()]
                    = complexPtrModel[r + c * model->array().getDimensions().getRows()];
            }
        }
    }

    // Inject new variable into context
    if (m_evaluator && m_evaluator->getContext()) {
        m_evaluator->getContext()->getBaseScope()->insertVariable(
            wstring_to_utf8(QStringTowstring(newVarName)), newVar);
        openVariable(newVarName);
    }
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

    // Check each opened variable to see if it still exists in context
    QStringList variableNames = m_openedVariables.keys();

    for (const QString& variableName : variableNames) {

        ArrayOf value;
        bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
            wstring_to_utf8(QStringTowstring(variableName)), value);

        if (!found) {
            variablesToClose.append(variableName);
        } else {
            QString name = variableName;
            updateTabTitle(name, value);
            refreshVariable(name);
        }
    }

    // Close tabs for variables that no longer exist
    for (const QString& varName : variablesToClose) {
        closeVariableTab(varName);
    }
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
    QString tabTitle = variableName + " [" + getVariableDimensionsQString(value) + " "
        + getVariableClassAsQString(value) + "]";

    NelsonType variableType = value.getDataClass();
    if (value.isSparse() || !value.is2D()) {
        variableType = NLS_UNKNOWN;
    }

    switch (variableType) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        // Create table view for numeric types
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);
        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
        tableView->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);

        VariableTableModel* model
            = new VariableTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableTableModel::modelChanged, tableView,
            [tableView]() { tableView->resizeColumnsToContents(); });

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
    } break;

    default: {
        // Create text edit for default/other variable types
        QTextEdit* textEdit = new QTextEdit();
        textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        textEdit->setReadOnly(true); // Make it read-only by default
        textEdit->setFont(QFont("Courier", 10)); // Use monospace font for better formatting

        // Convert variable content to string representation
        QString variableContent = getVariableContentAsString(value);
        textEdit->setPlainText(variableContent);

        // Set up context menu for text edit (copy only, since it's read-only)
        createTextEditContextMenu(textEdit);
        viewWidget = textEdit;
    } break;
    }

    m_tabWidget->addTab(viewWidget, tabTitle);
    m_tabWidget->setCurrentWidget(viewWidget);
    m_openedVariables.insert(variableName, viewWidget);

    return true;
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
QtVariablesEditor::closeEvent(QCloseEvent* event)
{
    // Ferme tous les onglets ouverts
    while (m_tabWidget && m_tabWidget->count() > 0) {
        QWidget* widget = m_tabWidget->widget(0);
        m_tabWidget->removeTab(0);

        // Recherche du nom de la variable
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
    savePosition();
    QDockWidget::closeEvent(event);
}
//=============================================================================
void
QtVariablesEditor::createTableContextMenu(QTableView* tableView) //
{
    tableView->setContextMenuPolicy(Qt::ActionsContextMenu);
    tableView->addAction(m_copyAction);
    tableView->addAction(m_pasteAction);
}

//=============================================================================
void
QtVariablesEditor::copySelectedCells() //
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) { //
        return;
    }

    QModelIndexList selectedIndexes = currentTableView->selectionModel()->selectedIndexes();
    if (selectedIndexes.isEmpty()) { //
        return;
    }

    // Determine the bounding rectangle of the selection
    int minRow = selectedIndexes.first().row();
    int maxRow = selectedIndexes.first().row();
    int minCol = selectedIndexes.first().column();
    int maxCol = selectedIndexes.first().column();

    for (const QModelIndex& index : selectedIndexes) { //
        minRow = qMin(minRow, index.row());
        maxRow = qMax(maxRow, index.row());
        minCol = qMin(minCol, index.column());
        maxCol = qMax(maxCol, index.column());
    }

    // Build a string from the selected cells, separated by tabs and newlines
    QString clipboardText;
    for (int r = minRow; r <= maxRow; ++r) { //
        for (int c = minCol; c <= maxCol; ++c) { //
            QModelIndex index = currentTableView->model()->index(r, c);
            clipboardText += currentTableView->model()->data(index, Qt::DisplayRole).toString();
            if (c < maxCol) { //
                clipboardText += "\t";
            }
        }
        if (r < maxRow) { //
            clipboardText += "\n";
        }
    }

    QApplication::clipboard()->setText(clipboardText);
}

//=============================================================================
void
QtVariablesEditor::pasteDataFromClipboard()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        return;
    }

    VariableTableModel* model = qobject_cast<VariableTableModel*>(currentTableView->model());
    if (!model) {
        return;
    }

    QModelIndexList selectedIndexes = currentTableView->selectionModel()->selectedIndexes();
    if (selectedIndexes.isEmpty()) {
        // If no cells are selected, paste at (0,0) or some default.
        // For simplicity, we'll require a selection for now.
        QMessageBox::information(this, TR("Paste"),
            TR("Please select the top-left cell where you want to paste the data."));
        return;
    }

    // Get the top-left-most selected cell as the paste starting point
    int startRow = selectedIndexes.first().row();
    int startCol = selectedIndexes.first().column();

    QString clipboardText = QApplication::clipboard()->text();
    if (clipboardText.isEmpty()) {
        return;
    }

    // Store current selection for restoration
    QItemSelectionModel* selectionModel = currentTableView->selectionModel();
    QItemSelection currentSelection = selectionModel->selection();

    // The VariableTableModel now handles the parsing and application of pasted data
    if (model->pasteData(startRow, startCol, clipboardText)) {
        // Force table view to refresh
        currentTableView->reset();
        currentTableView->update();
        currentTableView->viewport()->update();

        // Optional: Restore or adjust selection to show pasted area
        QSize pasteSize = model->getPasteSize(clipboardText);
        if (pasteSize.isValid()) {
            QModelIndex topLeft = model->index(startRow, startCol);
            QModelIndex bottomRight
                = model->index(startRow + pasteSize.height() - 1, startCol + pasteSize.width() - 1);

            selectionModel->select(
                QItemSelection(topLeft, bottomRight), QItemSelectionModel::ClearAndSelect);
        }

        // Force column resize to fit new content
        currentTableView->resizeColumnsToContents();
        currentTableView->resizeRowsToContents();

    } else {
        QMessageBox::warning(this, TR("Paste Error"),
            TR("Could not paste data. Make sure the data format is compatible and fits within the "
               "variable's capacity."));
    }
}
//=============================================================================
void
QtVariablesEditor::handleDockLocationChanged(Qt::DockWidgetArea area)
{
    m_sizeGrip->setVisible(area == Qt::NoDockWidgetArea);
}
//=============================================================================
QString
QtVariablesEditor::getVariableClassAsQString(const ArrayOf& variable)
{
    std::wstring classname;
    ClassName(variable, classname);
    return wstringToQString(classname);
}
//=============================================================================
QString
QtVariablesEditor::getVariableDimensionsQString(const ArrayOf& variable)
{
    Dimensions dims = variable.getDimensions();
    return wstringToQString(dims.toWideString());
}
//=============================================================================
void
QtVariablesEditor::createTextEditContextMenu(QTextEdit* textEdit)
{
    textEdit->setContextMenuPolicy(Qt::ActionsContextMenu);

    // Create copy action specifically for text edit
    QAction* copyTextAction = new QAction(TR("Copy"), textEdit);
    copyTextAction->setShortcut(QKeySequence::Copy);
    connect(copyTextAction, &QAction::triggered, [textEdit]() {
        if (textEdit->textCursor().hasSelection()) {
            QApplication::clipboard()->setText(textEdit->textCursor().selectedText());
        } else {
            QApplication::clipboard()->setText(textEdit->toPlainText());
        }
    });

    // Create select all action
    QAction* selectAllAction = new QAction(TR("Select All"), textEdit);
    selectAllAction->setShortcut(QKeySequence::SelectAll);
    connect(selectAllAction, &QAction::triggered, [textEdit]() { textEdit->selectAll(); });

    textEdit->addAction(copyTextAction);
    textEdit->addAction(selectAllAction);
}

//=============================================================================
QString
QtVariablesEditor::getVariableContentAsString(const ArrayOf& variable)
{
    if (m_evaluator && m_evaluator->getContext()) {
        // Try to use Nelson's disp function to get string representation
        FunctionDefPtr functionDef = nullptr;
        m_evaluator->getContext()->lookupFunction(L"formattedDisplayText", functionDef);
        if (functionDef) {
            ArrayOfVector args;
            args << variable;
            try {
                // Capture output from disp function
                std::wostringstream woss;
                Interface* io = m_evaluator->getInterface();
                if (io) {
                    ArrayOfVector results = functionDef->evaluateFunction(m_evaluator, args, 1);
                    return wstringToQString(results[0].getContentAsWideString());
                }
            } catch (Exception&) {
                // Fall back to basic representation if disp fails
            }
        }
    }
    QString content;

    // Fallback: create basic string representation
    content = QString("Variable: %1\n").arg(getVariableClassAsQString(variable));
    content += QString("Dimensions: %1\n").arg(getVariableDimensionsQString(variable));
    content += QString("Type: %1\n\n").arg(getVariableClassAsQString(variable));

    // Add basic content preview
    if (variable.isScalar()) {
        try {
            if (variable.isCharacterArray() || variable.isStringArray()) {
                content += wstringToQString(variable.getContentAsWideString());
            } else {
                content += "Scalar value (use table view for detailed inspection)";
            }
        } catch (...) {
            content += "Content cannot be displayed as text";
        }
    } else {
        content += QString("Array with %1 elements\n").arg(variable.getElementCount());
        content += "Use table view for detailed inspection of array contents.";
    }

    return content;
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
        QString newTabTitle = variableName + " [" + getVariableDimensionsQString(value) + " "
            + getVariableClassAsQString(value) + "]";
        m_tabWidget->setTabText(tabIndex, newTabTitle);
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
        // Variable no longer exists, close its tab
        closeVariableTab(variableName);
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

    // Determine what type of widget we should have for the current value
    bool shouldBeTextEdit = shouldDisplayAsText(value);
    bool shouldBeTableView = shouldDisplayAsTable(value);

    // Check if current widget type matches what we need
    QTextEdit* textEdit = qobject_cast<QTextEdit*>(widget);
    QTableView* tableView = qobject_cast<QTableView*>(widget);

    bool isCurrentlyTextEdit = (textEdit != nullptr);
    bool isCurrentlyTableView = (tableView != nullptr);

    // If widget type doesn't match the data type, recreate the tab
    if ((shouldBeTextEdit && !isCurrentlyTextEdit)
        || (shouldBeTableView && !isCurrentlyTableView)) {

        // Close current tab and reopen with correct widget type
        closeVariableTab(variableName);
        if (m_evaluator) {
            openVariable(variableName);
        }
        return;
    }

    // Handle text edit updates
    if (textEdit && shouldBeTextEdit) {
        QString newContent = getVariableContentAsString(value);

        // Store selection and scroll position
        QTextCursor cursor = textEdit->textCursor();
        int cursorPosition = cursor.position();
        int scrollValue = textEdit->verticalScrollBar()->value();

        // Update content
        textEdit->setPlainText(newContent);

        // Restore cursor position and scroll
        int newLength = textEdit->toPlainText().length();
        cursor.setPosition(qMin(cursorPosition, newLength));
        textEdit->setTextCursor(cursor);
        textEdit->verticalScrollBar()->setValue(scrollValue);

        // Update tab title
        updateTabTitle(variableName, value);
        return;
    }

    // Handle table view updates
    if (tableView && shouldBeTableView) {
        // Check if the table structure is compatible
        VariableTableModel* model = qobject_cast<VariableTableModel*>(tableView->model());
        if (model) {
            // Check if dimensions changed significantly
            if (model->isStructureCompatible(value)) {
                // Same structure, just refresh data
                model->refreshFromArray(value);
            } else {
                // Structure changed, need to recreate the model
                VariableTableModel* newModel
                    = new VariableTableModel(variableName, value, m_evaluator, tableView);

                tableView->setModel(newModel);

                // Clean up old model
                delete model;

                // Adjust column widths for new structure
                tableView->resizeColumnsToContents();
            }
        } else {
            // No existing model, create new one
            VariableTableModel* newModel
                = new VariableTableModel(variableName, value, m_evaluator, tableView);

            // VariableTableModel* newModel = new VariableTableModel(value, this);
            tableView->setModel(newModel);
            tableView->resizeColumnsToContents();
        }

        updateTabTitle(variableName, value);
        return;
    }

    // Fallback: if we can't determine the appropriate widget type or something went wrong,
    // close and reopen the tab
    closeVariableTab(variableName);
    openVariable(variableName);
}

//=============================================================================
void
QtVariablesEditor::refreshAllVariables()
{
    // Refresh all currently opened variables
    QStringList variableNames = m_openedVariables.keys();
    for (const QString& varName : variableNames) {
        refreshVariable(varName);
    }
}
//=============================================================================
bool
QtVariablesEditor::shouldDisplayAsText(const ArrayOf& value)
{
    return !value.isDoubleType();
}
//=============================================================================
bool
QtVariablesEditor::shouldDisplayAsTable(const ArrayOf& value)
{
    return value.isDoubleType();
}
//=============================================================================
