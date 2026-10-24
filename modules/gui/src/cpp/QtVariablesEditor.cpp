//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include <QtCore/QMimeData>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QScrollBar>
#include "ForceWindowsTitleBarToDark.hpp"
#include "QtTranslation.hpp"
#include "QtVariablesEditor.h"
#include "QtWaitCursor.hpp"
#include "QtDockTitleBar.h"
#include "WorkspaceBrowser.hpp"
#include "VariableDoubleTableModel.h"
#include "VariableSingleTableModel.h"
#include "VariableIntegerTableModel.h"
#include "VariableStringTableModel.h"
#include "VariableLogicalTableModel.h"
#include "VariableRowCharactersTableModel.h"
#include "VariableCellTableModel.h"
#include "VariableTableTableModel.h"
#include "VariableStructTableModel.h"
#include "VariableDefaultModel.h"
#include "QtVariableTextEdit.h"
#include "IsValidVariableName.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "EvaluateCommand.hpp"
#include "KeyHash.hpp"
#include "DefaultFont.hpp"
#include "QStringConverter.hpp"
#include "ErrorID.hpp"
//=============================================================================
const QString SETTING_VARIABLES_EDITOR_GEOMETRY = "ve_Geometry";
//=============================================================================
// Suffix appended to the tab title of variables which cannot be edited.
static QString
readOnlyTabSuffix()
{
    return QString(" ") + TR("(read-only)");
}
//=============================================================================
// Monospaced font used by the read-only text views (module default font).
static QFont
defaultReadOnlyFont()
{
    std::wstring fontName = Nelson::getDefaultFontName();
    if (fontName.empty()) {
        fontName = L"Monospace";
    }
    QFont font(wstringToQString(fontName));
    font.setStyleHint(QFont::Monospace);
    font.setFixedPitch(true);
    return font;
}
//=============================================================================
QtVariablesEditor::QtVariablesEditor(QWidget* parent)
    : m_evaluator(nullptr), QDockWidget(TR("Variables Editor"), parent)
{
    setObjectName("VariablesEditorDockWidget");

    setupUI();
    setTitleBarWidget(new QtDockTitleBar(this, "ve"));
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

    QWidget* centralWidget = new QWidget(this);
    QVBoxLayout* mainLayout = new QVBoxLayout(centralWidget);

    m_toolBar = new QToolBar(TR("Variables Editor Toolbar"), centralWidget);
    m_toolBar->setIconSize(QSize(18, 18));
    m_toolBar->setMovable(false);
    m_toolBar->setFloatable(false);
    m_toolBar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

    QString refreshIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/refresh.svg");
    m_refreshAction = new QAction(TR("Refresh"), this);
    m_refreshAction->setIcon(QIcon(refreshIcon));
    m_refreshAction->setShortcut(QKeySequence::Refresh);
    m_refreshAction->setToolTip(TR("Refresh current variable"));
    connect(m_refreshAction, &QAction::triggered, this, &QtVariablesEditor::refreshCurrentVariable);

    QString createVariableIcon
        = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/create-variable.svg");
    m_createVariableAction = new QAction(TR("Create Variable"), this);
    m_createVariableAction->setToolTip(TR("Create new variable from selected items"));
    m_createVariableAction->setIcon(QIcon(createVariableIcon));
    connect(m_createVariableAction, &QAction::triggered, this,
        &QtVariablesEditor::createVariableFromSelection);

    m_replaceByEmptyAction = new QAction(TR("Replace by Empty"), this);
    m_replaceByEmptyAction->setShortcut(QKeySequence::Delete);
    m_replaceByEmptyAction->setToolTip(TR("Replace selected items with missing values"));
    connect(m_replaceByEmptyAction, &QAction::triggered, this, &QtVariablesEditor::replaceByEmpty);

    m_copyAction = new QAction(TR("Copy"), this);
    m_copyAction->setShortcut(QKeySequence::Copy);
    m_copyAction->setToolTip(TR("Copy selected cells"));
    connect(m_copyAction, &QAction::triggered, this, &QtVariablesEditor::copySelectedCells);

    m_pasteAction = new QAction(TR("Paste"), this);
    m_pasteAction->setShortcut(QKeySequence::Paste);
    m_pasteAction->setToolTip(TR("Paste clipboard content"));
    connect(m_pasteAction, &QAction::triggered, this, &QtVariablesEditor::pasteDataFromClipboard);

    m_pasteExcelDataAction = new QAction(TR("Paste Excel Data"), this);
    m_pasteExcelDataAction->setShortcut(QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_V));
    m_pasteExcelDataAction->setToolTip(
        TR("Paste data from Excel with enhanced formatting support"));
    connect(m_pasteExcelDataAction, &QAction::triggered, this,
        &QtVariablesEditor::pasteExcelDataFromClipboard);

    m_undoAction = new QAction(TR("Undo"), this);
    m_undoAction->setShortcut(QKeySequence::Undo);
    m_undoAction->setToolTip(TR("Undo last change"));
    connect(m_undoAction, &QAction::triggered, this, &QtVariablesEditor::undo);

    m_redoAction = new QAction(TR("Redo"), this);
    m_redoAction->setShortcut(QKeySequence::Redo);
    m_redoAction->setToolTip(TR("Redo last undone change"));
    connect(m_redoAction, &QAction::triggered, this, &QtVariablesEditor::redo);

    // Table edition actions shared by every tab (created once to avoid leaking
    // a new set of QAction for each opened variable).
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

    m_toolBar->addAction(m_refreshAction);
    m_toolBar->addAction(m_createVariableAction);
    m_toolBar->addSeparator();
    m_toolBar->addAction(m_copyAction);
    m_toolBar->addAction(m_pasteAction);
    m_toolBar->addSeparator();
    m_toolBar->addAction(m_undoAction);
    m_toolBar->addAction(m_redoAction);

    mainLayout->addWidget(m_toolBar);

    m_tabWidget = new QTabWidget(centralWidget);
    m_tabWidget->setTabsClosable(true);
    m_tabWidget->setMovable(true);
    connect(m_tabWidget, &QTabWidget::tabCloseRequested, this, &QtVariablesEditor::closeTab);
    connect(
        m_tabWidget, &QTabWidget::currentChanged, this, [this](int) { this->updateEditActions(); });

    mainLayout->addWidget(m_tabWidget);

    m_statusBar = new QStatusBar(centralWidget);
    m_statusBar->setSizeGripEnabled(false);
    mainLayout->addWidget(m_statusBar);

    centralWidget->setLayout(mainLayout);
    this->setWidget(centralWidget);
    hide();

    setFocusPolicy(Qt::StrongFocus);

    updateEditActions();
}
//=============================================================================
void
QtVariablesEditor::updateEditActions()
{
    VariableAbstractTableModel* model = nullptr;
    QWidget* currentWidget = m_tabWidget ? m_tabWidget->currentWidget() : nullptr;
    QTableView* tableView = qobject_cast<QTableView*>(currentWidget);
    if (tableView) {
        model = qobject_cast<VariableAbstractTableModel*>(tableView->model());
    }

    if (m_undoAction) {
        m_undoAction->setEnabled(model ? model->canUndo() : false);
    }
    if (m_redoAction) {
        m_redoAction->setEnabled(model ? model->canRedo() : false);
    }
    if (m_copyAction) {
        m_copyAction->setEnabled(model != nullptr);
    }
    if (m_pasteAction) {
        m_pasteAction->setEnabled(model != nullptr);
    }
}
//=============================================================================
void
QtVariablesEditor::showStatusMessage(const QString& message, bool isError)
{
    if (message.isEmpty()) {
        return;
    }
    if (m_statusBar) {
        m_statusBar->showMessage(message, isError ? 10000 : 5000);
        return;
    }
    // No status bar available: fall back to a modal notification.
    if (isError) {
        QMessageBox::warning(this, TR("Variables Editor"), message);
    } else {
        QMessageBox::information(this, TR("Variables Editor"), message);
    }
}
//=============================================================================
void
QtVariablesEditor::connectModelSignals(QObject* model)
{
    if (!model) {
        return;
    }
    // All variable models declare the same errorOccurred(QString) signal but do
    // not share it through a common base class: use the runtime connection.
    const auto connectionType
        = static_cast<Qt::ConnectionType>(Qt::AutoConnection | Qt::UniqueConnection);
    if (model->metaObject()->indexOfSignal("errorOccurred(QString)") >= 0) {
        connect(model, SIGNAL(errorOccurred(QString)), this, SLOT(onModelError(QString)),
            connectionType);
    }
    if (model->metaObject()->indexOfSignal("modelChanged()") >= 0) {
        connect(model, SIGNAL(modelChanged()), this, SLOT(updateEditActions()), connectionType);
    }
}
//=============================================================================
void
QtVariablesEditor::restorePosition()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    restoreGeometry(settings.value(SETTING_VARIABLES_EDITOR_GEOMETRY).toByteArray());
}
//=============================================================================
void
QtVariablesEditor::savePosition()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_VARIABLES_EDITOR_GEOMETRY, saveGeometry());
}
//=============================================================================
void
QtVariablesEditor::updateVariables()
{
    if (m_openedVariables.isEmpty() || !m_evaluator) {
        return;
    }

    QWidget* currentWidget = m_tabWidget->currentWidget();

    QStringList variablesToClose;

    for (auto it = m_openedVariables.constBegin(); it != m_openedVariables.constEnd(); ++it) {
        const QString& variableName = QString(it.key());
        const std::string variableNameUtf8 = wstring_to_utf8(QStringTowstring(variableName));

        ArrayOf currentValue;
        bool found = m_evaluator->getContext()->getCurrentScope()->lookupVariable(
            variableNameUtf8, currentValue);

        if (!found) {
            variablesToClose.append(variableName);
            continue;
        }

        QTableView* tableView = qobject_cast<QTableView*>(it.value());
        QtVariableTextEdit* textEdit = qobject_cast<QtVariableTextEdit*>(it.value());

        if (!tableView && !textEdit) {
            continue;
        }

        ArrayOf valueInView;
        if (tableView) {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (!model) {
                continue;
            }
            valueInView = model->array();
        } else {
            VariableDefaultModel* model = textEdit->model();
            if (!model) {
                continue;
            }
            valueInView = model->array();
        }

        const size_t currentHash = KeyHash(m_evaluator, valueInView);
        const size_t newHash = KeyHash(m_evaluator, currentValue);

        if (currentHash != newHash) {
            updateTabTitle(variableName);
            refreshVariable(variableName);
        }
    }

    m_tabWidget->setCurrentWidget(currentWidget);
    for (const QString& varName : variablesToClose) {
        closeVariableTab(varName);
    }
}
//=============================================================================
bool
QtVariablesEditor::isVariableExisting(const QString& variableName) const
{
    if (!m_evaluator || !m_evaluator->getContext() || variableName.isEmpty()) {
        return false;
    }
    ArrayOf value;
    return m_evaluator->getContext()->getCurrentScope()->lookupVariable(
        wstring_to_utf8(QStringTowstring(variableName)), value);
}
//=============================================================================
bool
QtVariablesEditor::isGraphicsObjectExisting(long long handle) const
{
    if (!m_evaluator || !m_evaluator->getContext()) {
        return false;
    }
    FunctionDefPtr functionDef = nullptr;
    if (!m_evaluator->getContext()->lookupFunction("isgraphics", functionDef) || !functionDef) {
        return false;
    }
    ArrayOfVector inputs;
    inputs << ArrayOf::graphicsObject(static_cast<go_handle>(handle));
    ArrayOfVector outputs = functionDef->evaluateFunction(m_evaluator, inputs, 1);
    if (outputs.empty() || !outputs[0].isLogical()) {
        return false;
    }
    return outputs[0].asLogicalScalar();
}
//=============================================================================
bool
QtVariablesEditor::openVariable(const QString& variableName)
{
    if (!m_evaluator || !m_evaluator->getContext()) {
        return false;
    }

    ArrayOf value;
    const std::string variableNameUtf8 = wstring_to_utf8(QStringTowstring(variableName));
    // look the variable up in the scope the user is actually in: openvar() is
    // callable from a script or from a function being debugged, where the
    // variable does not live in the base scope.
    bool found
        = m_evaluator->getContext()->getCurrentScope()->lookupVariable(variableNameUtf8, value);
    if (!found) {
        found = m_evaluator->getContext()->getBaseScope()->lookupVariable(variableNameUtf8, value);
    }

    bool containsVariable = m_openedVariables.contains(variableName);

    if (!found) {
        if (containsVariable) {
            closeVariableTab(variableName);
            return true;
        }
        // never a modal box here: openvar() is a scriptable builtin and a modal
        // dialog would block any non interactive run.
        showStatusMessage(wstringToQString(Nelson::resolveMessageTemplate(
                                               L"Nelson:gui:variableDoesExistWorkspace",
                                               L"Variable '%1' does not exist in the workspace."))
                              .arg(variableName),
            true);
        return false;
    }

    if (!containsVariable) {
        // Each displayed cell costs a QVariant: warn, but never block waiting
        // for an answer, for the same reason as above.
        constexpr double LARGE_VARIABLE_ELEMENT_COUNT = 1.0e6;
        const double elementCount = static_cast<double>(value.elementCount());
        if (elementCount > LARGE_VARIABLE_ELEMENT_COUNT) {
            showStatusMessage(TR("Variable '%1' has %2 elements, displaying it may take a while.")
                                  .arg(variableName)
                                  .arg(elementCount, 0, 'f', 0),
                true);
        }
    }

    Nelson::setApplicationWaitCursor();
    restorePosition();
    setVisible(true);

    if (containsVariable) {
        m_tabWidget->setCurrentWidget(m_openedVariables[variableName]);
        QTableView* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
        VariableAbstractTableModel* model
            = tableView ? qobject_cast<VariableAbstractTableModel*>(tableView->model()) : nullptr;
        if (model) {
            size_t hash1 = KeyHash(m_evaluator, model->array());
            size_t hash2 = KeyHash(m_evaluator, value);
            if (hash1 != hash2) {
                refreshCurrentVariable();
            }
            updateEditActions();
            Nelson::restoreApplicationWaitCursor();
            return true;
        }
        refreshVariable(variableName);
        updateEditActions();
        Nelson::restoreApplicationWaitCursor();
        return true;
    }

    QWidget* viewWidget = nullptr;

    NelsonType variableType = value.dataClass();
    if (value.isSparse() || !value.is2D()) {
        variableType = NLS_UNKNOWN;
    }

    QString tabTitle = variableName;
    switch (variableType) {
    case NLS_CLASSDEF_ARRAY:
    case NLS_CLASS_ARRAY: {
        if (value.isTable()) {
            QTableView* tableView = new QTableView();
            tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
            tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

            tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

            VariableTableTableModel* model
                = new VariableTableTableModel(variableName, value, m_evaluator, tableView);
            tableView->setModel(model);
            connectModelSignals(model);
            connect(model, &VariableTableTableModel::modelChanged, this, [this, tableView]() {
                VariableAbstractTableModel* model
                    = qobject_cast<VariableAbstractTableModel*>(tableView->model());
                if (model) {
                    this->updateTabTitle(model->variableName());
                }
            });

            createTableContextMenu(tableView);
            viewWidget = tableView;
            tabTitle += " [" + model->getVariableClassAsString() + " "
                + model->getVariableDimensionsAsString() + "]";
            tableView->installEventFilter(this);

            connect(tableView->horizontalHeader(), &QHeaderView::sectionDoubleClicked, this,
                [tableView](int logicalIndex) {
                    auto* model = qobject_cast<VariableTableTableModel*>(tableView->model());
                    if (!model) {
                        return;
                    }

                    QString current
                        = model->headerData(logicalIndex, Qt::Horizontal, Qt::DisplayRole)
                              .toString();
                    bool ok = false;
                    QString newName = QInputDialog::getText(tableView, QObject::tr("Rename Field"),
                        QObject::tr("Enter new name for field:"), QLineEdit::Normal, current, &ok);

                    if (ok && !newName.isEmpty() && newName != current) {
                        if (!model->setHeaderData(
                                logicalIndex, Qt::Horizontal, newName, Qt::EditRole)) {
                            QMessageBox::warning(tableView, QObject::tr("Rename Failed"),
                                QObject::tr("Could not rename field. The name may be duplicate or "
                                            "invalid."));
                        }
                    }
                });

            connect(tableView->verticalHeader(), &QHeaderView::sectionDoubleClicked, this,
                [tableView](int logicalIndex) {
                    auto* model = qobject_cast<VariableTableTableModel*>(tableView->model());
                    if (!model) {
                        return;
                    }

                    if (!model->haveRowNames()) {
                        return;
                    }

                    QString currentName
                        = model->headerData(logicalIndex, Qt::Vertical, Qt::DisplayRole).toString();
                    bool ok = false;
                    QString newName = QInputDialog::getText(tableView, QObject::tr("Rename Field"),
                        QObject::tr("Enter new name for row:"), QLineEdit::Normal, currentName,
                        &ok);

                    if (ok && !newName.isEmpty() && newName != currentName) {
                        if (!model->setHeaderData(
                                logicalIndex, Qt::Vertical, newName, Qt::EditRole)) {
                            QMessageBox::warning(tableView, QObject::tr("Rename Failed"),
                                QObject::tr("Could not rename row. The name may be duplicate or "
                                            "invalid."));
                        }
                    }
                });
        } else {
            QtVariableTextEdit* textEdit = new QtVariableTextEdit(variableName, value, m_evaluator);
            textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            textEdit->setReadOnly(true);
            textEdit->setFont(defaultReadOnlyFont());
            connectModelSignals(textEdit);
            createTextEditContextMenu(textEdit);
            viewWidget = textEdit;
            tabTitle += " [" + textEdit->getVariableClassAsString() + " "
                + textEdit->getVariableDimensionsAsString() + "]" + readOnlyTabSuffix();
        }

    } break;
    case NLS_STRUCT_ARRAY: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableStructTableModel* model
            = new VariableStructTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableStructTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

        connect(tableView->horizontalHeader(), &QHeaderView::sectionDoubleClicked, this,
            [tableView](int logicalIndex) {
                if (logicalIndex == 0) {
                    return;
                }

                auto* model = qobject_cast<VariableStructTableModel*>(tableView->model());
                if (!model) {
                    return;
                }

                QString currentName
                    = model->headerData(logicalIndex, Qt::Horizontal, Qt::DisplayRole).toString();
                bool ok = false;
                QString newName = QInputDialog::getText(tableView, QObject::tr("Rename Field"),
                    QObject::tr("Enter new name for field:"), QLineEdit::Normal, currentName, &ok);

                if (ok && !newName.isEmpty() && newName != currentName) {
                    if (!model->setHeaderData(
                            logicalIndex, Qt::Horizontal, newName, Qt::EditRole)) {
                        QMessageBox::warning(tableView, QObject::tr("Rename Failed"),
                            QObject::tr(
                                "Could not rename field. The name may be duplicate or invalid."));
                    }
                }
            });

    } break;

    case NLS_CELL_ARRAY: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableCellTableModel* model
            = new VariableCellTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableCellTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;

    case NLS_STRING_ARRAY: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableStringTableModel* model
            = new VariableStringTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableStringTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;
    case NLS_LOGICAL: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableLogicalTableModel* model
            = new VariableLogicalTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableLogicalTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;

    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableIntegerTableModel* model
            = new VariableIntegerTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableIntegerTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);
    } break;

    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableSingleTableModel* model
            = new VariableSingleTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableSingleTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);
    } break;
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableDoubleTableModel* model
            = new VariableDoubleTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connectModelSignals(model);
        connect(model, &VariableDoubleTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;
    case NLS_CHAR: {
        if (value.isRowVectorCharacterArray() && !value.isEmpty()) {
            QTableView* tableView = new QTableView();
            tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
            tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

            tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

            VariableRowCharactersTableModel* model
                = new VariableRowCharactersTableModel(variableName, value, m_evaluator, tableView);
            tableView->setModel(model);
            connectModelSignals(model);
            connect(
                model, &VariableRowCharactersTableModel::modelChanged, this, [this, tableView]() {
                    VariableAbstractTableModel* model
                        = qobject_cast<VariableAbstractTableModel*>(tableView->model());
                    if (model) {
                        this->updateTabTitle(model->variableName());
                    }
                });

            createTableContextMenu(tableView);
            viewWidget = tableView;
            tabTitle += " [" + model->getVariableClassAsString() + " "
                + model->getVariableDimensionsAsString() + "]";
            tableView->installEventFilter(this);

        } else {
            QtVariableTextEdit* textEdit = new QtVariableTextEdit(variableName, value, m_evaluator);
            textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            textEdit->setReadOnly(true);
            textEdit->setFont(defaultReadOnlyFont());
            connectModelSignals(textEdit);
            createTextEditContextMenu(textEdit);
            viewWidget = textEdit;
            tabTitle += " [" + textEdit->getVariableClassAsString() + " "
                + textEdit->getVariableDimensionsAsString() + "]" + readOnlyTabSuffix();
        }
    } break;
    default: {
        QtVariableTextEdit* textEdit = new QtVariableTextEdit(variableName, value, m_evaluator);
        textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        textEdit->setReadOnly(true);
        textEdit->setFont(defaultReadOnlyFont());
        connectModelSignals(textEdit);
        createTextEditContextMenu(textEdit);
        viewWidget = textEdit;
        tabTitle += " [" + textEdit->getVariableClassAsString() + " "
            + textEdit->getVariableDimensionsAsString() + "]" + readOnlyTabSuffix();
    } break;
    }

    m_tabWidget->addTab(viewWidget, tabTitle);
    m_tabWidget->setCurrentWidget(viewWidget);
    m_openedVariables.insert(variableName, viewWidget);
    updateEditActions();

    Nelson::restoreApplicationWaitCursor();
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
    std::wstring command = L"display(" + variable.wname() + L", 'val');";
    std::wstring result;
    EvaluateConsoleCommandToString(m_evaluator, command, result);
    return wstringToQString(result);
}
//=============================================================================
void
QtVariablesEditor::updateTabTitle(const QString& variableName)
{
    if (!m_openedVariables.contains(variableName)) {
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

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
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                newTabTitle = newTabTitle + " [" + model->getVariableClassAsString() + " "
                    + model->getVariableDimensionsAsString() + "]";
                m_tabWidget->setTabText(tabIndex, newTabTitle);
                return;
            }
        }
        QtVariableTextEdit* textEdit = qobject_cast<QtVariableTextEdit*>(widget);
        if (textEdit) {
            VariableDefaultModel* model = textEdit->model();
            if (model) {
                newTabTitle = newTabTitle + " [" + model->getVariableClassAsString() + " "
                    + model->getVariableDimensionsAsString() + "]";
                if (textEdit->isReadOnly()) {
                    newTabTitle += readOnlyTabSuffix();
                }
                m_tabWidget->setTabText(tabIndex, newTabTitle);
            }
        }
    }
}
//=============================================================================
void
QtVariablesEditor::refreshVariable(const QString& variableName)
{
    if (!m_evaluator || !m_openedVariables.contains(variableName)) {
        return;
    }

    ArrayOf value;
    bool found = m_evaluator->getContext()->getCurrentScope()->lookupVariable(
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

        updateTabTitle(variableName);
        return;
    }

    if (tableView && shouldBeTableView) {
        QAbstractItemModel* currentModel = tableView->model();
        VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);

        VariableAbstractTableModel* newModel = nullptr;

        auto createOrReuseModel = [&](VariableAbstractTableModel* newModelCandidate) {
            if (model && model->isStructureCompatible(value)) {
                model->refreshFromArray(value);
                delete newModelCandidate;
                return static_cast<VariableAbstractTableModel*>(nullptr);
            }
            return newModelCandidate;
        };
        if (value.isLogical()) {
            newModel = createOrReuseModel(
                new VariableLogicalTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isIntegerType()) {
            newModel = createOrReuseModel(
                new VariableIntegerTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isSingleType()) {
            newModel = createOrReuseModel(
                new VariableSingleTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isDoubleType()) {
            newModel = createOrReuseModel(
                new VariableDoubleTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isStringArray()) {
            newModel = createOrReuseModel(
                new VariableStringTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isStruct()) {
            newModel = createOrReuseModel(
                new VariableStructTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isCell()) {
            newModel = createOrReuseModel(
                new VariableCellTableModel(variableName, value, m_evaluator, tableView));
        } else if (value.isTable()) {
            newModel = createOrReuseModel(
                new VariableTableTableModel(variableName, value, m_evaluator, tableView));
        }

        if (newModel) {
            tableView->setModel(newModel);
            connectModelSignals(newModel);
            delete currentModel;
        }

        updateTabTitle(variableName);
        updateEditActions();
        return;
    }

    closeVariableTab(variableName);
    openVariable(variableName);
}
//=============================================================================
void
QtVariablesEditor::createVariableFromSelection()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        QMessageBox::warning(this, TR("Create Variable"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:tableCurrentlyActive", L"No table is currently active.")));
        return;
    }

    VariableAbstractTableModel* model
        = qobject_cast<VariableAbstractTableModel*>(currentTableView->model());

    if (!model) {
        QMessageBox::warning(this, TR("Create Variable"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:currentViewDoesSupport",
                L"Current view does not support variable creation.")));
        return;
    }

    QModelIndexList selected = currentTableView->selectionModel()->selectedIndexes();
    if (!model->isValidSelectionForExtraction(selected)) {
        QMessageBox::information(this, TR("Create Variable"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:invalidSelectionVariableCreation",
                    L"Invalid selection for variable creation.")));
        return;
    }
    QString newVarName
        = QInputDialog::getText(this, TR("New Variable"), TR("Enter new variable name:"));
    if (newVarName.isEmpty() || !IsValidVariableName(QStringTowstring(newVarName))) {
        QMessageBox::critical(this, TR("Error"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:validVariableNameExpected", L"Valid variable name expected.")));
        return;
    }

    ArrayOf newVar;
    if (model) {
        newVar = model->createArrayFromSelection(selected);
    }
    if (newVar.isEmpty()) {
        QMessageBox::critical(this, TR("Error"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:failedCreateVariableFrom",
                L"Failed to create variable from selection.")));
        return;
    }

    if (m_evaluator && m_evaluator->getContext()) {
        m_evaluator->getContext()->getCurrentScope()->insertVariable(
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

    VariableAbstractTableModel* model
        = qobject_cast<VariableAbstractTableModel*>(currentTableView->model());

    if (!model) {
        return;
    }

    QModelIndexList selectedIndexes = currentTableView->selectionModel()->selectedIndexes();
    QString clipboardText;
    if (model) {
        clipboardText = model->getSelectedDataAsText(selectedIndexes);
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

    QAbstractItemModel* baseModel = currentTableView->model();
    if (!baseModel) {
        return;
    }

    VariableAbstractTableModel* modelAbstract
        = qobject_cast<VariableAbstractTableModel*>(baseModel);

    if (!modelAbstract) {
        return;
    }

    auto model = static_cast<QAbstractItemModel*>(modelAbstract);

    QClipboard* clipboard = QApplication::clipboard();
    if (!clipboard) {
        return;
    }

    QString clipboardText = clipboard->text().trimmed();
    if (clipboardText.isEmpty()) {
        QMessageBox::information(
            this, TR("Paste"), TR("Clipboard is empty or contains no text data."));
        return;
    }

    QItemSelectionModel* selectionModel = currentTableView->selectionModel();
    if (!selectionModel) {
        QMessageBox::warning(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:selectionModelAvailable", L"No selection model available.")));
        return;
    }

    QModelIndex currentIndex = currentTableView->currentIndex();
    int startRow, startCol;

    if (currentIndex.isValid()) {
        startRow = currentIndex.row();
        startCol = currentIndex.column();
    } else {
        QModelIndexList selectedIndexes = selectionModel->selectedIndexes();
        if (!selectedIndexes.isEmpty()) {
            std::sort(selectedIndexes.begin(), selectedIndexes.end(),
                [](const QModelIndex& a, const QModelIndex& b) {
                    if (a.row() != b.row()) {
                        return a.row() < b.row();
                    }
                    return a.column() < b.column();
                });
            startRow = selectedIndexes.first().row();
            startCol = selectedIndexes.first().column();
        } else {
            startRow = 0;
            startCol = 0;
        }
    }

    if (startRow < 0 || startCol < 0) {
        QMessageBox::warning(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:invalidPastePosition", L"Invalid paste position.")));
        return;
    }

    QSize pasteSize;
    try {
        if (modelAbstract) {
            pasteSize = modelAbstract->getPasteSize(clipboardText);
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Error"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:errorAnalyzingClipboardData",
                    L"Error analyzing clipboard data: %1"))
                .arg(QString::fromStdString(e.what())));
        return;
    }

    if (!pasteSize.isValid() || pasteSize.width() <= 0 || pasteSize.height() <= 0) {
        QMessageBox::warning(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:invalidClipboardDataFormat", L"Invalid clipboard data format.")));
        return;
    }

    int currentRowCount = model->rowCount();
    int currentColCount = model->columnCount();
    int requiredRows = startRow + pasteSize.height();
    int requiredCols = startCol + pasteSize.width();

    bool needsExtension = false;

    if (requiredRows > currentRowCount) {
        int rowsToAdd = requiredRows - currentRowCount;
        try {
            for (int i = 0; i < rowsToAdd; ++i) {
                if (!modelAbstract->insertRowAt(currentRowCount + i)) {
                    QMessageBox::warning(this, TR("Paste Error"),
                        wstringToQString(
                            Nelson::resolveMessageTemplate(L"Nelson:gui:failedExtendTableRows",
                                L"Failed to extend table rows. Paste may be incomplete.")));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Error"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(
                        L"Nelson:gui:errorExtendingTableRows", L"Error extending table rows: %1"))
                    .arg(QString::fromStdString(e.what())));
        } catch (...) {
            QMessageBox::warning(this, TR("Paste Error"),
                wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:cannotExtendTableRows",
                    L"Cannot extend table rows. Paste area may extend beyond table boundaries.")));
        }
    }

    if (requiredCols > currentColCount) {
        int colsToAdd = requiredCols - currentColCount;
        try {
            for (int i = 0; i < colsToAdd; ++i) {
                if (!modelAbstract->insertColumnAt(currentColCount + i)) {
                    QMessageBox::warning(this, TR("Paste Error"),
                        wstringToQString(
                            Nelson::resolveMessageTemplate(L"Nelson:gui:failedExtendTableColumns",
                                L"Failed to extend table columns. Paste may be incomplete.")));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Error"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(L"Nelson:gui:errorExtendingTableColumns",
                        L"Error extending table columns: %1"))
                    .arg(QString::fromStdString(e.what())));
        } catch (...) {
            QMessageBox::warning(this, TR("Paste Error"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(L"Nelson:gui:cannotExtendTableColumns",
                        L"Cannot extend table columns. Paste area may extend beyond table "
                        L"boundaries.")));
        }
    }

    if (needsExtension) {
        currentTableView->reset();
        currentTableView->update();
    }

    bool pasteSuccess = false;

    try {
        if (modelAbstract) {
            pasteSuccess = modelAbstract->pasteDataFromClipboard(startRow, startCol, clipboardText);
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:errorOccurredWhilePasting",
                                 L"An error occurred while pasting: %1"))
                .arg(QString::fromStdString(e.what())));
        return;
    } catch (...) {
        QMessageBox::critical(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:unknownErrorOccurredWhile",
                L"An unknown error occurred while pasting data.")));
        return;
    }

    if (pasteSuccess) {
        currentTableView->reset();
        currentTableView->update();

        if (pasteSize.isValid() && pasteSize.width() > 0 && pasteSize.height() > 0) {
            int maxRow = std::min(startRow + pasteSize.height() - 1, model->rowCount() - 1);
            int maxCol = std::min(startCol + pasteSize.width() - 1, model->columnCount() - 1);

            if (maxRow >= startRow && maxCol >= startCol) {
                QModelIndex topLeft = model->index(startRow, startCol);
                QModelIndex bottomRight = model->index(maxRow, maxCol);

                if (topLeft.isValid() && bottomRight.isValid()) {
                    selectionModel->select(
                        QItemSelection(topLeft, bottomRight), QItemSelectionModel::ClearAndSelect);

                    currentTableView->setCurrentIndex(topLeft);
                }
            }
        }

        QString currentVariableName;
        QWidget* currentWidget = m_tabWidget->currentWidget();
        for (auto it = m_openedVariables.begin(); it != m_openedVariables.end(); ++it) {
            if (it.value() == currentWidget) {
                currentVariableName = it.key();
                break;
            }
        }

        if (!currentVariableName.isEmpty()) {
            updateTabTitle(currentVariableName);
        }

        WorkspaceBrowser::updateWorkspaceBrowser();

        updateEditActions();

    } else {
        QMessageBox::warning(this, TR("Paste Error"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:couldPasteDataPlease",
                L"Could not paste data. Please check that:\n The data format is compatible with "
                L"the table\n The clipboard contains valid data")));
    }
}
//=============================================================================
void
QtVariablesEditor::refreshCurrentVariable()
{
    int currentIndex = m_tabWidget->currentIndex();
    if (currentIndex < 0 || m_openedVariables.isEmpty()) {
        return;
    }

    QWidget* currentWidget = m_tabWidget->currentWidget();
    if (!currentWidget) {
        return;
    }

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
QtVariablesEditor::onModelSelectionCreated(const QString& suggestedName, const ArrayOf& newArray)
{
    // Reserved slot: the selectionCreated() signal is declared by the models but
    // is never emitted today. Variable creation from a selection is handled
    // synchronously by createVariableFromSelection(). Kept (and intentionally
    // not connected) so that models can later notify the editor asynchronously.
    Q_UNUSED(suggestedName);
    Q_UNUSED(newArray);
}
//=============================================================================
void
QtVariablesEditor::onModelError(const QString& message)
{
    showStatusMessage(message, true);
}
//=============================================================================
void
QtVariablesEditor::closeVariableTab(const QString& variableName)
{
    if (!m_openedVariables.contains(variableName)) {
        return;
    }

    QWidget* widget = m_openedVariables[variableName];

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
    switch (value.dataClass()) {
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_STRING_ARRAY:
    case NLS_DOUBLE: {
        asText = false;
    } break;
    case NLS_CHAR: {
        if (value.isRowVectorCharacterArray() && !value.isEmpty()) {
            asText = false; // Row vector character arrays are displayed as tables
        } else {
            asText = true; // Other character arrays are displayed as text
        }
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
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->insertRowAt(currentIndex.row());
    }
}
//=============================================================================
void
QtVariablesEditor::insertRowBelow()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->insertRowAt(currentIndex.row() + 1);
    }
}
//=============================================================================
void
QtVariablesEditor::insertColumnLeft()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->insertColumnAt(tableView->currentIndex().column());
        return;
    }
}
//=============================================================================
void
QtVariablesEditor::insertColumnRight()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->insertColumnAt(currentIndex.column() + 1);
    }
}
//=============================================================================
void
QtVariablesEditor::deleteRow()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->deleteRowAt(currentIndex.row());
    }
}
//=============================================================================
void
QtVariablesEditor::deleteColumn()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }
    const QModelIndex currentIndex = tableView->currentIndex();
    if (!currentIndex.isValid()) {
        return;
    }
    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }
    VariableAbstractTableModel* model = qobject_cast<VariableAbstractTableModel*>(currentModel);
    if (model) {
        model->deleteColumnAt(currentIndex.column());
    }
}
//=============================================================================
void
QtVariablesEditor::createTableContextMenu(QTableView* tableView)
{
    auto* model = tableView->model();
    auto* stringModel = qobject_cast<VariableStringTableModel*>(model);
    auto* structModel = qobject_cast<VariableStructTableModel*>(model);
    auto* tableModel = qobject_cast<VariableTableTableModel*>(model);

    tableView->setContextMenuPolicy(Qt::ActionsContextMenu);
    tableView->addAction(m_copyAction);
    tableView->addAction(m_pasteAction);

    // Separators are parented to the table view so that they are destroyed
    // together with the tab they belong to.
    auto addSeparator = [tableView]() {
        QAction* sep = new QAction(tableView);
        sep->setSeparator(true);
        tableView->addAction(sep);
    };

    addSeparator();
    tableView->addAction(m_undoAction);
    tableView->addAction(m_redoAction);

    if (tableModel) {
        return;
    }

    addSeparator();

    if (!structModel) {
        tableView->addAction(m_pasteExcelDataAction);
        addSeparator();
    }

    tableView->addAction(m_insertRowAboveAction);
    tableView->addAction(m_insertRowBelowAction);
    tableView->addAction(m_insertColLeftAction);
    tableView->addAction(m_insertColRightAction);
    addSeparator();
    tableView->addAction(m_deleteRowAction);
    tableView->addAction(m_deleteColAction);

    if (stringModel) {
        addSeparator();
        tableView->addAction(m_replaceByEmptyAction);
    }
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
void
QtVariablesEditor::replaceByEmpty()
{
    auto* tableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!tableView) {
        return;
    }

    QAbstractItemModel* currentModel = tableView->model();
    if (!currentModel) {
        return;
    }

    VariableStringTableModel* stringModel = qobject_cast<VariableStringTableModel*>(currentModel);
    if (!stringModel) {
        return;
    }

    QModelIndexList indexesToReplace;
    QModelIndexList selectedIndexes = tableView->selectionModel()->selectedIndexes();

    if (!selectedIndexes.isEmpty()) {
        indexesToReplace = selectedIndexes;
    } else {
        QModelIndex currentIndex = tableView->currentIndex();
        if (currentIndex.isValid()) {
            indexesToReplace.append(currentIndex);
        } else {
            QMessageBox::information(this, TR("Replace by Empty"),
                TR("Please navigate to a cell or select items you want to replace with missing "
                   "values."));
            return;
        }
    }

    try {
        if (stringModel->replaceSelectedWithMissing(indexesToReplace)) {
            tableView->reset();
            tableView->update();

            QString variableName = stringModel->variableName();
            updateTabTitle(variableName);

            WorkspaceBrowser::updateWorkspaceBrowser();
        } else {
            QMessageBox::warning(this, TR("Replace by Empty"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(L"Nelson:gui:failedReplaceItemsMissing",
                        L"Failed to replace items with missing values.")));
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Replace by Empty Error"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:errorOccurredWhileReplacing",
                    L"An error occurred while replacing items: %1"))
                .arg(QString::fromStdString(e.what())));
    } catch (...) {
        QMessageBox::critical(this, TR("Replace by Empty Error"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:unknownErrorOccurredWhileReplacing",
                    L"An unknown error occurred while replacing items.")));
    }
}
//=============================================================================
bool
QtVariablesEditor::eventFilter(QObject* obj, QEvent* event)
{
    // Handle ShortcutOverride to intercept Ctrl+V before QAction gets it
    if (event->type() == QEvent::ShortcutOverride) {
        QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
        QTableView* tableView = qobject_cast<QTableView*>(obj);

        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_V
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept();
            return true;
        }
        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_C
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept();
            return true;
        }
        if (keyEvent->key() == Qt::Key_V && keyEvent->modifiers().testFlag(Qt::ControlModifier)
            && keyEvent->modifiers().testFlag(Qt::ShiftModifier)) {
            event->accept();
            return true;
        }

        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_Z
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept();
            return true;
        }
        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_Y
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept();
            return true;
        }
    }

    if (!event || event->type() != QEvent::KeyPress) {
        return QDockWidget::eventFilter(obj, event);
    }

    const QKeyEvent* keyEvent = static_cast<const QKeyEvent*>(event);

    QTableView* tableView = qobject_cast<QTableView*>(obj);
    if (!tableView || !m_tabWidget || tableView != m_tabWidget->currentWidget()) {
        return QDockWidget::eventFilter(obj, event);
    }

    if (keyEvent->key() == Qt::Key_V && keyEvent->modifiers().testFlag(Qt::ControlModifier)
        && keyEvent->modifiers().testFlag(Qt::ShiftModifier)) {
        pasteExcelDataFromClipboard();
        return true;
    }

    if (keyEvent->key() == Qt::Key_Z && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
        undo();
        return true;
    }

    if (keyEvent->key() == Qt::Key_Y && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
        redo();
        return true;
    }

    if (keyEvent->key() == Qt::Key_V && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
        pasteDataFromClipboard();
        return true;
    }

    if (keyEvent->key() == Qt::Key_C && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
        copySelectedCells();
        return true;
    }

    if (keyEvent->matches(QKeySequence::Copy)) {
        copySelectedCells();
        return true;
    }

    if (keyEvent->key() == Qt::Key_Delete) {
        // Only handle delete for string models that support replacement by empty
        if (auto* stringModel = qobject_cast<VariableStringTableModel*>(tableView->model())) {
            replaceByEmpty();
            return true;
        }
    }

    return QDockWidget::eventFilter(obj, event);
}
//=============================================================================
void
QtVariablesEditor::pasteExcelDataFromClipboard()
{
    QTableView* currentTableView = qobject_cast<QTableView*>(m_tabWidget->currentWidget());
    if (!currentTableView) {
        return;
    }

    QAbstractItemModel* baseModel = currentTableView->model();
    if (!baseModel) {
        return;
    }

    VariableAbstractTableModel* modelAbstract
        = qobject_cast<VariableAbstractTableModel*>(baseModel);
    if (!modelAbstract) {
        return;
    }

    QClipboard* clipboard = QApplication::clipboard();
    if (!clipboard) {
        return;
    }

    QString clipboardText;
    const QMimeData* mimeData = clipboard->mimeData();

    if (!mimeData) {
        return;
    }

    // Excel uses these MIME types for rich data
    const bool hasExcelData = mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff12\"")
        || mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff8\"")
        || mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff5\"")
        || mimeData->hasFormat("text/html");

    clipboardText = clipboard->text().trimmed();

    if (clipboardText.isEmpty()) {
        QMessageBox::information(this, TR("Paste Excel Data"),
            TR("Clipboard is empty or contains no text data.\n"
               "Please copy data from Excel first."));
        return;
    }

    if (hasExcelData) {
        showStatusMessage(TR("Excel formatting detected."));
    }

    QItemSelectionModel* selectionModel = currentTableView->selectionModel();
    if (!selectionModel) {
        QMessageBox::warning(this, TR("Paste Excel Data"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:selectionModelAvailable", L"No selection model available.")));
        return;
    }

    QModelIndex currentIndex = currentTableView->currentIndex();
    int startRow, startCol;

    if (currentIndex.isValid()) {
        startRow = currentIndex.row();
        startCol = currentIndex.column();
    } else {
        QModelIndexList selectedIndexes = selectionModel->selectedIndexes();
        if (!selectedIndexes.isEmpty()) {
            std::sort(selectedIndexes.begin(), selectedIndexes.end(),
                [](const QModelIndex& a, const QModelIndex& b) {
                    if (a.row() != b.row()) {
                        return a.row() < b.row();
                    }
                    return a.column() < b.column();
                });
            startRow = selectedIndexes.first().row();
            startCol = selectedIndexes.first().column();
        } else {
            startRow = 0;
            startCol = 0;
        }
    }

    if (startRow < 0 || startCol < 0) {
        QMessageBox::warning(this, TR("Paste Excel Data"),
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:invalidPastePosition", L"Invalid paste position.")));
        return;
    }

    QString processedText = preprocessExcelData(clipboardText);

    QSize pasteSize;
    try {
        pasteSize = modelAbstract->getPasteSize(processedText);
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Excel Data"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:errorAnalyzingExcelClipboard",
                    L"Error analyzing Excel clipboard data: %1"))
                .arg(QString::fromStdString(e.what())));
        return;
    }

    if (!pasteSize.isValid() || pasteSize.width() <= 0 || pasteSize.height() <= 0) {
        QMessageBox::warning(this, TR("Paste Excel Data"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:invalidExcelDataFormat",
                L"Invalid Excel data format.\nPlease ensure you copied a valid range from "
                L"Excel.")));
        return;
    }

    constexpr int LARGE_PASTE_CELL_COUNT = 10000;
    if ((qint64)pasteSize.width() * (qint64)pasteSize.height() > LARGE_PASTE_CELL_COUNT) {
        int ret = QMessageBox::question(this, TR("Paste Excel Data"),
            TR("You are about to paste %1 rows and %2 columns of Excel data.\n"
               "This may take some time. Continue?")
                .arg(pasteSize.height())
                .arg(pasteSize.width()),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

        if (ret != QMessageBox::Yes) {
            return;
        }
    }

    auto model = static_cast<QAbstractItemModel*>(modelAbstract);
    int currentRowCount = model->rowCount();
    int currentColCount = model->columnCount();
    int requiredRows = startRow + pasteSize.height();
    int requiredCols = startCol + pasteSize.width();

    bool needsExtension = false;

    if (requiredRows > currentRowCount) {
        int rowsToAdd = requiredRows - currentRowCount;
        try {
            for (int i = 0; i < rowsToAdd; ++i) {
                if (!modelAbstract->insertRowAt(currentRowCount + i)) {
                    QMessageBox::warning(this, TR("Paste Excel Data"),
                        wstringToQString(
                            Nelson::resolveMessageTemplate(L"Nelson:gui:failedExtendTableRows",
                                L"Failed to extend table rows. Paste may be incomplete.")));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Excel Data"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(
                        L"Nelson:gui:errorExtendingTableRows", L"Error extending table rows: %1"))
                    .arg(QString::fromStdString(e.what())));
        }
    }

    if (requiredCols > currentColCount) {
        int colsToAdd = requiredCols - currentColCount;
        try {
            for (int i = 0; i < colsToAdd; ++i) {
                if (!modelAbstract->insertColumnAt(currentColCount + i)) {
                    QMessageBox::warning(this, TR("Paste Excel Data"),
                        wstringToQString(
                            Nelson::resolveMessageTemplate(L"Nelson:gui:failedExtendTableColumns",
                                L"Failed to extend table columns. Paste may be incomplete.")));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Excel Data"),
                wstringToQString(
                    Nelson::resolveMessageTemplate(L"Nelson:gui:errorExtendingTableColumns",
                        L"Error extending table columns: %1"))
                    .arg(QString::fromStdString(e.what())));
        }
    }

    if (needsExtension) {
        currentTableView->reset();
        currentTableView->update();
    }

    bool pasteSuccess = false;
    try {
        pasteSuccess = modelAbstract->pasteDataFromExcel(startRow, startCol, processedText);
    } catch (const std::exception&) {
        QMessageBox::critical(this, TR("Paste Excel Data"),
            wstringToQString(
                Nelson::resolveMessageTemplate(L"Nelson:gui:errorOccurredWhilePastingExcel",
                    L"An error occurred while pasting Excel data.")));
        return;
    }

    if (pasteSuccess) {
        currentTableView->reset();
        currentTableView->update();

        if (pasteSize.isValid() && pasteSize.width() > 0 && pasteSize.height() > 0) {
            int maxRow = std::min(startRow + pasteSize.height() - 1, model->rowCount() - 1);
            int maxCol = std::min(startCol + pasteSize.width() - 1, model->columnCount() - 1);

            if (maxRow >= startRow && maxCol >= startCol) {
                QModelIndex topLeft = model->index(startRow, startCol);
                QModelIndex bottomRight = model->index(maxRow, maxCol);

                if (topLeft.isValid() && bottomRight.isValid()) {
                    selectionModel->select(
                        QItemSelection(topLeft, bottomRight), QItemSelectionModel::ClearAndSelect);
                    currentTableView->setCurrentIndex(topLeft);
                }
            }
        }

        QString currentVariableName;
        QWidget* currentWidget = m_tabWidget->currentWidget();
        for (auto it = m_openedVariables.begin(); it != m_openedVariables.end(); ++it) {
            if (it.value() == currentWidget) {
                currentVariableName = it.key();
                break;
            }
        }

        if (!currentVariableName.isEmpty()) {
            updateTabTitle(currentVariableName);
        }

        WorkspaceBrowser::updateWorkspaceBrowser();
        updateEditActions();
    }
}
//=============================================================================
QString
QtVariablesEditor::preprocessExcelData(const QString& rawData)
{
    QString processedData = rawData;

    // Unicode minus sign used by some locales
    processedData.replace(QChar(0x2212), QChar('-'));

    // Remove thousand separators only when the whole cell is unambiguously a
    // grouped integer such as "1,234" or "12,345,678". A comma used as a
    // decimal separator (FR locale, "3,14") is left untouched: corrupting the
    // data would be much worse than not normalizing it.
    static const QRegularExpression groupedNumber("\\A[+-]?\\d{1,3}(,\\d{3})+(\\.\\d+)?\\z");

    processedData.replace("E+", "e+");
    processedData.replace("E-", "e-");

    QRegularExpression percentPattern("(\\d+(?:\\.\\d+)?)%");
    QRegularExpressionMatchIterator percentIt = percentPattern.globalMatch(processedData);
    while (percentIt.hasNext()) {
        QRegularExpressionMatch match = percentIt.next();
        QString numberStr = match.captured(1);
        double value = numberStr.toDouble() / 100.0;
        processedData.replace(match.captured(0), QString::number(value, 'g', 15));
    }

    processedData.replace("TRUE", "true", Qt::CaseInsensitive);
    processedData.replace("FALSE", "false", Qt::CaseInsensitive);

    QStringList lines = processedData.split('\n');
    for (QString& line : lines) {
        QStringList cells = line.split('\t');
        for (QString& cell : cells) {
            cell = cell.trimmed();
            if (groupedNumber.match(cell).hasMatch()) {
                cell.remove(QLatin1Char(','));
            }
        }
        line = cells.join('\t');
    }
    processedData = lines.join('\n');

    return processedData;
}
//=============================================================================
void
QtVariablesEditor::undo()
{
    QWidget* currentWidget = m_tabWidget->currentWidget();
    if (!currentWidget) {
        return;
    }

    if (auto* tableView = qobject_cast<QTableView*>(currentWidget)) {
        if (auto* model = qobject_cast<VariableAbstractTableModel*>(tableView->model())) {
            if (!model->canUndo()) {
                showStatusMessage(TR("Nothing to undo."));
                return;
            }
            model->undo();
            tableView->reset();
            tableView->update();
        }
    } else if (auto* textEdit = qobject_cast<QtVariableTextEdit*>(currentWidget)) {
        textEdit->undo();
    }
    updateEditActions();
}
//=============================================================================
void
QtVariablesEditor::redo()
{
    QWidget* currentWidget = m_tabWidget->currentWidget();
    if (!currentWidget) {
        return;
    }

    if (auto* tableView = qobject_cast<QTableView*>(currentWidget)) {
        if (auto* model = qobject_cast<VariableAbstractTableModel*>(tableView->model())) {
            if (!model->canRedo()) {
                showStatusMessage(TR("Nothing to redo."));
                return;
            }
            model->redo();
            tableView->reset();
            tableView->update();
        }
    } else if (auto* textEdit = qobject_cast<QtVariableTextEdit*>(currentWidget)) {
        textEdit->redo();
    }
    updateEditActions();
}
//=============================================================================
