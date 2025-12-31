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
#include "EvaluateCommand.hpp"
#include "KeyHash.hpp"
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
    m_toolBar->setIconSize(QSize(18, 18));
    m_toolBar->setMovable(false);
    m_toolBar->setFloatable(false);
    m_toolBar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

    // Create refresh action
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

    m_pasteExcelDataAction = new QAction(TR("Paste Excel Data"), this);
    m_pasteExcelDataAction->setShortcut(QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_V));
    m_pasteExcelDataAction->setToolTip(
        TR("Paste data from Excel with enhanced formatting support"));
    connect(m_pasteExcelDataAction, &QAction::triggered, this,
        &QtVariablesEditor::pasteExcelDataFromClipboard);

    // Handle docking location changes
    connect(this, &QDockWidget::dockLocationChanged, this,
        &QtVariablesEditor::handleDockLocationChanged);

    m_undoAction = new QAction(TR("Undo"), this);
    m_undoAction->setShortcut(QKeySequence::Undo);
    connect(m_undoAction, &QAction::triggered, this, &QtVariablesEditor::undo);

    m_redoAction = new QAction(TR("Redo"), this);
    m_redoAction->setShortcut(QKeySequence::Redo);
    connect(m_redoAction, &QAction::triggered, this, &QtVariablesEditor::redo);
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
    if (m_openedVariables.isEmpty() || !m_evaluator) {
        return;
    }

    QWidget* currentWidget = m_tabWidget->currentWidget();

    QStringList variablesToClose;

    for (auto it = m_openedVariables.constBegin(); it != m_openedVariables.constEnd(); ++it) {
        const QString& variableName = QString(it.key());
        const std::string variableNameUtf8 = wstring_to_utf8(QStringTowstring(variableName));

        ArrayOf currentValue;
        bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
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
QtVariablesEditor::openVariable(const QString& variableName)
{
    QApplication::setOverrideCursor(Qt::WaitCursor);
    restorePosition();
    setVisible(true);

    ArrayOf value;
    bool found = m_evaluator->getContext()->getBaseScope()->lookupVariable(
        wstring_to_utf8(QStringTowstring(variableName)), value);

    bool containsVariable = m_openedVariables.contains(variableName);

    if (!found && containsVariable) {
        closeVariableTab(variableName);
        QApplication::restoreOverrideCursor();
        return true;
    }

    if (containsVariable) {
        m_tabWidget->setCurrentWidget(m_openedVariables[variableName]);
        QTableView* tableView = (QTableView*)(m_tabWidget->currentWidget());
        VariableAbstractTableModel* model
            = qobject_cast<VariableAbstractTableModel*>(tableView->model());
        if (model) {
            size_t hash1 = KeyHash(m_evaluator, model->array());
            size_t hash2 = KeyHash(m_evaluator, value);
            if (hash1 == hash2) {
                QApplication::restoreOverrideCursor();
                return true;
            }
            refreshCurrentVariable();
            QApplication::restoreOverrideCursor();
            return true;
        }
        QApplication::restoreOverrideCursor();
        return false;
    }

    QWidget* viewWidget = nullptr;

    NelsonType variableType = value.getDataClass();
    if (value.isSparse() || !value.is2D()) {
        variableType = NLS_UNKNOWN;
    }

    QString tabTitle = variableName;
    switch (variableType) {
    case NLS_CLASS_ARRAY: {
        if (value.getClassType() == NLS_TABLE_TYPE) {
            QTableView* tableView = new QTableView();
            tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
            tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

            tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

            VariableTableTableModel* model
                = new VariableTableTableModel(variableName, value, m_evaluator, tableView);
            tableView->setModel(model);
            connect(model, &VariableTableTableModel::modelChanged, this, [this, tableView]() {
                VariableAbstractTableModel* model
                    = qobject_cast<VariableAbstractTableModel*>(tableView->model());
                if (model) {
                    this->updateTabTitle(model->variableName());
                }
            });

            // Set up context menu for the table view
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
            textEdit->setFont(QFont("Courier", 10));
            createTextEditContextMenu(textEdit);
            viewWidget = textEdit;
            tabTitle += " [" + textEdit->getVariableClassAsString() + " "
                + textEdit->getVariableDimensionsAsString() + "]";
        }

    } break;
    case NLS_STRUCT_ARRAY: {
        // Create table view for struct type
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableStructTableModel* model
            = new VariableStructTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableStructTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

        connect(tableView->horizontalHeader(), &QHeaderView::sectionDoubleClicked, this,
            [tableView](int logicalIndex) {
                if (logicalIndex == 0) {
                    return; // Skip the index column
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
        // Create table view for cell type
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableCellTableModel* model
            = new VariableCellTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableCellTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;

    case NLS_STRING_ARRAY: {
        // Create table view for string type
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableStringTableModel* model
            = new VariableStringTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableStringTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);

    } break;
    case NLS_LOGICAL: {
        // Create table view for logical type
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableLogicalTableModel* model
            = new VariableLogicalTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableLogicalTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });
        // Set up context menu for the table view
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
        // Create table view for integer type
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableIntegerTableModel* model
            = new VariableIntegerTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableIntegerTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });
        // Set up context menu for the table view
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
        connect(model, &VariableSingleTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        // Set up context menu for the table view
        createTableContextMenu(tableView);
        viewWidget = tableView;
        tabTitle += " [" + model->getVariableClassAsString() + " "
            + model->getVariableDimensionsAsString() + "]";
        tableView->installEventFilter(this);
    } break;
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        // Create table view for double and double complex types
        QTableView* tableView = new QTableView();
        tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        tableView->setSelectionBehavior(QAbstractItemView::SelectItems);
        tableView->setSelectionMode(QAbstractItemView::ContiguousSelection);

        tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Interactive);

        VariableDoubleTableModel* model
            = new VariableDoubleTableModel(variableName, value, m_evaluator, tableView);
        tableView->setModel(model);
        connect(model, &VariableDoubleTableModel::modelChanged, this, [this, tableView]() {
            VariableAbstractTableModel* model
                = qobject_cast<VariableAbstractTableModel*>(tableView->model());
            if (model) {
                this->updateTabTitle(model->variableName());
            }
        });

        // Set up context menu for the table view
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
            connect(
                model, &VariableRowCharactersTableModel::modelChanged, this, [this, tableView]() {
                    VariableAbstractTableModel* model
                        = qobject_cast<VariableAbstractTableModel*>(tableView->model());
                    if (model) {
                        this->updateTabTitle(model->variableName());
                    }
                });

            // Set up context menu for the table view
            createTableContextMenu(tableView);
            viewWidget = tableView;
            tabTitle += " [" + model->getVariableClassAsString() + " "
                + model->getVariableDimensionsAsString() + "]";
            tableView->installEventFilter(this);

        } else {
            QtVariableTextEdit* textEdit = new QtVariableTextEdit(variableName, value, m_evaluator);
            textEdit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
            textEdit->setReadOnly(true);
            textEdit->setFont(QFont("Courier", 10));
            createTextEditContextMenu(textEdit);
            viewWidget = textEdit;
            tabTitle += " [" + textEdit->getVariableClassAsString() + " "
                + textEdit->getVariableDimensionsAsString() + "]";
        }
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
    } break;
    }

    m_tabWidget->addTab(viewWidget, tabTitle);
    m_tabWidget->setCurrentWidget(viewWidget);
    m_openedVariables.insert(variableName, viewWidget);

    QApplication::restoreOverrideCursor();
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
        } else if (value.isClassType() && value.getClassType() == NLS_TABLE_TYPE) {
            newModel = createOrReuseModel(
                new VariableTableTableModel(variableName, value, m_evaluator, tableView));
        }

        if (newModel) {
            tableView->setModel(newModel);
            delete currentModel;
        }

        updateTabTitle(variableName);
        return;
    }

    // fallback
    closeVariableTab(variableName);
    openVariable(variableName);
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

    VariableAbstractTableModel* model
        = qobject_cast<VariableAbstractTableModel*>(currentTableView->model());

    if (!model) {
        QMessageBox::warning(
            this, TR("Create Variable"), TR("Current view does not support variable creation."));
        return;
    }

    QModelIndexList selected = currentTableView->selectionModel()->selectedIndexes();
    if (!model->isValidSelectionForExtraction(selected)) {
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
    if (model) {
        newVar = model->createArrayFromSelection(selected);
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
    // Get the current table view
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

    // Use a common interface or determine which model to use
    auto model = static_cast<QAbstractItemModel*>(modelAbstract);

    // Check clipboard content
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

    // Get selection information
    QItemSelectionModel* selectionModel = currentTableView->selectionModel();
    if (!selectionModel) {
        QMessageBox::warning(this, TR("Paste Error"), TR("No selection model available."));
        return;
    }

    // Use the current/focused item as the paste position instead of selected items
    QModelIndex currentIndex = currentTableView->currentIndex();
    int startRow, startCol;

    if (currentIndex.isValid()) {
        // Use the focused cell as the starting position
        startRow = currentIndex.row();
        startCol = currentIndex.column();
    } else {
        // If no current index, check if we have any selection
        QModelIndexList selectedIndexes = selectionModel->selectedIndexes();
        if (!selectedIndexes.isEmpty()) {
            // Sort selected indexes to get the top-left corner
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
            // Default to top-left corner if nothing is selected or focused
            startRow = 0;
            startCol = 0;
        }
    }

    // Validate paste position
    if (startRow < 0 || startCol < 0) {
        QMessageBox::warning(this, TR("Paste Error"), TR("Invalid paste position."));
        return;
    }

    // Calculate the required size for the paste data
    QSize pasteSize;
    try {
        if (modelAbstract) {
            pasteSize = modelAbstract->getPasteSize(clipboardText);
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Error"),
            TR("Error analyzing clipboard data: %1").arg(QString::fromStdString(e.what())));
        return;
    }

    if (!pasteSize.isValid() || pasteSize.width() <= 0 || pasteSize.height() <= 0) {
        QMessageBox::warning(this, TR("Paste Error"), TR("Invalid clipboard data format."));
        return;
    }

    // Check if we need to extend the table and do so if necessary
    int currentRowCount = model->rowCount();
    int currentColCount = model->columnCount();
    int requiredRows = startRow + pasteSize.height();
    int requiredCols = startCol + pasteSize.width();

    bool needsExtension = false;

    // Extend rows if necessary
    if (requiredRows > currentRowCount) {
        int rowsToAdd = requiredRows - currentRowCount;
        try {
            for (int i = 0; i < rowsToAdd; ++i) {
                if (!modelAbstract->insertRowAt(currentRowCount + i)) {
                    QMessageBox::warning(this, TR("Paste Error"),
                        TR("Failed to extend table rows. Paste may be incomplete."));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Error"),
                TR("Error extending table rows: %1").arg(QString::fromStdString(e.what())));
        } catch (...) {
            QMessageBox::warning(this, TR("Paste Error"),
                TR("Cannot extend table rows. Paste area may extend beyond table boundaries."));
        }
    }

    // Extend columns if necessary
    if (requiredCols > currentColCount) {
        int colsToAdd = requiredCols - currentColCount;
        try {
            for (int i = 0; i < colsToAdd; ++i) {
                if (!modelAbstract->insertColumnAt(currentColCount + i)) {
                    QMessageBox::warning(this, TR("Paste Error"),
                        TR("Failed to extend table columns. Paste may be incomplete."));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Error"),
                TR("Error extending table columns: %1").arg(QString::fromStdString(e.what())));
        } catch (...) {
            QMessageBox::warning(this, TR("Paste Error"),
                TR("Cannot extend table columns. Paste area may extend beyond table boundaries."));
        }
    }

    // Refresh the model if we extended the table
    if (needsExtension) {
        currentTableView->reset();
        currentTableView->update();
    }

    // Attempt to paste data
    bool pasteSuccess = false;

    try {
        if (modelAbstract) {
            pasteSuccess = modelAbstract->pasteDataFromClipboard(startRow, startCol, clipboardText);
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Error"),
            TR("An error occurred while pasting: %1").arg(QString::fromStdString(e.what())));
        return;
    } catch (...) {
        QMessageBox::critical(
            this, TR("Paste Error"), TR("An unknown error occurred while pasting data."));
        return;
    }

    if (pasteSuccess) {
        // Refresh the view
        currentTableView->reset();
        currentTableView->update();

        // Update selection to highlight pasted area
        if (pasteSize.isValid() && pasteSize.width() > 0 && pasteSize.height() > 0) {
            // Ensure we don't go beyond model bounds (though we should have extended if needed)
            int maxRow = std::min(startRow + pasteSize.height() - 1, model->rowCount() - 1);
            int maxCol = std::min(startCol + pasteSize.width() - 1, model->columnCount() - 1);

            if (maxRow >= startRow && maxCol >= startCol) {
                QModelIndex topLeft = model->index(startRow, startCol);
                QModelIndex bottomRight = model->index(maxRow, maxCol);

                if (topLeft.isValid() && bottomRight.isValid()) {
                    selectionModel->select(
                        QItemSelection(topLeft, bottomRight), QItemSelectionModel::ClearAndSelect);

                    // Set the current index to the top-left of the pasted area
                    currentTableView->setCurrentIndex(topLeft);
                }
            }
        }

        // Update tab title in case dimensions changed
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

        // Update workspace browser
        WorkspaceBrowser::updateWorkspaceBrowser();

    } else {
        QMessageBox::warning(this, TR("Paste Error"),
            TR("Could not paste data. Please check that:\n"
               "� The data format is compatible with the table\n"
               "� The clipboard contains valid data"));
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

    auto addSeparator = [this, tableView]() {
        QAction* sep = new QAction(this);
        sep->setSeparator(true);
        tableView->addAction(sep);
    };

    if (tableModel) {
        return;
    }

    addSeparator();

    if (!structModel) {
        tableView->addAction(m_pasteExcelDataAction);
        addSeparator();
    }

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
        // Use selected items
        indexesToReplace = selectedIndexes;
    } else {
        // Use current item if nothing is selected
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

    // Perform the replacement
    try {
        if (stringModel->replaceSelectedWithMissing(indexesToReplace)) {
            // Refresh the view
            tableView->reset();
            tableView->update();

            // Update the tab title in case dimensions changed
            QString variableName = stringModel->variableName();
            updateTabTitle(variableName);

            // Update workspace browser if available
            WorkspaceBrowser::updateWorkspaceBrowser();
        } else {
            QMessageBox::warning(
                this, TR("Replace by Empty"), TR("Failed to replace items with missing values."));
        }
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Replace by Empty Error"),
            TR("An error occurred while replacing items: %1")
                .arg(QString::fromStdString(e.what())));
    } catch (...) {
        QMessageBox::critical(this, TR("Replace by Empty Error"),
            TR("An unknown error occurred while replacing items."));
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
            event->accept(); // This prevents the QAction from handling it
            return true;
        }
        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_C
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept(); // This prevents the QAction from handling it
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
            event->accept(); // This prevents the QAction from handling it
            return true;
        }
        if (tableView && m_tabWidget && tableView == m_tabWidget->currentWidget()
            && keyEvent->key() == Qt::Key_Y
            && keyEvent->modifiers().testFlag(Qt::ControlModifier)) {
            event->accept(); // This prevents the QAction from handling it
            return true;
        }
    }

    // Early return if event is null or not a key press
    if (!event || event->type() != QEvent::KeyPress) {
        return QDockWidget::eventFilter(obj, event);
    }

    // Safe cast to QKeyEvent (already verified type above)
    const QKeyEvent* keyEvent = static_cast<const QKeyEvent*>(event);

    // Check if object is a QTableView and matches current widget
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

    // Handle paste - now this should work since ShortcutOverride intercepted it
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

    // Let parent handle other events
    return QDockWidget::eventFilter(obj, event);
}
//=============================================================================
void
QtVariablesEditor::pasteExcelDataFromClipboard()
{
    // Get the current table view
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

    // Check clipboard content
    QClipboard* clipboard = QApplication::clipboard();
    if (!clipboard) {
        return;
    }

    // Try to get Excel-specific formats first
    QString clipboardText;
    const QMimeData* mimeData = clipboard->mimeData();

    if (!mimeData) {
        return;
    }

    // Check for Excel-specific MIME types
    bool hasExcelData = false;

    // Excel uses these MIME types for rich data
    if (mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff12\"")
        || mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff8\"")
        || mimeData->hasFormat("application/x-qt-windows-mime;value=\"Biff5\"")
        || mimeData->hasFormat("text/html")) {
        hasExcelData = true;
    }

    // Get the text data (this works for most Excel copy operations)
    clipboardText = clipboard->text().trimmed();

    if (clipboardText.isEmpty()) {
        QMessageBox::information(this, TR("Paste Excel Data"),
            TR("Clipboard is empty or contains no text data.\n"
               "Please copy data from Excel first."));
        return;
    }

    // Show info about Excel data detection
    if (hasExcelData) {
        QMessageBox::information(this, TR("Paste Excel Data"),
            TR("Excel formatting detected. Processing Excel data..."));
    }

    // Get paste position
    QItemSelectionModel* selectionModel = currentTableView->selectionModel();
    if (!selectionModel) {
        QMessageBox::warning(this, TR("Paste Excel Data"), TR("No selection model available."));
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
        QMessageBox::warning(this, TR("Paste Excel Data"), TR("Invalid paste position."));
        return;
    }

    // Process Excel-specific formatting
    QString processedText = preprocessExcelData(clipboardText);

    // Calculate required size
    QSize pasteSize;
    try {
        pasteSize = modelAbstract->getPasteSize(processedText);
    } catch (const std::exception& e) {
        QMessageBox::critical(this, TR("Paste Excel Data"),
            TR("Error analyzing Excel clipboard data: %1").arg(QString::fromStdString(e.what())));
        return;
    }

    if (!pasteSize.isValid() || pasteSize.width() <= 0 || pasteSize.height() <= 0) {
        QMessageBox::warning(this, TR("Paste Excel Data"),
            TR("Invalid Excel data format.\n"
               "Please ensure you copied a valid range from Excel."));
        return;
    }

    // Show preview dialog for large pastes
    if (pasteSize.width() > 10 || pasteSize.height() > 20) {
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

    // Extend table if necessary
    auto model = static_cast<QAbstractItemModel*>(modelAbstract);
    int currentRowCount = model->rowCount();
    int currentColCount = model->columnCount();
    int requiredRows = startRow + pasteSize.height();
    int requiredCols = startCol + pasteSize.width();

    bool needsExtension = false;

    // Extend rows if necessary
    if (requiredRows > currentRowCount) {
        int rowsToAdd = requiredRows - currentRowCount;
        try {
            for (int i = 0; i < rowsToAdd; ++i) {
                if (!modelAbstract->insertRowAt(currentRowCount + i)) {
                    QMessageBox::warning(this, TR("Paste Excel Data"),
                        TR("Failed to extend table rows. Paste may be incomplete."));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Excel Data"),
                TR("Error extending table rows: %1").arg(QString::fromStdString(e.what())));
        }
    }

    // Extend columns if necessary
    if (requiredCols > currentColCount) {
        int colsToAdd = requiredCols - currentColCount;
        try {
            for (int i = 0; i < colsToAdd; ++i) {
                if (!modelAbstract->insertColumnAt(currentColCount + i)) {
                    QMessageBox::warning(this, TR("Paste Excel Data"),
                        TR("Failed to extend table columns. Paste may be incomplete."));
                    break;
                }
            }
            needsExtension = true;
        } catch (const std::exception& e) {
            QMessageBox::warning(this, TR("Paste Excel Data"),
                TR("Error extending table columns: %1").arg(QString::fromStdString(e.what())));
        }
    }

    // Refresh model if extended
    if (needsExtension) {
        currentTableView->reset();
        currentTableView->update();
    }

    // Attempt to paste the processed Excel data
    bool pasteSuccess = false;
    try {
        pasteSuccess = modelAbstract->pasteDataFromExcel(startRow, startCol, processedText);
    } catch (const std::exception&) {
        QMessageBox::critical(
            this, TR("Paste Excel Data"), TR("An error occurred while pasting Excel data."));
        return;
    }

    if (pasteSuccess) {
        // Refresh the view
        currentTableView->reset();
        currentTableView->update();

        // Update selection to highlight pasted area
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

        // Update tab title and workspace
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
    }
}
//=============================================================================
QString
QtVariablesEditor::preprocessExcelData(const QString& rawData)
{
    QString processedData = rawData;

    processedData.replace(QChar(0x2212), QChar('-'));

    processedData.replace("\t\t", "\t\t");

    QRegularExpression thousandSep("(\\d{1,3}),(\\d{3})");
    while (thousandSep.match(processedData).hasMatch()) {
        processedData.replace(thousandSep, "\\1\\2");
    }

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
            model->undo();
            tableView->reset();
            tableView->update();
        }
    } else if (auto* textEdit = qobject_cast<QtVariableTextEdit*>(currentWidget)) {
        textEdit->undo();
    }
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
            model->redo();
            tableView->reset();
            tableView->update();
        }
    } else if (auto* textEdit = qobject_cast<QtVariableTextEdit*>(currentWidget)) {
        textEdit->redo();
    }
}
//=============================================================================
