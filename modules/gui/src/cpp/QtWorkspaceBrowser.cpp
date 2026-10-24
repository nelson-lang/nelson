//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtWorkspaceBrowser.h"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "WorkspaceBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "QtFileDialogHistory.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "VariablesEditor.hpp"
#include <QtCore/QSettings>
#include <QtCore/QRegularExpression>
#include <QtCore/QItemSelection>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMenu>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QFileDialog>
#include <QtGui/QStandardItem>
//=============================================================================
const QString SETTING_WORKSPACE_BROWSER_VISIBILITY = "wb_Visibility";
const QString SETTING_WORKSPACE_BROWSER_GEOMETRY = "wb_Geometry";
const QString SETTING_WORKSPACE_BROWSER_HEADER_ORDER = "wb_Header_Order_v2";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SORT = "wb_Header_Sort_v2";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER = "wb_Header_Sort_Order_v2";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SIZES = "wb_Header_Sizes_v2";
//=============================================================================
using namespace Nelson;
//=============================================================================
#define WORKSPACE_VALUE_PREVIEW_MAX_LENGTH 120
//=============================================================================
bool
WorkspaceSortFilterProxyModel::lessThan(const QModelIndex& left, const QModelIndex& right) const
{
    const QVariant leftKey = sourceModel()->data(left, WORKSPACE_SORT_ROLE);
    const QVariant rightKey = sourceModel()->data(right, WORKSPACE_SORT_ROLE);
    if (leftKey.isValid() && rightKey.isValid()) {
        bool leftOk = false;
        bool rightOk = false;
        const double leftValue = leftKey.toDouble(&leftOk);
        const double rightValue = rightKey.toDouble(&rightOk);
        if (leftOk && rightOk && leftValue != rightValue) {
            return leftValue < rightValue;
        }
        if (leftOk && rightOk) {
            return QSortFilterProxyModel::lessThan(left, right);
        }
    }
    return QSortFilterProxyModel::lessThan(left, right);
}
//=============================================================================
QtWorkspaceBrowser::QtWorkspaceBrowser(QWidget* parent)
    : m_context(nullptr), QDockWidget(TR("Workspace Browser"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    QVBoxLayout* mainLayout = new QVBoxLayout;

    setupModel();

    m_tableView = new QTableView;
    m_tableView->setModel(m_proxyModel);
    m_tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    m_tableView->setSortingEnabled(true);
    m_tableView->verticalHeader()->setVisible(false);
    m_tableView->setSelectionMode(QAbstractItemView::ExtendedSelection);
    m_tableView->setSelectionBehavior(QAbstractItemView::SelectRows);

    setupHeaders();

    setupFilterBar(mainLayout);
    mainLayout->addWidget(m_tableView);

    QHeaderView* header = m_tableView->horizontalHeader();
    header->resizeSection(NAME_COLUMN, 64);

    header->setContextMenuPolicy(Qt::CustomContextMenu);
    header->setSectionsMovable(true);

    connect(header, SIGNAL(customContextMenuRequested(const QPoint&)), this,
        SLOT(showHeaderContextMenu(const QPoint&)));

    connect(m_tableView, &QTableView::doubleClicked, this, &QtWorkspaceBrowser::doubleClicked);

    setFocusPolicy(Qt::StrongFocus);

    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(mainLayout);
    setWidget(centralWidget);

    setObjectName("workspaceBrowser");
    setMinimumSize(50, 50);
    setTitleBarWidget(new QtDockTitleBar(this, "wb"));
}
//=============================================================================
QtWorkspaceBrowser::~QtWorkspaceBrowser()
{
    delete m_model;
    delete m_proxyModel;
}
//=============================================================================
void
QtWorkspaceBrowser::setupModel()
{
    m_model = new QStandardItemModel(this);
    m_proxyModel = new WorkspaceSortFilterProxyModel(this);
    m_proxyModel->setSourceModel(m_model);
    m_proxyModel->setSortCaseSensitivity(Qt::CaseInsensitive);
    m_proxyModel->setFilterCaseSensitivity(Qt::CaseInsensitive);
    m_proxyModel->setFilterKeyColumn(NAME_COLUMN);
}
//=============================================================================
void
QtWorkspaceBrowser::setupHeaders()
{
    m_model->setColumnCount(COLUMN_COUNT);
    m_model->setHorizontalHeaderItem(NAME_COLUMN, new QStandardItem(TR("Name")));
    m_model->setHorizontalHeaderItem(VALUE_COLUMN, new QStandardItem(TR("Value")));
    m_model->setHorizontalHeaderItem(CLASS_COLUMN, new QStandardItem(TR("Class")));
    m_model->setHorizontalHeaderItem(SIZE_COLUMN, new QStandardItem(TR("Size")));
    m_model->setHorizontalHeaderItem(BYTES_COLUMN, new QStandardItem(TR("Bytes")));
    m_model->setHorizontalHeaderItem(SCOPE_COLUMN, new QStandardItem(TR("Scope")));
}
//=============================================================================
void
QtWorkspaceBrowser::setupFilterBar(QVBoxLayout* mainLayout)
{
    m_filterLineEdit = new QLineEdit(this);
    m_filterLineEdit->setClearButtonEnabled(true);
    m_filterLineEdit->setPlaceholderText(TR("Filter variables by name"));
    connect(
        m_filterLineEdit, &QLineEdit::textChanged, this, &QtWorkspaceBrowser::applyVariableFilter);
    mainLayout->addWidget(m_filterLineEdit);
}
//=============================================================================
void
QtWorkspaceBrowser::applyVariableFilter()
{
    const QString pattern = m_filterLineEdit->text().trimmed();
    m_proxyModel->setFilterKeyColumn(NAME_COLUMN);
    m_proxyModel->setFilterRegularExpression(QRegularExpression(
        QRegularExpression::escape(pattern), QRegularExpression::CaseInsensitiveOption));
}
//=============================================================================
void
QtWorkspaceBrowser::setContext(Context* context)
{
    m_context = context;
}
//=============================================================================
void
QtWorkspaceBrowser::restorePosition()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    restoreGeometry(settings.value(SETTING_WORKSPACE_BROWSER_GEOMETRY).toByteArray());

    QByteArray headerOrderData
        = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_ORDER).toByteArray();
    m_tableView->horizontalHeader()->restoreState(headerOrderData);

    int sortColumn = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_SORT, 0).toInt();
    Qt::SortOrder sortOrder = static_cast<Qt::SortOrder>(settings
            .value(
                SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER, static_cast<int>(Qt::AscendingOrder))
            .toInt());

    m_tableView->sortByColumn(sortColumn, sortOrder);

    QByteArray headerSizesData
        = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_SIZES).toByteArray();
    m_tableView->horizontalHeader()->restoreState(headerSizesData);
}
//=============================================================================
void
QtWorkspaceBrowser::restoreVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    bool savedVisibility = settings.value(SETTING_WORKSPACE_BROWSER_VISIBILITY, true).toBool();
    setVisible(savedVisibility);
}
//=============================================================================
void
QtWorkspaceBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_WORKSPACE_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_WORKSPACE_BROWSER_GEOMETRY, saveGeometry());

    settings.setValue(
        SETTING_WORKSPACE_BROWSER_HEADER_ORDER, m_tableView->horizontalHeader()->saveState());
    settings.setValue(SETTING_WORKSPACE_BROWSER_HEADER_SORT,
        m_tableView->horizontalHeader()->sortIndicatorSection());
    settings.setValue(SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER,
        static_cast<int>(m_tableView->horizontalHeader()->sortIndicatorOrder()));
    settings.setValue(
        SETTING_WORKSPACE_BROWSER_HEADER_SIZES, m_tableView->horizontalHeader()->saveState());
}
//=============================================================================
void
QtWorkspaceBrowser::doubleClicked(const QModelIndex& index)
{
    QModelIndex sourceIndex = m_proxyModel->mapToSource(index);
    int selectedRow = sourceIndex.row();

    QStandardItem* nameItem = m_model->item(selectedRow, NAME_COLUMN);
    if (nameItem) {
        QString name = nameItem->text();
        VariablesEditor::openVariable(name.toStdWString());
        VariablesEditor::showVariablesEditor();
    }
}
//=============================================================================
void
QtWorkspaceBrowser::contextMenuEvent(QContextMenuEvent* event)
{
    QMenu contextMenu(this);

    const QStringList selectedNames = getSelectedVariableNames();
    const bool hasSelection = !selectedNames.isEmpty();
    const bool hasSingleSelection = (selectedNames.size() == 1);

    QAction* deleteAction = new QAction(TR("Delete"), this);
    connect(deleteAction, &QAction::triggered, this, &QtWorkspaceBrowser::onDeleteAction);
    contextMenu.addAction(deleteAction);
    deleteAction->setEnabled(hasSelection);

    QAction* renameAction = new QAction(TR("Rename"), this);
    connect(renameAction, &QAction::triggered, this, &QtWorkspaceBrowser::onRenameAction);
    contextMenu.addAction(renameAction);
    renameAction->setEnabled(hasSingleSelection);

    QAction* saveAsAction = new QAction(TR("Save As"), this);
    connect(saveAsAction, &QAction::triggered, this, &QtWorkspaceBrowser::onSaveAsAction);
    contextMenu.addAction(saveAsAction);
    saveAsAction->setEnabled(hasSelection);

    contextMenu.addSeparator();

    QString variableName = hasSingleSelection ? selectedNames.first() : QString();
    if (variableName.isEmpty()) {
        contextMenu.exec(event->globalPos());
        return;
    }
    ArrayOf* variable = m_context->lookupVariable(wstring_to_utf8(QStringTowstring(variableName)));
    if (!variable) {
        contextMenu.exec(event->globalPos());
        return;
    }
    if (!variable->isNumeric()) {
        contextMenu.exec(event->globalPos());
        return;
    }
    if (variable->isEmpty()) {
        contextMenu.exec(event->globalPos());
        return;
    }
    if (variable->isScalar()) {
        contextMenu.exec(event->globalPos());
        return;
    }
    if (!variable->is2D()) {
        contextMenu.exec(event->globalPos());
        return;
    }

    QStringList vectorArg = { "plot", "bar", "pie", "hist" };
    QStringList matrixOnlyArg = { "surf", "mesh", "image" };
    QStringList sparseOnlyArg = { "spy" };

    QMenu* plotGalleryMenu = contextMenu.addMenu(TR("Plot Gallery"));
    if (!variable->isSparse()) {
        for (auto plotName : vectorArg) {
            QString command = getPlotCommand(plotName, variableName);
            QAction* plotAction = new QAction(command, this);
            connect(plotAction, &QAction::triggered, [this, command]() { onPlotAction(command); });
            plotGalleryMenu->addAction(plotAction);
        }
        if (!variable->isVector()) {
            for (auto plotName : matrixOnlyArg) {
                QString command = getPlotCommand(plotName, variableName);
                QAction* plotAction = new QAction(command, this);
                connect(
                    plotAction, &QAction::triggered, [this, command]() { onPlotAction(command); });
                plotGalleryMenu->addAction(plotAction);
            }
        }
    }
    if (variable->isSparse()) {
        for (auto plotName : sparseOnlyArg) {
            QString command = getPlotCommand(plotName, variableName);
            QAction* plotAction = new QAction(command, this);
            connect(plotAction, &QAction::triggered, [this, command]() { onPlotAction(command); });
            plotGalleryMenu->addAction(plotAction);
        }
    }

    contextMenu.exec(event->globalPos());
}
//=============================================================================
void
QtWorkspaceBrowser::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Delete) {
        onDeleteAction();
    } else if (event->key() == Qt::Key_F5) {
        updateVariables();
    }
    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QtWorkspaceBrowser::updateVariables()
{
    if (!m_context) {
        return;
    }

    // Preserve the current selection and the scroll position: the model is fully
    // rebuilt on each prompt and would otherwise lose both.
    const QStringList previouslySelected = getSelectedVariableNames();
    int scrollPosition = 0;
    if (m_tableView->verticalScrollBar()) {
        scrollPosition = m_tableView->verticalScrollBar()->value();
    }

    m_model->removeRows(0, m_model->rowCount());

    wstringVector variablesList;
    m_context->getCurrentScope()->getVariablesList(false, variablesList);
    wstringVector globalVariablesList;
    m_context->getGlobalScope()->getVariablesList(false, globalVariablesList);
    variablesList.insert(
        variablesList.end(), globalVariablesList.begin(), globalVariablesList.end());
    std::sort(variablesList.begin(), variablesList.end());
    auto last = std::unique(variablesList.begin(), variablesList.end());
    variablesList.erase(last, variablesList.end());

    for (int i = 0; i < variablesList.size(); i++) {
        bool isGlobal = m_context->isVariableGlobal(wstring_to_utf8(variablesList[i]));
        ArrayOf* variable = m_context->lookupVariable(wstring_to_utf8(variablesList[i]));
        if (variable) {
            QString variableName(wstringToQString(variablesList[i]));
            std::wstring variableClassname = ClassToStringW(variable->dataClass());
            if (variable->dataClass() == NLS_HANDLE) {
                variableClassname = utf8_to_wstring(variable->handleCategory());
            }
            if (variable->dataClass() == NLS_CLASS_ARRAY
                || variable->dataClass() == NLS_CLASSDEF_ARRAY) {
                variableClassname = utf8_to_wstring(variable->classType());
            }

            QString value(getVariableAsQString(variable));
            QString complexStr = variable->isComplex() ? QString("complex") : QString("");
            QString sparseStr = variable->isSparse() ? QString(NLS_SPARSE_STR) : QString("");
            QString subTypeStr = sparseStr + " " + complexStr;
            if (!complexStr.isEmpty() && !sparseStr.isEmpty()) {
                subTypeStr = sparseStr + " " + complexStr;
            } else {
                subTypeStr = complexStr + sparseStr;
            }
            QString type(wstringToQString(variableClassname));
            if (!subTypeStr.isEmpty()) {
                type = type + " (" + subTypeStr + ")";
            }
            QString flags = isGlobal ? TR("global") : "";
            QString size(wstringToQString(variable->dimensions().toWideString()));

            const double elementCount = (double)variable->dimensions().elementCount();
            // byteSize() throws for sparse arrays (like whos, which reports NaN);
            // guard it so assigning a sparse variable doesn't crash the browser.
            const double byteSize = variable->isSparse() ? 0.0 : (double)variable->byteSize();

            QStandardItem* nameItem = new QStandardItem(variableName);

            QString valuePreview(value);
            if (valuePreview.length() > WORKSPACE_VALUE_PREVIEW_MAX_LENGTH) {
                valuePreview
                    = valuePreview.left(WORKSPACE_VALUE_PREVIEW_MAX_LENGTH) + QChar(0x2026);
            }
            QStandardItem* valueItem = new QStandardItem(valuePreview);
            valueItem->setData(value, Qt::ToolTipRole);
            bool isScalarValue = false;
            const double scalarValue = value.toDouble(&isScalarValue);
            if (isScalarValue) {
                valueItem->setData(scalarValue, WORKSPACE_SORT_ROLE);
            }

            QStandardItem* typeItem = new QStandardItem(type);

            QStandardItem* sizeItem = new QStandardItem(size);
            sizeItem->setData(elementCount, WORKSPACE_SORT_ROLE);

            QStandardItem* bytesItem = new QStandardItem(
                variable->isSparse() ? QString("") : QString::number((qulonglong)byteSize));
            bytesItem->setData(byteSize, WORKSPACE_SORT_ROLE);

            QStandardItem* flagsItem = new QStandardItem(flags);

            QList<QStandardItem*> rowItems;
            rowItems << nameItem;
            rowItems << valueItem;
            rowItems << typeItem;
            rowItems << sizeItem;
            rowItems << bytesItem;
            rowItems << flagsItem;

            for (auto item : rowItems) {
                item->setEditable(false);
            }

            m_model->appendRow(rowItems);
        }
    }

    restoreSelectionAndScroll(previouslySelected, scrollPosition);
}
//=============================================================================
void
QtWorkspaceBrowser::restoreSelectionAndScroll(const QStringList& selectedNames, int scrollPosition)
{
    if (!selectedNames.isEmpty()) {
        QItemSelectionModel* selectionModel = m_tableView->selectionModel();
        if (selectionModel) {
            QItemSelection selection;
            for (int row = 0; row < m_model->rowCount(); ++row) {
                QStandardItem* item = m_model->item(row, NAME_COLUMN);
                if (item && selectedNames.contains(item->text())) {
                    QModelIndex first
                        = m_proxyModel->mapFromSource(m_model->index(row, NAME_COLUMN));
                    QModelIndex last = m_proxyModel->mapFromSource(
                        m_model->index(row, m_model->columnCount() - 1));
                    if (first.isValid() && last.isValid()) {
                        selection.select(first, last);
                    }
                }
            }
            if (!selection.isEmpty()) {
                selectionModel->select(selection, QItemSelectionModel::ClearAndSelect);
            }
        }
    }
    if (m_tableView->verticalScrollBar()) {
        m_tableView->verticalScrollBar()->setValue(scrollPosition);
    }
}
//=============================================================================
void
QtWorkspaceBrowser::showHeaderContextMenu(const QPoint& pos)
{
    QHeaderView* header = m_tableView->horizontalHeader();

    QMenu contextMenu(this);

    for (int i = 0; i < header->count(); ++i) {
        QAction* toggleAction
            = new QAction(header->model()->headerData(i, Qt::Horizontal).toString(), this);
        toggleAction->setCheckable(true);
        toggleAction->setChecked(!header->isSectionHidden(i));

        connect(toggleAction, &QAction::triggered,
            [=]() { header->setSectionHidden(i, !toggleAction->isChecked()); });

        contextMenu.addAction(toggleAction);
    }
    contextMenu.exec(header->mapToGlobal(pos));
}
//=============================================================================
QString
QtWorkspaceBrowser::getVariableAsQString(const ArrayOf* variable)
{
    QString value;

    if (!variable) {
        return value;
    }

    switch (variable->dataClass()) {
    case NLS_DOUBLE: {
        value = handleNumericVariable<double>(variable);
    } break;
    case NLS_SINGLE: {
        value = handleNumericVariable<single>(variable);
    } break;
    case NLS_DCOMPLEX: {
        value = handleComplexVariable<double>(variable);
    } break;
    case NLS_SCOMPLEX: {
        value = handleComplexVariable<single>(variable);
    } break;
    case NLS_INT8: {
        value = handleNumericVariable<int8>(variable);
    } break;
    case NLS_INT16: {
        value = handleNumericVariable<int16>(variable);
    } break;
    case NLS_INT32: {
        value = handleNumericVariable<int32>(variable);
    } break;
    case NLS_INT64: {
        value = handleNumericVariable<int64>(variable);
    } break;
    case NLS_UINT8: {
        value = handleNumericVariable<uint8>(variable);
    } break;
    case NLS_UINT16: {
        value = handleNumericVariable<uint16>(variable);
    } break;
    case NLS_UINT32: {
        value = handleNumericVariable<uint32>(variable);
    } break;
    case NLS_UINT64: {
        value = handleNumericVariable<uint64>(variable);
    } break;
    case NLS_LOGICAL: {
        value = handleLogicalVariable(variable);
    } break;
    case NLS_CHAR: {
        value = handleCharVariable(variable);
    } break;
    case NLS_CELL_ARRAY: {
        value = handleCellArray(variable);
        break;
    }
    case NLS_STRING_ARRAY: {
        value = handleStringArray(variable);
    } break;

    case NLS_FUNCTION_HANDLE: {
        value = handleFunctionHandle(variable);
    } break;
    default: {
        value = handleDefaultCase(variable);
    } break;
    }
    return value;
}
//=============================================================================
template <typename T>
QString
QtWorkspaceBrowser::handleNumericVariable(const ArrayOf* variable) const
{
    if (variable->isEmpty()) {
        return QString("[]");
    } else if (variable->isScalar() && !variable->isSparse()) {
        T* ptr = (T*)variable->dataPointer();
        T v = ptr[0];
        return QString::fromLatin1(std::to_string(v).c_str());
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
template <typename T>
QString
QtWorkspaceBrowser::handleComplexVariable(const ArrayOf* variable) const
{
    if (variable->isEmpty()) {
        return QString("[]");
    } else if (variable->isScalar()) {
        T* ptr = (T*)variable->dataPointer();
        std::complex<T>* ptrZ = reinterpret_cast<std::complex<T>*>(ptr);
        std::complex<T> v = ptrZ[0];
        std::string realPart = std::to_string(v.real());
        std::string imagPart = std::to_string(std::abs(v.imag()));
        std::string separator = v.imag() > 0 ? " + " : " - ";
        std::string res = realPart + separator + imagPart + "i";
        return QString::fromLatin1(res.c_str());
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleLogicalVariable(const ArrayOf* variable) const
{
    if (variable->isScalar()) {
        return wstringToQString(variable->asLogicalScalar() ? L"true" : L"false");
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleCharVariable(const ArrayOf* variable) const
{
    if (variable->isEmpty()) {
        return "''";
    } else {
        if (variable->isRowVector() || variable->isScalar()) {
            return wstringToQString(L"\'" + variable->asWideString() + L"\'");
        } else {
            return wstringToQString(variable->dimensions().toWideString() + L" char");
        }
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleCellArray(const ArrayOf* variable) const
{
    return (variable->isEmpty()) ? QString("{}") : handleDefaultCase(variable);
}
//=============================================================================
QString
QtWorkspaceBrowser::handleStringArray(const ArrayOf* variable) const
{
    if (variable->isScalar()) {
        ArrayOf* ptr = (ArrayOf*)variable->dataPointer();
        if (ptr && ptr->isDoubleClass()) {
            return wstringToQString(L"<missing>");
        }
        return wstringToQString(L"\"" + variable->asWideString() + L"\"");
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleFunctionHandle(const ArrayOf* variable) const
{
    if (variable->isScalar()) {
        function_handle fh = variable->asFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        return (cp) ? QString::fromStdString(cp->toString()) : QString();
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleDefaultCase(const ArrayOf* variable) const
{
    NelsonType valueType = variable->dataClass();
    std::wstring className = ClassToStringW(valueType);
    if (valueType == NLS_HANDLE) {
        className = utf8_to_wstring(variable->handleCategory());
    }
    if (valueType == NLS_CLASS_ARRAY || valueType == NLS_CLASSDEF_ARRAY) {
        className = utf8_to_wstring(variable->classType());
    }
    std::wstring variableClassname = (variable->isSparse()) ? L"sparse " + className : className;
    return wstringToQString(variable->dimensions().toWideString()) + " "
        + wstringToQString(variableClassname);
}
//=============================================================================
QString
QtWorkspaceBrowser::getCurrentVariableNameSelected()
{
    QString name;
    QModelIndexList selectedIndexes = m_tableView->selectionModel()->selectedIndexes();

    if (!selectedIndexes.isEmpty()) {
        QModelIndex sourceIndex = m_proxyModel->mapToSource(selectedIndexes.first());
        int selectedRow = sourceIndex.row();
        QStandardItem* nameItem = m_model->item(selectedRow, NAME_COLUMN);
        if (nameItem) {
            name = nameItem->text();
        }
    }
    return name;
}
//=============================================================================
QStringList
QtWorkspaceBrowser::getSelectedVariableNames()
{
    QStringList names;
    if (!m_tableView->selectionModel()) {
        return names;
    }
    const QModelIndexList selectedRows = m_tableView->selectionModel()->selectedRows(NAME_COLUMN);
    for (const QModelIndex& index : selectedRows) {
        QModelIndex sourceIndex = m_proxyModel->mapToSource(index);
        QStandardItem* nameItem = m_model->item(sourceIndex.row(), NAME_COLUMN);
        if (nameItem && !names.contains(nameItem->text())) {
            names << nameItem->text();
        }
    }
    if (names.isEmpty()) {
        QString name = getCurrentVariableNameSelected();
        if (!name.isEmpty()) {
            names << name;
        }
    }
    return names;
}
//=============================================================================
bool
QtWorkspaceBrowser::isGlobalVariable(const QString& name)
{
    if (!m_context || name.isEmpty()) {
        return false;
    }
    return m_context->isVariableGlobal(wstring_to_utf8(QStringTowstring(name)));
}
//=============================================================================
void
QtWorkspaceBrowser::onDeleteAction()
{
    QStringList names = getSelectedVariableNames();
    if (names.isEmpty()) {
        return;
    }
    QStringList localNames;
    QStringList globalNames;
    for (const QString& name : names) {
        if (isGlobalVariable(name)) {
            globalNames << QString("'%1'").arg(name);
        } else {
            localNames << QString("'%1'").arg(name);
        }
    }
    QString command;
    if (!localNames.isEmpty()) {
        command += QString("clear(%1);").arg(localNames.join(","));
    }
    if (!globalNames.isEmpty()) {
        command += QString("clear('global',%1);").arg(globalNames.join(","));
    }
    if (!command.isEmpty()) {
        emit postCommand(command);
    }
}
//=============================================================================
void
QtWorkspaceBrowser::onRenameAction()
{
    QString oldName = getCurrentVariableNameSelected();
    if (!oldName.isEmpty()) {
        bool ok;
        QString newName = QInputDialog::getText(
            this, TR("Rename Variable"), TR("Enter a new name:"), QLineEdit::Normal, oldName, &ok);

        if (ok && !newName.isEmpty() && newName != oldName) {
            QString clearCommand = isGlobalVariable(oldName)
                ? QString("clear('global','%1');").arg(oldName)
                : QString("clear('%1');").arg(oldName);
            QString command = QString("%2 = %1;").arg(oldName, newName) + clearCommand;
            emit postCommand(command);
        }
    }
}
//=============================================================================
void
QtWorkspaceBrowser::onSaveAsAction()
{
    QStringList variableNames = getSelectedVariableNames();
    if (variableNames.isEmpty()) {
        return;
    }
    QString defaultName
        = (variableNames.size() == 1) ? variableNames.first() : QString("workspace");
    QString newFileName = QFileDialog::getSaveFileName(this, TR("Save Variables As"),
        qtDialogInitialSavePath(L"desktop.save", QString(), defaultName),
        TR("Nelson Files (*.nh5)"));

    if (!newFileName.isEmpty()) {
        qtRecordDialogSelection(L"desktop.save", QStringLiteral("save"), newFileName);
        QStringList quotedNames;
        for (const QString& variableName : variableNames) {
            quotedNames << QString("'%1'").arg(variableName);
        }
        QString command = QString("save('%1',%2);").arg(newFileName, quotedNames.join(","));
        emit postCommand(command);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::getPlotCommand(const QString& plotType, const QString& variableName)
{
    return QString("%1(%2)").arg(plotType, variableName);
}
//=============================================================================
void
QtWorkspaceBrowser::onPlotAction(const QString& plotCommand)
{
    if (!plotCommand.isEmpty()) {
        QStringList commands;
        commands.push_back(plotCommand);
        emit sendCommands(commands);
    }
}
//=============================================================================
void
QtWorkspaceBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeWorkspaceBrowser();
}
//=============================================================================
