//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtWorkspaceBrowser.h"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "WorkspaceBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "ClassToString.hpp"
#include "AnonymousMacroFunctionDef.hpp"

#include <QtCore/QSettings>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMenu>
#include <QtWidgets/QSizeGrip>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QFileDialog>
//=============================================================================
const QString SETTING_WORKSPACE_BROWSER_VISIBILITY = "wb_Visibility";
const QString SETTING_WORKSPACE_BROWSER_GEOMETRY = "wb_Geometry";
const QString SETTING_WORKSPACE_BROWSER_HEADER_ORDER = "wb_Header_Order";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SORT = "wb_Header_Sort";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER = "wb_Header_Sort_Order";
const QString SETTING_WORKSPACE_BROWSER_HEADER_SIZES = "wb_Header_Sizes";
//=============================================================================
using namespace Nelson;
//=============================================================================
QtWorkspaceBrowser::QtWorkspaceBrowser(QWidget* parent)
    : m_context(nullptr), QDockWidget(TR("Workspace Browser"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    QVBoxLayout* mainLayout = new QVBoxLayout;

    m_tableWidget = new QTableWidget;
    m_tableWidget->setEditTriggers(QAbstractItemView::NoEditTriggers);
    m_tableWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    m_tableWidget->setSortingEnabled(true);
    m_tableWidget->verticalHeader()->setVisible(false);

    mainLayout->addWidget(m_tableWidget);
    setLayout(mainLayout);

    QHeaderView* header = m_tableWidget->horizontalHeader();
    header->resizeSection(0, 64);

    // Set the context menu for the header
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    header->setSectionsMovable(true);

    connect(header, SIGNAL(customContextMenuRequested(const QPoint&)), this,
        SLOT(showHeaderContextMenu(const QPoint&)));

    setFocusPolicy(Qt::StrongFocus);
    sizeGrip = new QSizeGrip(this);
    mainLayout->addWidget(sizeGrip, 0, Qt::AlignBottom | Qt::AlignRight);

    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(mainLayout);
    setWidget(centralWidget);

    setObjectName("workspaceBrowser");
    setMinimumSize(50, 50);
    connect(this, &QDockWidget::dockLocationChanged, this,
        &QtWorkspaceBrowser::handleDockLocationChanged);
}
//=============================================================================
void
QtWorkspaceBrowser::setContext(Context* context)
{
    m_context = context;
}
//=============================================================================
void
QtWorkspaceBrowser::restorePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    bool savedVisibility = settings.value(SETTING_WORKSPACE_BROWSER_VISIBILITY, true).toBool();
    setVisible(savedVisibility);
    restoreGeometry(settings.value(SETTING_WORKSPACE_BROWSER_GEOMETRY).toByteArray());
    QByteArray headerOrderData
        = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_ORDER).toByteArray();
    m_tableWidget->horizontalHeader()->restoreState(headerOrderData);
    int sortColumn = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_SORT, 0).toInt();
    Qt::SortOrder sortOrder
        = static_cast<Qt::SortOrder>(settings
                                         .value(SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER,
                                             static_cast<int>(Qt::AscendingOrder))
                                         .toInt());

    m_tableWidget->sortByColumn(sortColumn, sortOrder);
    QByteArray headerSizesData
        = settings.value(SETTING_WORKSPACE_BROWSER_HEADER_SIZES).toByteArray();
    m_tableWidget->horizontalHeader()->restoreState(headerSizesData);
}
//=============================================================================
void
QtWorkspaceBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_WORKSPACE_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_WORKSPACE_BROWSER_GEOMETRY, saveGeometry());

    settings.setValue(
        SETTING_WORKSPACE_BROWSER_HEADER_ORDER, m_tableWidget->horizontalHeader()->saveState());
    settings.setValue(SETTING_WORKSPACE_BROWSER_HEADER_SORT,
        m_tableWidget->horizontalHeader()->sortIndicatorSection());
    settings.setValue(SETTING_WORKSPACE_BROWSER_HEADER_SORT_ORDER,
        static_cast<int>(m_tableWidget->horizontalHeader()->sortIndicatorOrder()));
    settings.setValue(
        SETTING_WORKSPACE_BROWSER_HEADER_SIZES, m_tableWidget->horizontalHeader()->saveState());
}
//=============================================================================
void
QtWorkspaceBrowser::doubleClicked(const QModelIndex& index)
{
}
//=============================================================================
void
QtWorkspaceBrowser::contextMenuEvent(QContextMenuEvent* event)
{
    QMenu contextMenu(this);

    QAction* deleteAction = new QAction(TR("Delete"), this);
    connect(deleteAction, &QAction::triggered, this, &QtWorkspaceBrowser::onDeleteAction);
    contextMenu.addAction(deleteAction);

    QAction* renameAction = new QAction(TR("Rename"), this);
    connect(renameAction, &QAction::triggered, this, &QtWorkspaceBrowser::onRenameAction);
    contextMenu.addAction(renameAction);

    QAction* saveAsAction = new QAction(TR("Save As"), this);
    connect(saveAsAction, &QAction::triggered, this, &QtWorkspaceBrowser::onSaveAsAction);
    contextMenu.addAction(saveAsAction);

    contextMenu.addSeparator();

    QString variableName = getCurrentVariableNameSelected();
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
    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QtWorkspaceBrowser::updateVariables()
{
    if (!m_context) {
        return;
    }
    wstringVector variablesList;
    m_context->getCurrentScope()->getVariablesList(false, variablesList);
    wstringVector globalVariablesList;
    m_context->getGlobalScope()->getVariablesList(false, globalVariablesList);
    variablesList.insert(
        variablesList.end(), globalVariablesList.begin(), globalVariablesList.end());
    std::sort(variablesList.begin(), variablesList.end());
    auto last = std::unique(variablesList.begin(), variablesList.end());
    variablesList.erase(last, variablesList.end());

    m_tableWidget->setRowCount((int)variablesList.size());
    m_tableWidget->setColumnCount(5);
    m_tableWidget->setHorizontalHeaderLabels(
        QStringList() << TR("Name") << TR("Value") << TR("Class") << TR("Size") << TR("Scope"));

    for (int i = 0; i < variablesList.size(); i++) {
        bool isPersistent = m_context->isVariablePersistent(wstring_to_utf8(variablesList[i]));
        bool isGlobal = m_context->isVariableGlobal(wstring_to_utf8(variablesList[i]));
        ArrayOf* variable = m_context->lookupVariable(wstring_to_utf8(variablesList[i]));

        QString variableName(wstringToQString(variablesList[i]));
        std::wstring variableClassname = ClassToStringW(variable->getDataClass());
        if (variable->getDataClass() == NLS_HANDLE) {
            variableClassname = utf8_to_wstring(variable->getHandleCategory());
        }
        if (variable->getDataClass() == NLS_CLASS_ARRAY) {
            variableClassname = utf8_to_wstring(variable->getClassType());
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
        QString size(wstringToQString(variable->getDimensions().toWideString()));

        m_tableWidget->setItem(i, 0, new QTableWidgetItem(variableName));
        m_tableWidget->setItem(i, 1, new QTableWidgetItem(value));
        m_tableWidget->setItem(i, 2, new QTableWidgetItem(type));
        m_tableWidget->setItem(i, 3, new QTableWidgetItem(size));
        m_tableWidget->setItem(i, 4, new QTableWidgetItem(flags));
    }
}
//=============================================================================
void
QtWorkspaceBrowser::showHeaderContextMenu(const QPoint& pos)
{
    QHeaderView* header = m_tableWidget->horizontalHeader();

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

    switch (variable->getDataClass()) {
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
    } else if (variable->isScalar()) {
        T* ptr = (T*)variable->getDataPointer();
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
        T* ptr = (T*)variable->getDataPointer();
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
        return wstringToQString(variable->getContentAsLogicalScalar() ? L"true" : L"false");
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleCharVariable(const ArrayOf* variable) const
{
    if (variable->isScalar()) {
        return "''";
    } else {
        return wstringToQString(L"\'" + variable->getContentAsWideString() + L"\'");
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
        return wstringToQString(L"\"" + variable->getContentAsWideString() + L"\"");
    } else {
        return handleDefaultCase(variable);
    }
}
//=============================================================================
QString
QtWorkspaceBrowser::handleFunctionHandle(const ArrayOf* variable) const
{
    if (variable->isScalar()) {
        function_handle fh = variable->getContentAsFunctionHandle();
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
    NelsonType valueType = variable->getDataClass();
    std::wstring className = ClassToStringW(valueType);
    if (valueType == NLS_HANDLE) {
        className = utf8_to_wstring(variable->getHandleCategory());
    }
    if (valueType == NLS_CLASS_ARRAY) {
        className = utf8_to_wstring(variable->getClassType());
    }
    std::wstring variableClassname = (variable->isSparse()) ? L"sparse " + className : className;
    return wstringToQString(variable->getDimensions().toWideString()) + " "
        + wstringToQString(variableClassname);
}
//=============================================================================
QString
QtWorkspaceBrowser::getCurrentVariableNameSelected()
{
    QString name;
    QModelIndexList selectedIndexes = m_tableWidget->selectionModel()->selectedIndexes();

    if (!selectedIndexes.isEmpty()) {
        int nameColumn = -1;
        for (int i = 0; i < m_tableWidget->columnCount(); ++i) {
            QString headerText = m_tableWidget->horizontalHeaderItem(i)->text();
            if (headerText == TR("Name")) {
                nameColumn = i;
                break;
            }
        }
        int selectedRow = selectedIndexes.first().row();
        QTableWidgetItem* nameItem = m_tableWidget->item(selectedRow, nameColumn);
        if (nameItem) {
            name = nameItem->text();
        }
    }
    return name;
}
//=============================================================================
void
QtWorkspaceBrowser::onDeleteAction()
{
    QString name = getCurrentVariableNameSelected();
    if (!name.isEmpty()) {
        QString command = QString("clear('%1');").arg(name);
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
            QString command = QString("%2 = %1;clear('%1');").arg(oldName, newName);
            emit postCommand(command);
        }
    }
}
//=============================================================================
void
QtWorkspaceBrowser::onSaveAsAction()
{
    QString variableName = getCurrentVariableNameSelected();
    if (!variableName.isEmpty()) {
        QString newFileName = QFileDialog::getSaveFileName(
            this, TR("Save Variable As"), variableName, TR("Nelson Files (*.nh5)"));

        if (!newFileName.isEmpty()) {
            QString command = QString("save('%2','%1');").arg(variableName, newFileName);
            emit postCommand(command);
        }
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
QtWorkspaceBrowser::handleDockLocationChanged(Qt::DockWidgetArea area)
{
    sizeGrip->setVisible(area == Qt::NoDockWidgetArea);
}
//=============================================================================
void
QtWorkspaceBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeWorkspaceBrowser();
}
//=============================================================================
