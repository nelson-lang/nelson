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

#include <QtCore/QSettings>

class VariableTableModel : public QAbstractTableModel
{
public:
    VariableTableModel(ArrayOf array, QObject* parent = nullptr)
        : QAbstractTableModel(parent), m_array(std::move(array))
    {
        m_rows = static_cast<int>(m_array.getRows());
        m_cols = static_cast<int>(m_array.getColumns());
    }

    int
    rowCount(const QModelIndex& = QModelIndex()) const override
    {
        return m_rows;
    }
    int
    columnCount(const QModelIndex& = QModelIndex()) const override
    {
        return m_cols;
    }

    QVariant
    data(const QModelIndex& index, int role) const override
    {
        if (!index.isValid() || role != Qt::DisplayRole)
            return QVariant();

        if (m_array.getDataClass() == NLS_DOUBLE) {
            double* ptr = (double*)(m_array.getDataPointer());
            double val = ptr[index.row() + index.column() * m_rows];
            return val;
        }

        return QVariant("?");
    }

    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override
    {
        if (!index.isValid() || role != Qt::EditRole)
            return false;

        if (m_array.getDataClass() != NLS_DOUBLE)
            return false;

        bool ok = false;
        double dval = value.toDouble(&ok);
        if (!ok) {
            return false; 
        }
        double* ptr = (double*)(m_array.getDataPointer());
        ptr[index.row() + index.column() * m_rows] = dval;

        emit dataChanged(index, index); // Met à jour la vue
        return true;
    }

    Qt::ItemFlags
    flags(const QModelIndex& index) const override
    {
        if (!index.isValid())
            return Qt::NoItemFlags;
        return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
    }

private:
    ArrayOf m_array;
    int m_rows = 0;
    int m_cols = 0;
};

//=============================================================================
const QString SETTING_VARIABLES_EDITOR_VISIBILITY = "ve_Visibility";
const QString SETTING_VARIABLES_EDITOR_GEOMETRY = "ve_Geometry";
//=============================================================================
QtVariablesEditor::QtVariablesEditor(QWidget* parent)
    : m_context(nullptr), QDockWidget(TR("Variables Editor"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    setupUI();
}
//=============================================================================
QtVariablesEditor::~QtVariablesEditor() { }
//=============================================================================
void
QtVariablesEditor::setContext(Context* context)
{
    m_context = context;
}
//=============================================================================
void
QtVariablesEditor::setupUI()
{
    m_tabWidget = new QTabWidget(this);
    m_tabWidget->setTabsClosable(true);
    m_tabWidget->setMovable(true);
    connect(m_tabWidget, &QTabWidget::tabCloseRequested, this, &QtVariablesEditor::closeTab);
    setWidget(m_tabWidget);
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
QtVariablesEditor::restoreVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    bool savedVisibility = settings.value(SETTING_VARIABLES_EDITOR_VISIBILITY, true).toBool();
    setVisible(savedVisibility);
}
//=============================================================================
void
QtVariablesEditor::savePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_VARIABLES_EDITOR_VISIBILITY, isVisible());
    settings.setValue(SETTING_VARIABLES_EDITOR_GEOMETRY, saveGeometry());
}
//=============================================================================
void
QtVariablesEditor::updateVariables()
{
}
//=============================================================================
bool
QtVariablesEditor::openVariable(const QString& variableName)
{
    if (m_openedVariables.contains(variableName)) {
        m_tabWidget->setCurrentWidget(m_openedVariables[variableName]);
        return true;
    }

    ArrayOf value;
    bool found = m_context->getBaseScope()->lookupVariable(
        wstring_to_utf8(QStringTowstring(variableName)), value);
    if (!found) {
        return false;
    }

    Dimensions dimensions = value.getDimensions();
    if (!value.is2D() && !value.isEmpty()) {
        return false;
    }

    QTableView* tableView = new QTableView();
    tableView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    VariableTableModel* model = new VariableTableModel(value, tableView);
    tableView->setModel(model);

    m_tabWidget->addTab(tableView, variableName);
    m_tabWidget->setCurrentWidget(tableView);
    m_openedVariables.insert(variableName, tableView);

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

    QDockWidget::closeEvent(event);
}
//=============================================================================
