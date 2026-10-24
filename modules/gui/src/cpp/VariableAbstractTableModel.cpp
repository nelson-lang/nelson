//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableAbstractTableModel.h"
#include <QtCore/QMetaObject>
#include <exception>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
QString
VariableAbstractTableModel::variableName() const
{
    return m_variableName;
}
//=============================================================================
const ArrayOf&
VariableAbstractTableModel::array() const
{
    return m_array;
}
//=============================================================================
NelsonType
VariableAbstractTableModel::dataClass()
{
    return m_array.dataClass();
}
//=============================================================================
Evaluator*
VariableAbstractTableModel::getEvaluator()
{
    return m_evaluator;
}
//=============================================================================
QString
VariableAbstractTableModel::getVariableClassAsString() const
{
    std::wstring classname;
    ClassName(m_array, classname);
    return wstringToQString(classname);
}
//=============================================================================
QString
VariableAbstractTableModel::getVariableDimensionsAsString() const
{
    Dimensions dims = m_array.dimensions();
    return wstringToQString(dims.toWideString());
}
//=============================================================================
void
VariableAbstractTableModel::clearUndoState()
{
    m_undoStack.clear();
    m_redoStack.clear();
}
//=============================================================================
void
VariableAbstractTableModel::saveCurrentStateForUndo()
{
    if (m_isPerformingUndo) {
        return;
    }

    try {
        ArrayOf snapshot = m_array;
        snapshot.ensureSingleOwner();
        m_undoStack.push_back(std::move(snapshot));
        while (m_undoStack.size() > MAX_UNDO_LEVELS) {
            m_undoStack.pop_front();
        }
        // Any new edit invalidates the redo history
        m_redoStack.clear();
    } catch (const std::exception&) {
        clearUndoState();
    }
}
//=============================================================================
bool
VariableAbstractTableModel::applyState(QVector<ArrayOf>& source, QVector<ArrayOf>& destination)
{
    if (source.isEmpty() || m_isPerformingUndo) {
        return false;
    }

    try {
        m_isPerformingUndo = true;

        ArrayOf currentState = m_array;
        currentState.ensureSingleOwner();

        ArrayOf restored = source.takeLast();

        beginResetModel();
        m_array = restored;
        m_array.ensureSingleOwner();
        rebuildModelFromCurrentArray();
        endResetModel();

        destination.push_back(std::move(currentState));
        while (destination.size() > MAX_UNDO_LEVELS) {
            destination.pop_front();
        }

        persistChangesToContext();
        QMetaObject::invokeMethod(this, "modelChanged", Qt::DirectConnection);

        m_isPerformingUndo = false;
        return true;
    } catch (const std::exception& e) {
        m_isPerformingUndo = false;
        QMetaObject::invokeMethod(this, "errorOccurred", Qt::DirectConnection,
            Q_ARG(QString, tr("Failed to restore previous state: %1").arg(e.what())));
        return false;
    }
}
//=============================================================================
void
VariableAbstractTableModel::redo()
{
    applyState(m_redoStack, m_undoStack);
}
//=============================================================================
void
VariableAbstractTableModel::undo()
{
    applyState(m_undoStack, m_redoStack);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
