//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "QStringConverter.hpp"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QString>
#include <QtCore/QSize>
//=============================================================================
namespace Nelson {

class VariableAbstractTableModel : public QAbstractTableModel
{
    //=============================================================================
    Q_OBJECT
    //=============================================================================
public:
    VariableAbstractTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr)
        : m_variableName(name)
        , m_array(array)
        , m_evaluator(evaluator)
        , QAbstractTableModel(parent) {};
    //=============================================================================
    ~VariableAbstractTableModel() override = default;
    //=============================================================================
    QString
    variableName() const;
    //=============================================================================
    const ArrayOf&
    array() const;
    //=============================================================================
    NelsonType
    getDataClass();
    //=============================================================================
    Evaluator*
    getEvaluator();
    //=============================================================================
    QString
    getVariableClassAsString() const;
    //=============================================================================
    QString
    getVariableDimensionsAsString() const;
    //=============================================================================
    virtual void
    redo() {};
    //=============================================================================
    virtual void
    undo() {};
    //=============================================================================
    virtual bool
    isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const
    {
        return false;
    };
    //=============================================================================
    virtual ArrayOf
    createArrayFromSelection(const QModelIndexList& selectedIndexes) const
    {
        return ArrayOf();
    }
    //=============================================================================
    virtual QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
    {
        return QString();
    }
    //=============================================================================
    virtual QSize
    getPasteSize(const QString& text) const
    {
        return QSize();
    }
    //=============================================================================
    virtual bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text)
    {
        return false;
    }
    //=============================================================================
    virtual bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text)
    {
        return false;
    }
    //=============================================================================
    virtual bool
    insertRowAt(int row)
    {
        return false;
    }
    //=============================================================================
    virtual bool
    insertColumnAt(int col)
    {
        return false;
    }
    //=============================================================================
    virtual bool
    deleteRowAt(int row)
    {
        return false;
    }
    //=============================================================================
    virtual bool
    deleteColumnAt(int col)
    {
        return false;
    }
    //=============================================================================
    virtual void
    refreshFromArray(ArrayOf& value)
    {
    }
    //=============================================================================
    virtual bool
    isStructureCompatible(const ArrayOf& value)
    {
        return false;
    }
    //=============================================================================
    bool
    canUndo() const
    {
        return m_hasUndoState && !m_isPerformingUndo;
    }
    //=============================================================================
    bool
    canRedo() const
    {
        return m_hasUndoState && !m_isPerformingUndo;
    }
    //=============================================================================
    void
    clearUndoState()
    {
        m_hasUndoState = false;
        m_undoArray = ArrayOf(); // Clear the stored array
    }
    //=============================================================================
    void
    saveCurrentStateForUndo()
    {
        if (m_isPerformingUndo) {
            return; // Don't save state during undo/redo operations
        }

        try {
            // Create a deep copy of the current array
            m_undoArray = m_array;
            m_array.ensureSingleOwner();
            m_hasUndoState = true;
        } catch (const std::exception&) {
            m_hasUndoState = false;
        }
    }
    //=============================================================================
protected:
    ArrayOf m_array;
    QString m_variableName;
    Evaluator* m_evaluator;
    //=============================================================================
    ArrayOf m_undoArray; // Stores the previous state for undo
    bool m_hasUndoState; // Flag to track if undo is available
    bool m_isPerformingUndo; // Flag to prevent saving state during undo/redo
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
