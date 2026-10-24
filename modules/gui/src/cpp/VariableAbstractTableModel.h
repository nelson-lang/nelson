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
#include <QtCore/QVector>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
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
        : QAbstractTableModel(parent)
        , m_array(array)
        , m_variableName(name)
        , m_evaluator(evaluator)
        , m_isPerformingUndo(false) { };
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
    dataClass();
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
    redo();
    //=============================================================================
    virtual void
    undo();
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
        return !m_undoStack.isEmpty() && !m_isPerformingUndo;
    }
    //=============================================================================
    bool
    canRedo() const
    {
        return !m_redoStack.isEmpty() && !m_isPerformingUndo;
    }
    //=============================================================================
    void
    clearUndoState();
    //=============================================================================
    void
    saveCurrentStateForUndo();
    //=============================================================================
signals:
    //=============================================================================
    // Emitted whenever the underlying array changed (edit, resize, paste, undo).
    void
    modelChanged();
    //=============================================================================
    // Emitted when an edit or a command failed, with a user facing message.
    void
    errorOccurred(const QString& message);
    //=============================================================================
    // Emitted when a new variable is extracted from the current selection.
    void
    selectionCreated(const QString& suggestedName, const ArrayOf& newArray);
    //=============================================================================
protected:
    virtual void
    rebuildModelFromCurrentArray()
    {
    }
    //=============================================================================
    virtual void
    persistChangesToContext()
    {
    }
    //=============================================================================
    // Rectangular bounds of a selection, shared by every model.
    struct SelectionBounds
    {
        int minRow, maxRow, minCol, maxCol;
        bool isValid = false;
    };
    //=============================================================================
    // A grid of cells parsed from clipboard / Excel text, shared by every model.
    struct PasteData
    {
        QVector<QVector<QString>> cells;
        int rows = 0;
        int cols = 0;
        bool isValid = false;
    };
    //=============================================================================
    ArrayOf m_array;
    QString m_variableName;
    Evaluator* m_evaluator;
    //=============================================================================
    // The edit cache is never typed: real data lives in m_array, display goes through QVariant.
    int m_rows = 0;
    int m_cols = 0;
    int m_rows_display = 0;
    int m_cols_display = 0;
    QVector<QVariant> m_editData;
    QTimer* m_persistenceTimer = nullptr;
    bool m_pendingChanges = false;
    //=============================================================================
    static constexpr int MIN_DISPLAY_SIZE = 10;
    static constexpr int DEFAULT_EMPTY_SIZE = 10;
    static constexpr int MAX_PASTE_SIZE = 10000;
    //=============================================================================
    // Bounded undo/redo history: m_undoStack holds the previous states (most
    // recent last), m_redoStack the states undone so far (most recent last).
    static constexpr int MAX_UNDO_LEVELS = 32;
    QVector<ArrayOf> m_undoStack;
    QVector<ArrayOf> m_redoStack;
    bool m_isPerformingUndo;
    //=============================================================================
private:
    bool
    applyState(QVector<ArrayOf>& source, QVector<ArrayOf>& destination);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
