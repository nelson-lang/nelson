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
#include "Context.hpp"
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "characters_encoding.hpp"
#include "VariableAbstractTableModel.h"
#include "QStringConverter.hpp"
#include "WorkspaceBrowser.hpp"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QVector>
#include <QtCore/QString>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtCore/QSize>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
class VariableSingleTableModel : public VariableAbstractTableModel
{
    Q_OBJECT

public:
    explicit VariableSingleTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableSingleTableModel() override = default;

    // QAbstractTableModel interface
    int
    rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int
    columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant
    data(const QModelIndex& index, int role) const override;
    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override;
    Qt::ItemFlags
    flags(const QModelIndex& index) const override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role) const override;

    // Array management
    void
    refreshFromArray(ArrayOf& value) override;
    bool
    isStructureCompatible(const ArrayOf& value) override;
    void
    forceViewUpdate();

    // Selection and clipboard operations - MOVED FROM QtVariablesEditor
    QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const override;
    bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text) override;
    bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text) override;

    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const override;

    // Variable creation from selection - MOVED FROM QtVariablesEditor
    ArrayOf
    createArrayFromSelection(const QModelIndexList& selectedIndexes) const override;
    bool
    isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const override;

    // Row/Column operations - MOVED FROM QtVariablesEditor
    bool
    insertRowAt(int row) override;
    bool
    insertColumnAt(int col) override;
    bool
    deleteRowAt(int row) override;
    bool
    deleteColumnAt(int col) override;

    bool
    shouldDisplayAsTable() const;

    void
    redo() override;
    void
    undo() override;

signals:
    void
    modelChanged();
    void
    errorOccurred(const QString& message);
    void
    selectionCreated(const QString& suggestedName, const ArrayOf& newArray);

private:
    // Core data management
    void
    initializeDimensions();
    void
    initializeEditData();
    bool
    validateArrayState() const;

    // Display and editing
    QVariant
    getDisplayValue(int row, int col) const;
    bool
    hasOriginalData() const;
    bool
    isValidPosition(int row, int col) const;

    // Array manipulation
    bool
    expandDisplayIfNeeded(int row, int col);
    bool
    resizeArrayIfNeeded(int row, int col);
    bool
    setArrayValue(int row, int col, ArrayOf& value);
    void
    updateEditData(int row, int col, const QVariant& value);

    // Data conversion and validation
    QString
    formatDisplayValue(std::complex<single> value) const;
    QString
    formatDisplayValue(single value) const;

    // Persistence
    void
    persistChangesToContext();
    void
    schedulePeristenceUpdate();
    void
    performBatchedPersistence();

    // Selection operations helpers - MOVED FROM QtVariablesEditor
    struct SelectionBounds
    {
        int minRow, maxRow, minCol, maxCol;
        bool isValid = false;
    };
    SelectionBounds
    getSelectionBounds(const QModelIndexList& selectedIndexes) const;

    // Enhanced paste helpers
    struct PasteData
    {
        QVector<QVector<QString>> cells;
        int rows = 0;
        int cols = 0;
        bool isValid = false;
    };
    PasteData
    parsePasteText(const QString& text) const;
    bool
    validatePasteData(const PasteData& data, int startRow, int startCol) const;
    bool
    applyPasteData(const PasteData& data, int startRow, int startCol);

    // Utilities
    int
    indexToFlat(int row, int col) const;
    std::pair<int, int>
    flatToIndex(int flatIndex) const;
    bool
    expandArrayIfNeeded(int row, int col);
    bool
    expandDisplay(int row, int col);
    bool
    resizeArray(int row, int col);
    void
    resizeEditData(int oldRows, int oldCols);

    // Member variables
    int m_rows = 0;
    int m_cols = 0;
    int m_rows_display = 0;
    int m_cols_display = 0;
    QVector<QVariant> m_editData;
    QTimer* m_persistenceTimer = nullptr;
    bool m_pendingChanges = false;

    // Configuration
    static constexpr int MIN_DISPLAY_SIZE = 10;
    static constexpr int DEFAULT_EMPTY_SIZE = 10;
    static constexpr int MAX_PASTE_SIZE = 10000;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
