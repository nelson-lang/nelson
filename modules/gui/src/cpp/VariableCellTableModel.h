//=============================================================================
// Copyright (c) 2025-present Allan CORNET (Nelson)
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
class VariableCellTableModel : public VariableAbstractTableModel
{
    Q_OBJECT

public:
    explicit VariableCellTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableCellTableModel() override = default;

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

    void
    refreshFromArray(ArrayOf& value) override;
    bool
    isStructureCompatible(const ArrayOf& value) override;
    void
    forceViewUpdate();

    QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const override;
    bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text) override;
    bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text) override;

    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const override;

    ArrayOf
    createArrayFromSelection(const QModelIndexList& selectedIndexes) const override;
    bool
    isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const override;

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

    bool
    expandDisplayIfNeeded(int row, int col);

    bool
    isCellScalarOrEditable(int row, int col) const;

private:
    void
    initializeDimensions();
    void
    initializeEditData();
    void
    rebuildModelFromCurrentArray() override;

    QVariant
    getDisplayValue(int row, int col) const;
    bool
    hasOriginalData() const;
    bool
    isValidPosition(int row, int col) const;

    bool
    resizeArrayIfNeeded(int row, int col);
    bool
    setArrayMissingValue(int row, int col);
    bool
    setArrayValue(int row, int col, const std::wstring& value);
    void
    updateEditData(int row, int col, const QVariant& value);

    void
    persistChangesToContext() override;
    void
    schedulePersistenceUpdate();
    void
    performBatchedPersistence();

    struct SelectionBounds
    {
        int minRow, maxRow, minCol, maxCol;
        bool isValid = false;
    };
    SelectionBounds
    getSelectionBounds(const QModelIndexList& selectedIndexes) const;

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

    // Extra rows/columns kept available for editing beyond the array bounds.
    // Additive (not multiplicative) so that large arrays do not explode memory.
    static constexpr int DISPLAY_MARGIN = 10;

    QVariant
    getCellDisplayValue(int row, int col) const;
    QVariant
    getCellEditValue(int row, int col) const;

    bool
    setCellValue(int row, int col, const ArrayOf& cellValue);
    QVariant
    formatCellForDisplay(const QVariant& cellData) const;
    QVariant
    formatCellForDisplay(const ArrayOf& cellArray) const;

    ArrayOf
    parseInputToArrayOf(const QString& input, int row, int col) const;
    QVariant
    formatCellForEdit(const ArrayOf& cellArray) const;

    QVariant
    formatCellForEdit(const QVariant& cellData) const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
