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
#include <QtCore/QStringList>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtCore/QSize>
#include <memory>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
// Base for the "fielded" variable models (Struct and Table): grids whose columns
// map onto named fields. It holds the field-name cache, the edit cache,
// persistence, paste and selection helpers. It has no Q_OBJECT: signals and slots
// live in VariableAbstractTableModel. Container-specific behaviour (struct field
// access vs table column access) goes through the pure virtual hooks below.
class VariableFieldedTableModel : public VariableAbstractTableModel
{
public:
    VariableFieldedTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);

    int
    rowCount(const QModelIndex& parent = QModelIndex()) const override;

    void
    refreshFromArray(ArrayOf& value) override;
    bool
    isStructureCompatible(const ArrayOf& value) override;
    void
    forceViewUpdate();

    bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text) override;
    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const override;
    bool
    isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const override;

    bool
    shouldDisplayAsTable() const;

    //=============================================================================
    // Type-specific hook: display-grid expansion differs (the Struct grid keeps
    // an extra leading "Fields" column). Public because callers expand the grid
    // directly.
    //=============================================================================
    virtual bool
    expandDisplayIfNeeded(int row, int col)
        = 0;

protected:
    void
    rebuildModelFromCurrentArray() override;
    void
    persistChangesToContext() override;

    //=============================================================================
    // Type-agnostic helpers shared by the concrete fielded models.
    //=============================================================================
    void
    initializeEditData();
    void
    updateFieldNames();

    QVariant
    getDisplayValue(int row, int col) const;
    QVariant
    getCellDisplayValue(int row, int col) const;
    QVariant
    getCellEditValue(int row, int col) const;
    bool
    hasOriginalData() const;
    bool
    isValidPosition(int row, int col) const;

    bool
    resizeArrayIfNeeded(int row, int col);
    void
    updateEditData(int row, int col, const QVariant& value);
    bool
    expandDisplay(int row, int col);
    bool
    resizeArray(int row, int col);
    void
    resizeEditData(int oldRows, int oldCols);

    void
    schedulePersistenceUpdate();
    void
    performBatchedPersistence();

    VariableAbstractTableModel::SelectionBounds
    getSelectionBounds(const QModelIndexList& selectedIndexes) const;

    VariableAbstractTableModel::PasteData
    parsePasteText(const QString& text) const;
    bool
    validatePasteData(
        const VariableAbstractTableModel::PasteData& data, int startRow, int startCol) const;
    bool
    applyPasteData(const VariableAbstractTableModel::PasteData& data, int startRow, int startCol);

    QVariant
    formatStructForDisplay(const QVariant& cellData) const;
    QVariant
    formatCellForEdit(const QVariant& cellData) const;
    ArrayOf
    parseInputToArrayOf(const QString& input, int row, int col) const;

    QString
    fieldName(int fieldIndex) const;
    int
    fieldIndex(const QString& fieldName) const;

    bool
    isValidStructArray(const ArrayOf& array) const;
    bool
    hasConsistentFields(const ArrayOf& array) const;

    int
    indexToFlat(int row, int col) const;
    std::pair<int, int>
    flatToIndex(int flatIndex) const;

    //=============================================================================
    // Type-specific hooks: implemented by each concrete fielded model. Struct
    // reads/writes struct fields; Table reads/writes table columns.
    //=============================================================================
    virtual void
    initializeDimensions()
        = 0;
    // true when the array is of the container type handled by the model.
    virtual bool
    isCompatibleArrayType(const ArrayOf& array) const
        = 0;
    // Raw (utf8) field/variable names of the array, in display order.
    virtual stringVector
    rawFieldNames(const ArrayOf& array) const
        = 0;
    // Value of the cell at (row, col) as an ArrayOf.
    virtual ArrayOf
    fieldValue(int row, int col) const
        = 0;
    // Grow the underlying array to make (row, col) addressable.
    virtual bool
    expandArrayIfNeeded(int row, int col)
        = 0;
    virtual bool
    writePastedValue(int row, int col, const ArrayOf& value)
        = 0;
    virtual QVariant
    formatStructForDisplay(const ArrayOf& cellArray) const
        = 0;
    virtual QVariant
    formatCellForEdit(const ArrayOf& cellArray) const
        = 0;

    //=============================================================================
    // Shared state: the ordered list of field/variable names backing the grid
    // columns.
    QStringList m_fieldNames;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
