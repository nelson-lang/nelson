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
#include "VariableFieldedTableModel.h"
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
class VariableStructTableModel : public VariableFieldedTableModel
{
    Q_OBJECT

public:
    explicit VariableStructTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableStructTableModel() override = default;

    Qt::ItemFlags
    flags(const QModelIndex& index) const override;
    bool
    setHeaderData(
        int section, Qt::Orientation orientation, const QVariant& value, int role) override;

    int
    columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant
    data(const QModelIndex& index, int role) const override;
    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role) const override;

    QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const override;
    bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text) override;

    ArrayOf
    createArrayFromSelection(const QModelIndexList& selectedIndexes) const override;

    bool
    insertRowAt(int row) override;
    bool
    insertColumnAt(int col) override;
    bool
    deleteRowAt(int row) override;
    bool
    deleteColumnAt(int col) override;

    bool
    expandDisplayIfNeeded(int row, int col) override;

private:
    // Fielded-model hooks (struct field access).
    void
    initializeDimensions() override;
    bool
    isCompatibleArrayType(const ArrayOf& array) const override;
    stringVector
    rawFieldNames(const ArrayOf& array) const override;
    ArrayOf
    fieldValue(int structIndex, int fieldIndex) const override;
    bool
    expandArrayIfNeeded(int row, int col) override;
    bool
    writePastedValue(int row, int col, const ArrayOf& value) override;
    QVariant
    formatStructForDisplay(const ArrayOf& cellArray) const override;
    QVariant
    formatCellForEdit(const ArrayOf& cellArray) const override;

    bool
    setArrayMissingValue(int row, int col);
    bool
    setArrayValue(int row, int col, const std::wstring& value);
    bool
    setCellValue(int row, int col, const ArrayOf& cellValue);
    bool
    setFieldValue(int structIndex, int fieldIndex, const ArrayOf& value);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
