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
#include "VariableNumericTableModel.hpp"
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
class VariableIntegerTableModel : public VariableNumericTableModel
{
    Q_OBJECT

public:
    explicit VariableIntegerTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableIntegerTableModel() override = default;

    QVariant
    data(const QModelIndex& index, int role) const override;
    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override;
    Qt::ItemFlags
    flags(const QModelIndex& index) const override;

    bool
    isStructureCompatible(const ArrayOf& value) override;

    bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text) override;

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

protected:
    // Typed hooks from VariableNumericTableModel
    bool
    resizeArray(int row, int col) override;
    bool
    validatePasteData(const PasteData& data, int startRow, int startCol) const override;
    bool
    applyPasteData(const PasteData& data, int startRow, int startCol) override;

private:
    QVariant
    getDisplayValue(int row, int col) const;
    bool
    hasOriginalData() const;

    bool
    resizeArrayIfNeeded(int row, int col);
    bool
    setArrayValue(int row, int col, ArrayOf& value);

    QString
    formatDisplayValue(ArrayOf& value) const;

    NelsonType referenceType = NelsonType::NLS_UNKNOWN;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
