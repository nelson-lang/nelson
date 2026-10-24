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
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
// Base for the numeric variable models (Double, Single, Integer, Logical,
// String): the type-agnostic table logic lives here. It has no Q_OBJECT: signals
// and slots live in VariableAbstractTableModel. Concrete models supply the typed
// behaviour through the pure virtual hooks below.
class VariableNumericTableModel : public VariableAbstractTableModel
{
public:
    VariableNumericTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);

    int
    rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int
    columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role) const override;

    void
    refreshFromArray(ArrayOf& value) override;
    void
    forceViewUpdate();

    QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const override;
    bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text) override;

    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const override;

protected:
    void
    rebuildModelFromCurrentArray() override;
    void
    persistChangesToContext() override;

    //=============================================================================
    // Typed hooks: implemented by every concrete numeric model with a uniform
    // signature. Everything above is written against these.
    //=============================================================================
    virtual bool
    resizeArray(int row, int col)
        = 0;
    virtual bool
    validatePasteData(const PasteData& data, int startRow, int startCol) const
        = 0;
    virtual bool
    applyPasteData(const PasteData& data, int startRow, int startCol)
        = 0;

    //=============================================================================
    // Type-agnostic helpers shared by the concrete models.
    //=============================================================================
    void
    initializeDimensions();
    void
    initializeEditData();
    bool
    validateArrayState() const;

    bool
    isValidPosition(int row, int col) const;

    bool
    expandDisplayIfNeeded(int row, int col);
    void
    updateEditData(int row, int col, const QVariant& value);

    SelectionBounds
    getSelectionBounds(const QModelIndexList& selectedIndexes) const;

    PasteData
    parsePasteText(const QString& text) const;

    int
    indexToFlat(int row, int col) const;
    std::pair<int, int>
    flatToIndex(int flatIndex) const;
    bool
    expandArrayIfNeeded(int row, int col);
    bool
    expandDisplay(int row, int col);
    void
    resizeEditData(int oldRows, int oldCols);

    void
    schedulePersistenceUpdate();
    void
    performBatchedPersistence();

    // Extra rows/columns kept available for editing beyond the array bounds.
    // Additive (not multiplicative) so that large arrays do not explode memory.
    static constexpr int DISPLAY_MARGIN = 10;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
