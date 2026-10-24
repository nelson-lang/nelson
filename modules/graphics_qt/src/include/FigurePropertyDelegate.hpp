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
#include <QtWidgets/QStyledItemDelegate>
//=============================================================================
namespace Nelson {
class FigurePropertyDelegate : public QStyledItemDelegate
{
public:
    using QStyledItemDelegate::QStyledItemDelegate;
    QWidget*
    createEditor(QWidget* parent, const QStyleOptionViewItem& option,
        const QModelIndex& index) const override;
    void
    setEditorData(QWidget* editor, const QModelIndex& index) const override;
    void
    setModelData(
        QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
