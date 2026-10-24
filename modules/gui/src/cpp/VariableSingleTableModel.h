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
#include "VariableTypedTableModel.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Thin concrete model for single / single-complex arrays. All behaviour lives
// in VariableTypedTableModel<single>; this subclass exists only to carry the
// Q_OBJECT that moc requires (templates cannot host Q_OBJECT).
class VariableSingleTableModel : public VariableTypedTableModel<single>
{
    Q_OBJECT

public:
    explicit VariableSingleTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableSingleTableModel() override = default;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
