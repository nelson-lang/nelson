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
#include "VariableDefaultModel.h"
#include <QtWidgets/QTextEdit>
#include <QtCore/QPointer>
//=============================================================================
namespace Nelson {
//=============================================================================
class QtVariableTextEdit : public QTextEdit
{
    Q_OBJECT
    //=============================================================================
public:
    explicit QtVariableTextEdit(const QString& variableName, ArrayOf array, Evaluator* evaluator,
        QWidget* parent = nullptr);
    ~QtVariableTextEdit() override = default;

    VariableDefaultModel*
    model() const
    {
        return m_model;
    }

    void
    refreshFromArray(const ArrayOf& value);
    bool
    isStructureCompatible(const ArrayOf& value) const;
    QString
    getVariableClassAsString() const;
    QString
    getVariableDimensionsAsString() const;

    QString
    variableName() const;
    const ArrayOf&
    array() const;
    //=============================================================================
signals:
    void
    modelChanged();
    void
    errorOccurred(const QString& message);
    void
    variableModified(const QString& variableName);
    //=============================================================================
protected:
    void
    showEvent(QShowEvent* event) override;
    //=============================================================================
private slots:
    void
    onModelContentChanged();
    void
    onModelError(const QString& message);
    void
    onModelVariableModified(const QString& variableName);
    //=============================================================================
private:
    QPointer<VariableDefaultModel> m_model;

    void
    setupConnections();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
