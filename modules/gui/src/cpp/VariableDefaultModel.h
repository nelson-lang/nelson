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
#include <QtCore/QObject>
#include <QtCore/QString>
#include <QtCore/QTimer>
#include <QtGui/QTextDocument>
#include <QtWidgets/QTextEdit>
#include <memory>
//=============================================================================
namespace Nelson {
class VariableDefaultModel : public QObject
{
    Q_OBJECT

public:
    explicit VariableDefaultModel(const QString& name, ArrayOf array, Evaluator* evaluator,
        QTextEdit* textEdit, QObject* parent = nullptr);
    ~VariableDefaultModel() override = default;

    void
    refreshFromArray(const ArrayOf& value);
    bool
    isStructureCompatible(const ArrayOf& value) const;
    void
    forceViewUpdate();

    QString
    getFormattedContent() const;

    QString
    formatArrayContent(const ArrayOf& array) const;

    QString
    getVariableClassAsString() const;
    QString
    getVariableDimensionsAsString() const;
    bool
    isReadOnly() const;
    bool
    canEdit() const;

    bool
    pasteText(const QString& text, int position = -1);
    bool
    insertText(const QString& text, int position = -1);

    QString
    variableName() const
    {
        return m_variableName;
    }
    const ArrayOf&
    array() const
    {
        return m_array;
    }
    NelsonType
    dataClass() const
    {
        return m_array.dataClass();
    }
    Evaluator*
    getEvaluator() const
    {
        return m_evaluator;
    }

signals:
    void
    contentChanged();
    void
    errorOccurred(const QString& message);
    void
    variableModified(const QString& variableName);

private slots:
    void
    onTextChanged();
    void
    performBatchedPersistence();

private:
    void
    initializeContent();
    void
    connectTextEditSignals();
    void
    disconnectTextEditSignals();

    void
    persistChangesToContext();
    void
    schedulePersistenceUpdate();

    ArrayOf m_array;
    QString m_variableName;
    Evaluator* m_evaluator;
    QTextEdit* m_textEdit;

    QString m_ContentAsString;
    bool m_isReadOnly = true;
    bool m_hasValidContent = true;

    QTimer* m_persistenceTimer = nullptr;
    bool m_pendingChanges = false;
    bool m_suppressSignals = false;

    static constexpr int DEFAULT_MAX_LINES = 1000;
    static constexpr int DEFAULT_MAX_LINE_LENGTH = 200;
    static constexpr int MIN_UPDATE_INTERVAL_MS = 500;
    static constexpr int MAX_CONTENT_SIZE = 1000000;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
