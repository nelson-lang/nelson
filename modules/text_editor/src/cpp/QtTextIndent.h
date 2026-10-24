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
#include "QtTextEdit.h"
#include <QtCore/QObject>
//=============================================================================
class QtTextIndent : public QObject
{
    Q_OBJECT
    QtTextEdit* m_te;
    int m_indentSize;
    bool m_enabled;

public:
    QtTextIndent(QObject* parent = nullptr);
    ~QtTextIndent() override;
    void
    setDocument(QtTextEdit* te);
    [[nodiscard]] QtTextEdit*
    document() const;
    void
    setIndentSize(int indentSize);
    [[nodiscard]] int
    indentSize() const;
    // automatic indentation is only meaningful for Nelson scripts
    void
    setEnabled(bool enabled);
    [[nodiscard]] bool
    isEnabled() const;
private slots:
    void
    update();
};
//=============================================================================
