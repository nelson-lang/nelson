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

public:
    QtTextIndent();
    ~QtTextIndent() override;
    void
    setDocument(QtTextEdit* te);
    [[nodiscard]] QtTextEdit*
    document() const;
private slots:
    void
    update();
};
//=============================================================================
