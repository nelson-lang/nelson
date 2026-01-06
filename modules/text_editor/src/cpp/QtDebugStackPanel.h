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
#include <QtWidgets/QWidget>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QVBoxLayout>
#include "Evaluator.hpp"
//=============================================================================
class QtDebugStackPanel : public QWidget
{
    Q_OBJECT

public:
    explicit QtDebugStackPanel(Nelson::Evaluator* eval, QWidget* parent = nullptr);
    ~QtDebugStackPanel() override = default;

    void
    refreshStack();
    void
    clear();

signals:
    void
    frameClicked(const QString& filename, int line);

private slots:
    void
    onItemClicked(QListWidgetItem* item);

private:
    QListWidget* stackList;
    Nelson::Evaluator* nlsEvaluator;

    struct StackFrameData
    {
        QString filename;
        int line;
    };
};
//=============================================================================
