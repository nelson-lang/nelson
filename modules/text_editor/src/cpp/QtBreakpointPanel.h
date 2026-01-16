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
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QPushButton>
#include "Evaluator.hpp"
//=============================================================================
class QtBreakpointPanel : public QWidget
{
    Q_OBJECT

public:
    explicit QtBreakpointPanel(Nelson::Evaluator* eval, QWidget* parent = nullptr);
    ~QtBreakpointPanel() override = default;

    void
    refreshBreakpoints();
    void
    clear();

signals:
    void
    breakpointClicked(const QString& filename, int line);
    void
    breakpointRemoved(const QString& filename, int line);
    void
    breakpointsChanged();

private slots:
    void
    onCellClicked(int row, int column);
    void
    onCellChanged(int row, int column);
    void
    onRemoveSelected();
    void
    onRemoveAll();

private:
    QTableWidget* breakpointTable;
    Nelson::Evaluator* nlsEvaluator;
    QPushButton* removeButton;
    QPushButton* removeAllButton;
    bool updatingTable = false;
};
//=============================================================================
