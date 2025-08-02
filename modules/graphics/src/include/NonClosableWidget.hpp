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
#include <QtGui/QCloseEvent>
//=============================================================================
class NonClosableWidget : public QWidget
{
public:
    NonClosableWidget() : QWidget(nullptr, Qt::FramelessWindowHint) {};
    void
    closeEvent(QCloseEvent* ce) override;
};
//=============================================================================
