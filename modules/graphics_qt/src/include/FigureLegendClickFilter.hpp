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
#include <QtCore/QTimer>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QApplication>
#include <QtWidgets/QToolButton>
#include <functional>
//=============================================================================
namespace Nelson {
class FigureLegendClickFilter : public QObject
{
public:
    FigureLegendClickFilter(QToolButton* button, std::function<void()> isolate)
        : QObject(button), button(button), isolate(std::move(isolate))
    {
        timer.setSingleShot(true);
        connect(&timer, &QTimer::timeout, button, [button]() {
            button->setDown(false);
            button->click();
        });
        button->installEventFilter(this);
    }

protected:
    bool
    eventFilter(QObject*, QEvent* event) override
    {
        if (event->type() == QEvent::MouseButtonDblClick
            && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton) {
            timer.stop();
            doubleClick = true;
            button->setDown(false);
            isolate();
            return true;
        }
        if (event->type() == QEvent::MouseButtonRelease
            && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton) {
            button->setDown(false);
            if (!doubleClick) {
                timer.start(QApplication::doubleClickInterval());
            }
            doubleClick = false;
            return true;
        }
        return false;
    }

private:
    QToolButton* button;
    std::function<void()> isolate;
    QTimer timer;
    bool doubleClick = false;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
