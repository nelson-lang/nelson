//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtDebugStackPanel.h"
#include <QtWidgets/QLabel>
#include <QtCore/QFileInfo>
#include "DebugStack.hpp"
#include "QStringConverter.hpp"
//=============================================================================
QtDebugStackPanel::QtDebugStackPanel(Nelson::Evaluator* eval, QWidget* parent)
    : QWidget(parent), nlsEvaluator(eval)
{
    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(2, 2, 2, 2);
    layout->setSpacing(2);

    QLabel* titleLabel = new QLabel(tr("Call Stack"), this);
    QFont titleFont = titleLabel->font();
    titleFont.setBold(true);
    titleLabel->setFont(titleFont);
    layout->addWidget(titleLabel);

    stackList = new QListWidget(this);
    stackList->setAlternatingRowColors(true);
    layout->addWidget(stackList);

    connect(stackList, &QListWidget::itemClicked, this, &QtDebugStackPanel::onItemClicked);
}
//=============================================================================
void
QtDebugStackPanel::refreshStack()
{
    stackList->clear();

    if (!nlsEvaluator || !nlsEvaluator->isBreakpointActive()) {
        return;
    }

    // Get call stack from evaluator and convert to stackTrace
    Nelson::stackTrace positions;
    Nelson::DebugStack(nlsEvaluator->callstack, 0, positions);

    // Iterate through stack frames (newest to oldest)
    for (int i = static_cast<int>(positions.size()) - 1; i >= 0; --i) {
        const Nelson::PositionScript& pos = positions[static_cast<size_t>(i)];

        std::wstring filename = pos.getFilename();
        std::wstring functionName = pos.getFunctionName();
        int line = pos.getLine();

        QListWidgetItem* item = new QListWidgetItem(stackList);

        QString filenameStr = Nelson::wstringToQString(filename);
        QString functionStr = Nelson::wstringToQString(functionName);

        // Format: "In functionName (file.m:42)"
        QString displayText;
        if (!functionStr.isEmpty() && !filenameStr.isEmpty() && line > 0) {
            displayText = QString("In %1 (%2:%3)")
                              .arg(functionStr)
                              .arg(QFileInfo(filenameStr).fileName())
                              .arg(line);
        } else if (!functionStr.isEmpty() && !filenameStr.isEmpty()) {
            displayText
                = QString("In %1 (%2)").arg(functionStr).arg(QFileInfo(filenameStr).fileName());
        } else if (!functionStr.isEmpty()) {
            displayText = QString("In %1").arg(functionStr);
        } else {
            displayText = QString("<unknown>");
        }

        item->setText(displayText);

        if (!filenameStr.isEmpty() && line > 0) {
            item->setToolTip(QString("%1:%2").arg(filenameStr).arg(line));

            // Store full filename and line as user data
            StackFrameData* frameData = new StackFrameData { filenameStr, line };
            item->setData(Qt::UserRole, QVariant::fromValue(reinterpret_cast<quintptr>(frameData)));
        } else {
            item->setData(Qt::UserRole, QVariant::fromValue(quintptr(0)));
        }

        stackList->addItem(item);
    }
}
//=============================================================================
void
QtDebugStackPanel::clear()
{
    // Clean up user data before clearing
    for (int i = 0; i < stackList->count(); ++i) {
        QListWidgetItem* item = stackList->item(i);
        quintptr dataPtr = item->data(Qt::UserRole).value<quintptr>();
        if (dataPtr) {
            delete reinterpret_cast<StackFrameData*>(dataPtr);
        }
    }
    stackList->clear();
}
//=============================================================================
void
QtDebugStackPanel::onItemClicked(QListWidgetItem* item)
{
    if (!item) {
        return;
    }

    quintptr dataPtr = item->data(Qt::UserRole).value<quintptr>();
    if (dataPtr) {
        StackFrameData* frameData = reinterpret_cast<StackFrameData*>(dataPtr);
        emit frameClicked(frameData->filename, frameData->line);
    }
}
//=============================================================================
