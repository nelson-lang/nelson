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
#include <QtCore/QPointer>
#include <QtCore/QString>
#include <QtWidgets/QDialog>
//=============================================================================
class QAction;
class QKeyEvent;
class QLineEdit;
class QListWidget;
class QListWidgetItem;
//=============================================================================
class DesktopCommandPalette : public QDialog
{
    Q_OBJECT

public:
    explicit DesktopCommandPalette(QWidget* parent = nullptr);

    void
    addAction(QAction* action, const QString& category);
    void
    removeAction(QAction* action);
    void
    openWithQuery(const QString& query = QString());

    static int
    matchScore(const QString& text, const QString& query);

protected:
    void
    keyPressEvent(QKeyEvent* event) override;

private slots:
    void
    refreshResults();
    void
    triggerCurrent();

private:
    struct Entry
    {
        QPointer<QAction> action;
        QString category;
    };

    QAction*
    actionForItem(const QListWidgetItem* item) const;
    void
    removeDeletedActions();

    QList<Entry> m_entries;
    QLineEdit* m_search;
    QListWidget* m_results;
};
//=============================================================================
