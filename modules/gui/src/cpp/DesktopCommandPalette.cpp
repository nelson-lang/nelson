//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DesktopCommandPalette.h"
#include "DesktopUiStyle.h"
#include "QtTranslation.hpp"
#include <algorithm>
#include <QtCore/QtGlobal>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QKeyEvent>
#include <QtGui/QKeySequence>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QVBoxLayout>
//=============================================================================
using namespace Nelson;
//=============================================================================
DesktopCommandPalette::DesktopCommandPalette(QWidget* parent) : QDialog(parent)
{
    setObjectName("NelsonCommandPalette");
    setAccessibleName(TR("Command search"));
    setWindowTitle(TR("Command search"));
    setWindowFlags(windowFlags() | Qt::Tool);
    resize(520, 360);

    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(10, 10, 10, 10);
    layout->setSpacing(6);

    m_search = new QLineEdit(this);
    m_search->setPlaceholderText(TR("Search commands"));
    m_search->setAccessibleName(TR("Search commands"));
    m_search->setClearButtonEnabled(true);
    connect(m_search, &QLineEdit::textChanged, this, &DesktopCommandPalette::refreshResults);
    connect(m_search, &QLineEdit::returnPressed, this, &DesktopCommandPalette::triggerCurrent);
    layout->addWidget(m_search);

    m_results = new QListWidget(this);
    m_results->setAccessibleName(TR("Command results"));
    m_results->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(m_results, &QListWidget::itemActivated, this,
        [this](QListWidgetItem*) { triggerCurrent(); });
    layout->addWidget(m_results);
}
//=============================================================================
void
DesktopCommandPalette::addAction(QAction* action, const QString& category)
{
    if (!action || action->isSeparator()) {
        return;
    }
    m_entries.append({ action, category });
    if (isVisible()) {
        refreshResults();
    }
}
//=============================================================================
void
DesktopCommandPalette::removeAction(QAction* action)
{
    for (int index = m_entries.size() - 1; index >= 0; --index) {
        if (!m_entries[index].action || m_entries[index].action == action) {
            m_entries.removeAt(index);
        }
    }
    if (isVisible()) {
        refreshResults();
    }
}
//=============================================================================
void
DesktopCommandPalette::removeDeletedActions()
{
    for (int index = m_entries.size() - 1; index >= 0; --index) {
        if (!m_entries[index].action) {
            m_entries.removeAt(index);
        }
    }
}
//=============================================================================
QAction*
DesktopCommandPalette::actionForItem(const QListWidgetItem* item) const
{
    if (!item) {
        return nullptr;
    }
    QAction* savedAction = reinterpret_cast<QAction*>(item->data(Qt::UserRole).value<quintptr>());
    if (!savedAction) {
        return nullptr;
    }
    for (const Entry& entry : m_entries) {
        QAction* action = entry.action.data();
        if (action && action == savedAction) {
            return action;
        }
    }
    return nullptr;
}
//=============================================================================
int
DesktopCommandPalette::matchScore(const QString& text, const QString& query)
{
    if (query.trimmed().isEmpty()) {
        return 1;
    }
    const QString normalizedText = text.toCaseFolded();
    const QString normalizedQuery = query.trimmed().toCaseFolded();
    if (normalizedText.startsWith(normalizedQuery)) {
        return 300 - qMin(normalizedText.size(), 200);
    }
    const int wordPosition = normalizedText.indexOf(QString(" ") + normalizedQuery);
    if (wordPosition >= 0) {
        return 200 - qMin(wordPosition, 100);
    }
    const int position = normalizedText.indexOf(normalizedQuery);
    return position >= 0 ? 100 - qMin(position, 90) : -1;
}
//=============================================================================
void
DesktopCommandPalette::openWithQuery(const QString& query)
{
    m_search->setText(query);
    refreshResults();
    show();
    raise();
    activateWindow();
    m_search->setFocus();
    m_search->selectAll();
}
//=============================================================================
void
DesktopCommandPalette::refreshResults()
{
    removeDeletedActions();
    struct Match
    {
        int score;
        QString label;
        QString category;
        QAction* action;
    };
    QList<Match> matches;
    for (const Entry& entry : m_entries) {
        QAction* action = entry.action.data();
        if (!action || !action->isVisible()) {
            continue;
        }
        const QString label = desktopAccessibleActionText(action);
        const int score = matchScore(label + QString(" ") + entry.category, m_search->text());
        if (score >= 0) {
            matches.append({ score, label, entry.category, action });
        }
    }
    std::sort(matches.begin(), matches.end(), [](const Match& left, const Match& right) {
        if (left.score != right.score) {
            return left.score > right.score;
        }
        return left.label.localeAwareCompare(right.label) < 0;
    });

    m_results->clear();
    for (const Match& match : matches) {
        QString text = match.category.isEmpty()
            ? match.label
            : QString("%1  -  %2").arg(match.label, match.category);
        if (!match.action->shortcut().isEmpty()) {
            text += QString("    %1").arg(
                match.action->shortcut().toString(QKeySequence::NativeText));
        }
        QListWidgetItem* item = new QListWidgetItem(match.action->icon(), text, m_results);
        item->setData(
            Qt::UserRole, QVariant::fromValue<quintptr>(reinterpret_cast<quintptr>(match.action)));
        item->setFlags(
            match.action->isEnabled() ? item->flags() : item->flags() & ~Qt::ItemIsEnabled);
    }
    if (m_results->count() > 0) {
        m_results->setCurrentRow(0);
    }
}
//=============================================================================
void
DesktopCommandPalette::triggerCurrent()
{
    QListWidgetItem* item = m_results->currentItem();
    if (!item || !(item->flags() & Qt::ItemIsEnabled)) {
        return;
    }
    QAction* action = actionForItem(item);
    if (!action || !action->isEnabled()) {
        refreshResults();
        return;
    }
    accept();
    action->trigger();
}
//=============================================================================
void
DesktopCommandPalette::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape) {
        reject();
        return;
    }
    if (event->key() == Qt::Key_Down && m_search->hasFocus()) {
        m_results->setFocus();
        if (m_results->currentRow() < 0 && m_results->count() > 0) {
            m_results->setCurrentRow(0);
        }
        return;
    }
    if (event->key() == Qt::Key_Up && m_search->hasFocus()) {
        m_results->setFocus();
        if (m_results->count() > 0) {
            m_results->setCurrentRow(m_results->count() - 1);
        }
        return;
    }
    QDialog::keyPressEvent(event);
}
//=============================================================================
