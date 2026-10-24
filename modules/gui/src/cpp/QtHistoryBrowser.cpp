//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include <QtGui/QClipboard>
#include <QtWidgets/QMenu>
#include <QtWidgets/QVBoxLayout>
#include <QtGui/QContextMenuEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QSettings>
#include "QtHistoryBrowser.h"
#include "QtDockTitleBar.h"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "HistoryBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "NelsonPalette.hpp"
#include "QtTerminal.h"
#include "ErrorID.hpp"
//=============================================================================
const QString SETTING_HISTORY_BROWSER_VISIBILITY = "hb_visibility";
const QString SETTING_HISTORY_BROWSER_GEOMETRY = "hb_geometry";
//=============================================================================
using namespace Nelson;
//=============================================================================
QtHistoryBrowser::QtHistoryBrowser(QWidget* parent) : QDockWidget(TR("History Browser"), parent)
{
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    setObjectName("historyBrowser");

    QVBoxLayout* layout = new QVBoxLayout;

    m_searchLineEdit = new QLineEdit(this);
    m_searchLineEdit->setClearButtonEnabled(true);
    m_searchLineEdit->setPlaceholderText(TR("Search history"));
    connect(m_searchLineEdit, &QLineEdit::textChanged, this, &QtHistoryBrowser::applySearchFilter);
    layout->addWidget(m_searchLineEdit);

    m_listWidget = new QListWidget;
    layout->addWidget(m_listWidget);
    setLayout(layout);
    m_listWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    setFocusPolicy(Qt::StrongFocus);
    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(layout);
    setWidget(centralWidget);

    setTitleBarWidget(new QtDockTitleBar(this, "hb"));

    createContextMenu();

    connect(m_listWidget, &QListWidget::itemDoubleClicked, this, &QtHistoryBrowser::doubleClicked);
}
//=============================================================================
void
QtHistoryBrowser::createContextMenu()
{
    m_popup = new QMenu(this);
    m_execute = new QAction(TR("Execute selection"), this);
    connect(m_execute, SIGNAL(triggered()), this, SLOT(execute()));

    m_clearall = new QAction(TR("Clear all"), this);
    connect(m_clearall, SIGNAL(triggered()), this, SLOT(clearAll()));

    m_delete = new QAction(TR("Delete selection"), this);
    connect(m_delete, SIGNAL(triggered()), this, SLOT(deleteItem()));

    m_copy = new QAction(TR("Copy selection"), this);
    m_copy->setShortcut(QKeySequence::Copy);
    connect(m_copy, SIGNAL(triggered()), this, SLOT(copy()));

    m_toCommandLine = new QAction(TR("Copy to Command Line"), this);
    connect(m_toCommandLine, SIGNAL(triggered()), this, SLOT(toCommandLine()));

    m_toEditor = new QAction(TR("Create Script"), this);
    connect(m_toEditor, SIGNAL(triggered()), this, SLOT(toTextEditor()));
    m_popup->addAction(m_execute);
    m_popup->addAction(m_toCommandLine);
    m_popup->addAction(m_clearall);
    m_popup->addAction(m_copy);
    m_popup->addAction(m_delete);
    m_popup->addAction(m_toEditor);
}
//=============================================================================
void
QtHistoryBrowser::contextMenuEvent(QContextMenuEvent* event)
{
    if (m_popup && event) {
        QList<QListWidgetItem*> items = m_listWidget->selectedItems();
        m_toEditor->setEnabled(items.size() > 0);
        m_execute->setEnabled(items.size() > 0);
        m_toCommandLine->setEnabled(items.size() > 0);
        m_copy->setEnabled(items.size() > 0);
        m_delete->setEnabled(items.size() > 0);
        m_toEditor->setVisible(NelsonConfiguration::getInstance()->useEmbeddedEditor());
        m_popup->exec(event->globalPos());
    }
}
//=============================================================================
static bool
isSessionHeaderLine(const QString& line)
{
    return line.startsWith("%% --") && line.endsWith("-- %%");
}
//=============================================================================
void
QtHistoryBrowser::applySearchFilter()
{
    const QString pattern = m_searchLineEdit->text().trimmed();
    for (int i = 0; i < m_listWidget->count(); ++i) {
        QListWidgetItem* item = m_listWidget->item(i);
        if (pattern.isEmpty()) {
            item->setHidden(false);
        } else {
            item->setHidden(!item->text().contains(pattern, Qt::CaseInsensitive));
        }
    }
}
//=============================================================================
QStringList
QtHistoryBrowser::selectedCommands()
{
    QStringList commands;
    QList<QListWidgetItem*> items = m_listWidget->selectedItems();
    for (QListWidgetItem* item : items) {
        // session headers ('%% -- date -- %%') are not commands
        if (isSessionHeaderLine(item->text())) {
            continue;
        }
        commands.push_back(item->text());
    }
    return commands;
}
//=============================================================================
static QStringList
splitOnCarriageReturnAndRemoveIt(const QString& t)
{
    QStringList lines;
    for (auto line : t.split('\n')) {
        if (line != "\n" && !line.isEmpty()) {
            lines.push_back(line);
        }
    }
    return lines;
}
//=============================================================================
void
QtHistoryBrowser::addCommand(const QStringList& lines)
{
    m_listWidget->clearSelection();
    if (lines.isEmpty()) {
        return;
    }
    QString previous;
    // session header color: readable with both light and dark themes
    QColor color = Nelson::isDarkPalette() ? QColor(102, 194, 232) : QColor(21, 101, 138);
    QBrush brush(color);
    for (const QString& line : lines) {
        if (line == previous) {
            continue;
        }
        previous = line;
        QListWidgetItem* item = new QListWidgetItem(line, m_listWidget);
        if (isSessionHeaderLine(line)) {
            item->setForeground(brush);
            // a session header is not a command: it must not be selectable
            item->setFlags(item->flags() & ~Qt::ItemIsSelectable);
        }
        m_listWidget->addItem(item);
    }
    m_listWidget->setCurrentRow(m_listWidget->count() - 1);
    m_listWidget->scrollToBottom();
    applySearchFilter();
}
//=============================================================================
void
QtHistoryBrowser::addCommand(const QString& text)
{
    m_listWidget->clearSelection();
    QStringList lines = splitOnCarriageReturnAndRemoveIt(text);
    addCommand(lines);
}
//=============================================================================
void
QtHistoryBrowser::doubleClicked(QListWidgetItem* item)
{
    if (!item || isSessionHeaderLine(item->text())) {
        return;
    }
    emit sendCommands(QStringList(item->text()));
}
//=============================================================================
void
QtHistoryBrowser::execute()
{
    QStringList commands = selectedCommands();
    if (commands.isEmpty()) {
        return;
    }
    if (commands.size() > 10) {
        QMessageBox msgBox(this);
        msgBox.setIcon(QMessageBox::Warning);
        msgBox.setWindowTitle(TR("Execute selection"));
        msgBox.setText(
            TR("Are you certain you wish to execute the %1 selected lines?").arg(commands.size()));
        msgBox.setInformativeText(
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:gui:pleaseNoteThatAction",
                L"Please note that this action cannot be undone.")));
        msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
        msgBox.setDefaultButton(QMessageBox::No);
        if (msgBox.exec() != QMessageBox::Yes) {
            return;
        }
    }
    emit sendCommands(commands);
}
//=============================================================================
void
QtHistoryBrowser::toCommandLine()
{
    const QStringList commands = selectedCommands();
    if (commands.isEmpty()) {
        return;
    }
    emit sendToCommandLine(commands);
}
//=============================================================================
void
QtHistoryBrowser::deleteItem()
{
    QList<QListWidgetItem*> selectedItems = m_listWidget->selectedItems();

    for (QListWidgetItem* selectedItem : selectedItems) {
        delete selectedItem;
    }
    synchronizeHistoryManager(SYNC_HB_TO_HM);
    m_listWidget->scrollToBottom();
}
//=============================================================================
void
QtHistoryBrowser::toTextEditor()
{
    std::wstring text;
    const QStringList commands = selectedCommands();

    for (const QString& command : commands) {
        text += QStringTowstring(command) + L"\n";
    }

    if (!text.empty()) {
        HistoryBrowser::setTextForTextEditor(text);
        emit sendToTextEditor();
    }
}
//=============================================================================
void
QtHistoryBrowser::clearAll()
{
    QMessageBox msgBox;

    QString fileNameIcon = Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/appointment-new.svg");
    msgBox.setWindowIcon(QPixmap(fileNameIcon));
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setText(TR("Are you certain you wish to clear the history?"));
    msgBox.setInformativeText(wstringToQString(Nelson::resolveMessageTemplate(
        L"Nelson:gui:pleaseNoteThatAction", L"Please note that this action cannot be undone.")));
    msgBox.setWindowTitle(TR("Clear history"));
    msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
    msgBox.setDefaultButton(QMessageBox::Yes);
    if (msgBox.exec() == QMessageBox::Yes) {
        m_listWidget->clear();
        synchronizeHistoryManager(SYNC_HB_TO_HM);
        synchronizeHistoryManager(SYNC_HM_TO_HB);
    }
}
//=============================================================================
void
QtHistoryBrowser::copy()
{
    QStringList selectedItemsText;
    QList<QListWidgetItem*> selectedItems = m_listWidget->selectedItems();

    for (QListWidgetItem* selectedItem : selectedItems) {
        selectedItemsText << selectedItem->text();
    }

    if (!selectedItemsText.isEmpty()) {
        QApplication::clipboard()->setText(selectedItemsText.join("\n"));
    }
}
//=============================================================================
bool
QtHistoryBrowser::synchronizeHistoryManager(SYNC_DIRECTION direction)
{
    auto* hist = static_cast<Nelson::HistoryManager*>(
        Nelson::NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {

        switch (direction) {
        case SYNC_HB_TO_HM: {
            hist->clear(true);
            for (int i = 0; i < m_listWidget->count(); ++i) {
                hist->appendLine(Nelson::QStringTowstring(m_listWidget->item(i)->text()));
            }
            return true;
        } break;
        case SYNC_HM_TO_HB: {
            m_listWidget->clear();
            Nelson::wstringVector lines = hist->get();
            QStringList content;
            content.reserve(lines.size());
            for (auto line : lines) {
                content.push_back(Nelson::wstringToQString(line));
            }
            this->addCommand(content);
            return true;
        } break;
        default: {
        } break;
        }
    }
    return false;
}
//=============================================================================
void
QtHistoryBrowser::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter) {
        execute();
    } else if (event->key() == Qt::Key_Delete) {
        deleteItem();
    } else if (event->matches(QKeySequence::Copy)) {
        copy();
    }

    QWidget::keyPressEvent(event);
}
//=============================================================================
void
QtHistoryBrowser::restorePosition()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    restoreGeometry(settings.value(SETTING_HISTORY_BROWSER_GEOMETRY).toByteArray());
}
//=============================================================================
void
QtHistoryBrowser::restoreVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    bool savedVisibility = settings.value(SETTING_HISTORY_BROWSER_VISIBILITY, false).toBool();
    setVisible(savedVisibility);
}
//=============================================================================
void
QtHistoryBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_HISTORY_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_HISTORY_BROWSER_GEOMETRY, saveGeometry());
}
//=============================================================================
void
QtHistoryBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeHistoryBrowser();
}
//=============================================================================
