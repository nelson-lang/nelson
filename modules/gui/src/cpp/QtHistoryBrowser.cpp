//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include <QtWidgets/QSizeGrip>
#include <QtGui/QContextMenuEvent>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QSettings>

#include "QtHistoryBrowser.h"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
#include "Nelson_VERSION.h"
#include "HistoryBrowser.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "QtTerminal.h"
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
    m_listWidget = new QListWidget;
    layout->addWidget(m_listWidget);
    setLayout(layout);
    m_listWidget->setSelectionMode(QAbstractItemView::ExtendedSelection);
    setFocusPolicy(Qt::StrongFocus);
    sizeGrip = new QSizeGrip(this);
    layout->addWidget(sizeGrip, 0, Qt::AlignBottom | Qt::AlignRight);
    QWidget* centralWidget = new QWidget(this);
    centralWidget->setLayout(layout);
    setWidget(centralWidget);

    createContextMenu();

    connect(m_listWidget, &QListWidget::itemDoubleClicked, this, &QtHistoryBrowser::doubleClicked);
    connect(this, &QDockWidget::dockLocationChanged, this,
        &QtHistoryBrowser::handleDockLocationChanged);
}
//=============================================================================
void
QtHistoryBrowser::createContextMenu()
{
    m_popup = new QMenu;
    m_execute = new QAction(TR("Execute selection"), this);
    connect(m_execute, SIGNAL(triggered()), this, SLOT(execute()));

    m_clearall = new QAction(TR("Clear all"), this);
    connect(m_clearall, SIGNAL(triggered()), this, SLOT(clearAll()));

    m_delete = new QAction(TR("Delete selection"), this);
    connect(m_delete, SIGNAL(triggered()), this, SLOT(deleteItem()));

    m_copy = new QAction(TR("Copy selection"), this);
    m_copy->setShortcut(QKeySequence::Copy);
    connect(m_copy, SIGNAL(triggered()), this, SLOT(copy()));

    m_toEditor = new QAction(TR("Create Script"), this);
    connect(m_toEditor, SIGNAL(triggered()), this, SLOT(toTextEditor()));

    m_popup->addAction(m_execute);
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
        m_copy->setEnabled(items.size() > 0);
        m_popup->exec(event->globalPos());
    }
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
QtHistoryBrowser::addCommand(const QString& text)
{
    m_listWidget->clearSelection();
    QStringList lines = splitOnCarriageReturnAndRemoveIt(text);
    for (auto line : lines) {
        if ((m_listWidget->count() > 0)
            && (line == m_listWidget->item(m_listWidget->count() - 1)->text())) {
            return;
        }
        if (line.startsWith("%% --") && line.endsWith("-- %%")) {
            QColor color(37, 150, 190);
            QBrush brush(color);
            auto item = new QListWidgetItem(line, m_listWidget);
            item->setForeground(brush);
        } else {
            new QListWidgetItem(line, m_listWidget);
        }
    }
    m_listWidget->setCurrentRow(m_listWidget->count() - 1);
    m_listWidget->scrollToBottom();
}
//=============================================================================
void
QtHistoryBrowser::doubleClicked(QListWidgetItem* item)
{
    emit sendCommands(QStringList(item->text()));
}
//=============================================================================
void
QtHistoryBrowser::execute()
{
    QList<QListWidgetItem*> items = m_listWidget->selectedItems();
    QStringList commands;
    if (items.size() > 0) {
        for (int i = 0; i < items.size(); i++) {
            commands.push_back(items[i]->text());
        }
        emit sendCommands(commands);
    }
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
    QList<QListWidgetItem*> selectedItems = m_listWidget->selectedItems();

    for (QListWidgetItem* selectedItem : selectedItems) {
        text += QStringTowstring(selectedItem->text()) + L"\n";
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
    msgBox.setInformativeText(TR("Please note that this action cannot be undone."));
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
            for (auto line : lines) {
                this->addCommand(Nelson::wstringToQString(line));
            }
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
QtHistoryBrowser::restorePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    bool savedVisibility = settings.value(SETTING_HISTORY_BROWSER_VISIBILITY, true).toBool();
    setVisible(savedVisibility);

    restoreGeometry(settings.value(SETTING_HISTORY_BROWSER_GEOMETRY).toByteArray());
}
//=============================================================================
void
QtHistoryBrowser::savePositionAndVisibility()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_HISTORY_BROWSER_VISIBILITY, isVisible());
    settings.setValue(SETTING_HISTORY_BROWSER_GEOMETRY, saveGeometry());
}
//=============================================================================
void
QtHistoryBrowser::handleDockLocationChanged(Qt::DockWidgetArea area)
{
    sizeGrip->setVisible(area == Qt::NoDockWidgetArea);
}
//=============================================================================
void
QtHistoryBrowser::closeEvent(QCloseEvent* event)
{
    event->accept();
    emit closeHistoryBrowser();
}
//=============================================================================
