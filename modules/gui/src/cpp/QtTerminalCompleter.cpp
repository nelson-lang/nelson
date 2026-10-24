//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtTerminalCompleter.h"
#include "QtTerminal.h"
#include "QStringConverter.hpp"
#include "CompleterHelper.hpp"
#include "Types.hpp"
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QScrollBar>
#include <QtCore/QStringListModel>
#include <QtGui/QStandardItem>
#include <QtGui/QStandardItemModel>
#include <unordered_set>
//=============================================================================
// Roles used to carry the undecorated candidate name next to the displayed
// "name (type)" string, so that insertion never relies on text surgery.
constexpr int COMPLETION_RAW_NAME_ROLE = Qt::UserRole + 1;
constexpr int COMPLETION_IS_PATH_ROLE = Qt::UserRole + 2;
//=============================================================================
constexpr int COMPLETION_MAX_VISIBLE_ITEMS = 12;
//=============================================================================
QtTerminalCompleter::QtTerminalCompleter(QtTerminal* owner)
    : m_owner(owner), m_qCompleter(nullptr), m_model(nullptr)
{
}
//=============================================================================
QtTerminalCompleter::~QtTerminalCompleter()
{
    if (m_qCompleter) {
        delete m_qCompleter;
        m_qCompleter = nullptr;
        m_model = nullptr;
    }
}
//=============================================================================
void
QtTerminalCompleter::createIfNeeded()
{
    if (!m_qCompleter) {
        m_qCompleter = new QCompleter(m_owner);
        m_qCompleter->setWidget(m_owner);
        m_qCompleter->setModelSorting(QCompleter::UnsortedModel);
        m_qCompleter->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
        m_qCompleter->setCaseSensitivity(Qt::CaseInsensitive);
        m_qCompleter->setWrapAround(false);
        m_qCompleter->setMaxVisibleItems(COMPLETION_MAX_VISIBLE_ITEMS);
        m_model = new QStandardItemModel(m_qCompleter);
        m_qCompleter->setModel(m_model);
        // QCompleter::activated is overloaded: the QModelIndex flavour is used so the raw
        // (undecorated) candidate name can be read back from the item roles.
        QObject::connect(m_qCompleter,
            static_cast<void (QCompleter::*)(const QModelIndex&)>(&QCompleter::activated), m_owner,
            [this](const QModelIndex& index) { activateIndex(index); });
    }
}
//=============================================================================
void
QtTerminalCompleter::activateIndex(const QModelIndex& index)
{
    if (!m_owner) {
        return;
    }
    if (!index.isValid()) {
        return;
    }
    const QVariant rawName = index.data(COMPLETION_RAW_NAME_ROLE);
    if (rawName.isValid() && !rawName.toString().isEmpty()) {
        m_owner->insertCompletionItem(
            rawName.toString(), index.data(COMPLETION_IS_PATH_ROLE).toBool());
        return;
    }
    // Fallback to the decorated display string when the role is missing.
    m_owner->insertCompletion(index.data(Qt::DisplayRole).toString());
}
//=============================================================================
void
QtTerminalCompleter::complete(const QString& prefix)
{
    if (prefix.isEmpty()) {
        return;
    }

    createIfNeeded();
    if (!m_qCompleter) {
        return;
    }

    std::wstring line = QStringTowstring(prefix);
    std::wstring completionPrefix;
    wstringVector files;
    wstringVector builtin;
    wstringVector macros;
    wstringVector variables;
    wstringVector fields;
    wstringVector properties;
    wstringVector methods;

    bool showpopup = computeCompletion(
        line, completionPrefix, files, builtin, macros, variables, fields, properties, methods);

    if (showpopup) {
        updateModel(
            completionPrefix, files, builtin, macros, variables, fields, properties, methods);

        QAbstractItemView* popup = m_qCompleter->popup();
        QAbstractItemModel* model = m_qCompleter->completionModel();
        const int rows = model ? model->rowCount() : 0;

        if (popup && rows > 0) {
            QRect cr = m_owner->cursorRect();
            cr.setWidth(
                popup->sizeHintForColumn(0) + popup->verticalScrollBar()->sizeHint().width());
            // Anchor height must follow the text line, not an arbitrary constant.
            int rowHeight = popup->sizeHintForRow(0);
            if (rowHeight <= 0) {
                rowHeight = popup->fontMetrics().height();
            }
            if (cr.height() <= 0) {
                cr.setHeight(rowHeight);
            }
            m_qCompleter->complete(cr);
            m_qCompleter->setCurrentRow(0);
            m_qCompleter->popup()->setCurrentIndex(m_qCompleter->completionModel()->index(0, 0));
            popup->setVisible(true);
        } else {
            if (popup && popup->isVisible()) {
                popup->close();
            }
        }
    } else {
        QAbstractItemView* popup = m_qCompleter ? m_qCompleter->popup() : nullptr;
        if (popup && popup->isVisible()) {
            popup->close();
        }
    }
}
//=============================================================================
void
QtTerminalCompleter::updateModel(const std::wstring& prefix, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    if (m_qCompleter != nullptr && m_model != nullptr) {
        m_model->clear();
        populateModel(m_model, filesList, builtinList, macroList, variableList, fieldList,
            propertyList, methodList);
        m_qCompleter->setCompletionPrefix(wstringToQString(prefix));
    }
}
//=============================================================================
QAbstractItemModel*
QtTerminalCompleter::modelFromNelson(const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    auto* model = new QStandardItemModel(m_qCompleter);
    populateModel(model, filesList, builtinList, macroList, variableList, fieldList, propertyList,
        methodList);
    return model;
}
//=============================================================================
void
QtTerminalCompleter::populateModel(QStandardItemModel* model, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    std::unordered_set<std::wstring> uniqueNames;

    auto appendItemsWithPostfix
        = [&](const wstringVector& list, const std::wstring& postfix, bool isPath) {
              for (const auto& k : list) {
                  if (uniqueNames.insert(k).second) {
                      const QString rawName = wstringToQString(k);
                      auto* item = new QStandardItem(
                          rawName + QString(" (") + wstringToQString(postfix) + QString(")"));
                      item->setData(rawName, COMPLETION_RAW_NAME_ROLE);
                      item->setData(isPath, COMPLETION_IS_PATH_ROLE);
                      item->setEditable(false);
                      model->appendRow(item);
                  }
              }
          };

    appendItemsWithPostfix(variableList, POSTFIX_VARIABLE, false);
    appendItemsWithPostfix(fieldList, POSTFIX_FIELD, false);
    appendItemsWithPostfix(propertyList, POSTFIX_PROPERTY, false);
    appendItemsWithPostfix(methodList, POSTFIX_METHOD, false);
    appendItemsWithPostfix(macroList, POSTFIX_MACRO, false);
    appendItemsWithPostfix(builtinList, POSTFIX_BUILTIN, false);
    appendItemsWithPostfix(filesList, POSTFIX_FILES, true);
}
//=============================================================================
void
QtTerminalCompleter::insertCompletion(const QString& completion)
{
    if (m_owner) {
        m_owner->insertCompletion(completion);
    }
}
//=============================================================================
QCompleter*
QtTerminalCompleter::completer()
{
    return m_qCompleter;
}
//=============================================================================
