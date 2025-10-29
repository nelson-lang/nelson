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
//=============================================================================
QtTerminalCompleter::QtTerminalCompleter(QtTerminal* owner) : m_owner(owner), m_qCompleter(nullptr) {}
//=============================================================================
QtTerminalCompleter::~QtTerminalCompleter()
{
    if (m_qCompleter) {
        delete m_qCompleter;
        m_qCompleter = nullptr;
    }
}
//=============================================================================
void QtTerminalCompleter::createIfNeeded()
{
    if (!m_qCompleter) {
        m_qCompleter = new QCompleter(m_owner);
        m_qCompleter->setWidget(m_owner);
        m_qCompleter->setModelSorting(QCompleter::UnsortedModel);
        m_qCompleter->setCompletionMode(QCompleter::UnfilteredPopupCompletion);
        m_qCompleter->setCaseSensitivity(Qt::CaseSensitive);
        m_qCompleter->setWrapAround(false);
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        // QCompleter::activated is overloaded in Qt6; use a lambda and QMetaObject::invokeMethod
        // to call the owner's slot by name, avoiding pointer-to-member access issues.
        QObject::connect(m_qCompleter,
            static_cast<void (QCompleter::*)(const QString&)>(&QCompleter::activated),
            m_owner,
            [this](const QString& completion) {
                if (m_owner) {
                    QMetaObject::invokeMethod(m_owner, "insertCompletion", Qt::DirectConnection,
                        Q_ARG(QString, completion));
                }
            });
#else
        QObject::connect(m_qCompleter, SIGNAL(activated(QString)), m_owner, SLOT(insertCompletion(QString)));
#endif
    }
}
//=============================================================================
void QtTerminalCompleter::complete(const QString& prefix)
{
    if (prefix.isEmpty()) {
        return;
    }

    // ensure completer exists before any dereference
    createIfNeeded();
    if (!m_qCompleter) {
        return; // defensive: if creation failed for some reason
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
            cr.setWidth(popup->sizeHintForColumn(0)
                + popup->verticalScrollBar()->sizeHint().width());
            cr.setHeight(20);
            m_qCompleter->complete(cr);
            m_qCompleter->setCurrentRow(0);
            m_qCompleter->popup()->setCurrentIndex(m_qCompleter->completionModel()->index(0, 0));
            popup->setVisible(true);
        } else {
            // Nothing to show: ensure popup closed
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
void QtTerminalCompleter::updateModel(const std::wstring& prefix, const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    if (m_qCompleter != nullptr) {
        m_qCompleter->setModel(modelFromNelson(
            filesList, builtinList, macroList, variableList, fieldList, propertyList, methodList));
        m_qCompleter->setCompletionPrefix(wstringToQString(prefix));
    }
}
//=============================================================================
QAbstractItemModel* QtTerminalCompleter::modelFromNelson(const wstringVector& filesList,
    const wstringVector& builtinList, const wstringVector& macroList,
    const wstringVector& variableList, const wstringVector& fieldList,
    const wstringVector& propertyList, const wstringVector& methodList)
{
    QStringList words;

    auto appendItemsWithPostfix = [&](const wstringVector& list, const std::wstring& postfix) {
        for (const auto& k : list) {
            words.append(wstringToQString(k) + QString(" (") + wstringToQString(postfix) + QString(")"));
        }
    };

    appendItemsWithPostfix(filesList, POSTFIX_FILES);
    appendItemsWithPostfix(builtinList, POSTFIX_BUILTIN);
    appendItemsWithPostfix(macroList, POSTFIX_MACRO);
    appendItemsWithPostfix(variableList, POSTFIX_VARIABLE);
    appendItemsWithPostfix(fieldList, POSTFIX_FIELD);
    appendItemsWithPostfix(methodList, POSTFIX_METHOD);
    appendItemsWithPostfix(propertyList, POSTFIX_PROPERTY);
    words.sort();
    return new QStringListModel(words, m_qCompleter);
}
//=============================================================================
void QtTerminalCompleter::insertCompletion(const QString& completion)
{
    // forward to owner implementation
    if (m_owner) {
        m_owner->insertCompletion(completion);
    }
}
//=============================================================================
QCompleter* QtTerminalCompleter::completer()
{
    return m_qCompleter;
}
//=============================================================================
