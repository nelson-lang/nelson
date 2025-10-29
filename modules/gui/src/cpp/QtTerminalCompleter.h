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
#include <QtWidgets/QCompleter>
#include <QtCore/QStringListModel>
#include <QtCore/QAbstractItemModel>
#include <QtCore/QString>
#include <vector>
#include <string>
#include "Types.hpp"
//=============================================================================
using namespace Nelson;
class QtTerminal;
//=============================================================================
class QtTerminalCompleter
{
public:
    explicit QtTerminalCompleter(QtTerminal* owner);
    ~QtTerminalCompleter();

    void createIfNeeded();
    void complete(const QString& prefix);
    void updateModel(const std::wstring& prefix, const wstringVector& filesList,
        const wstringVector& builtinList, const wstringVector& macroList,
        const wstringVector& variableList, const wstringVector& fieldList,
        const wstringVector& propertyList, const wstringVector& methodList);

    QCompleter* completer();

    // Expose modelFromNelson for QtTerminal fallback
    QAbstractItemModel* modelFromNelson(const wstringVector& filesList, const wstringVector& builtinList,
        const wstringVector& macroList, const wstringVector& variableList,
        const wstringVector& fieldList, const wstringVector& propertyList,
        const wstringVector& methodList);

private:
    void insertCompletion(const QString& completion);

    QtTerminal* m_owner;
    QCompleter* m_qCompleter;
};
//=============================================================================
