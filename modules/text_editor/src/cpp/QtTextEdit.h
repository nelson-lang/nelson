//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
#include <QtCore/QMimeData>
#include <QtWidgets/QTextEdit>
//=============================================================================
class QCompleter;
class QAbstractItemModel;
//=============================================================================
using namespace Nelson;
//=============================================================================
class QtTextEdit : public QTextEdit
{
    Q_OBJECT
public:
    QtTextEdit();
    virtual ~QtTextEdit();
    void
    keyPressEvent(QKeyEvent* event);
    void
    contextMenuEvent(QContextMenuEvent* event);
    void
    focusInEvent(QFocusEvent* e) override;
    void
    setCompleter(QCompleter* completer);
    void
    comment();
    void
    uncomment();
    bool
    canInsertFromMimeData(const QMimeData* source) const;
    void
    insertFromMimeData(const QMimeData* source);

private:
    QCompleter* qCompleter;
    QString
    textUnderCursor() const;
    QAbstractItemModel*
    modelFromNelson(wstringVector filesList, wstringVector builtinList, wstringVector macroList,
        wstringVector variableList);
    void
    updateModel(std::wstring prefix, wstringVector filesList, wstringVector builtinList,
        wstringVector macroList, wstringVector variableList);
    void
    complete(QString prefix);

private slots:
    void
    insertCompletion(const QString& completion);

signals:
    void
    indent();
};
//=============================================================================
