//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtWidgets/QApplication>
#include <QtGui/QKeyEvent>
#include "QtTextEdit.hpp"
//=============================================================================
QtTextEdit::QtTextEdit()
{
    setLineWrapMode(QTextEdit::NoWrap);
}
//=============================================================================
QtTextEdit::~QtTextEdit()
{
}
//=============================================================================
void QtTextEdit::keyPressEvent(QKeyEvent *e)
{
    bool tab = false;
    int keycode = e->key();
    if ((keycode == Qt::Key_S) && QApplication::keyboardModifiers() && Qt::ControlModifier)
    {
        e->accept();
    }
    else
    {
        if (keycode)
        {
            QByteArray p(e->text().toUtf8());
            char key;
            if (!e->text().isEmpty())
            {
                key = p[0];
            }
            else
            {
                key = 0;
            }
            if (key == 0x09)
            {
                tab = true;
                emit indent();
            }
        }
        if (!tab)
        {
            QTextEdit::keyPressEvent(e);
        }
        else
        {
            e->accept();
        }
    }
}
//=============================================================================
void QtTextEdit::contextMenuEvent(QContextMenuEvent* e)
{
    e->ignore();
}
