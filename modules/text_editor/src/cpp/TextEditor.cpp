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
#include "TextEditor.hpp"
#include "QStringConverter.hpp"
#include "QtTextEditor.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static QtTextEditor* edit = nullptr;
//=============================================================================
bool
editor(Evaluator* eval)
{
    bool res = false;
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
    edit->showNormal();
    edit->raise();
    return res;
}
//=============================================================================
bool
editor(Evaluator* eval, std::wstring filename)
{
    bool res = false;
    if (edit == nullptr) {
        edit = new QtTextEditor(eval);
    }
    edit->loadOrCreateFile(wstringToQString(filename));
    edit->showNormal();
    edit->raise();
    return res;
}
//=============================================================================
bool
editor(Evaluator* eval, wstringVector filenames)
{
    bool res = true;
    for (size_t k = 0; k < filenames.size(); k++) {
        res = res && editor(eval, filenames[k]);
    }
    return res;
}
//=============================================================================
bool
closeEditor()
{
    if (edit != nullptr) {
        delete edit;
        edit = nullptr;
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
