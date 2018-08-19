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
#include "StringDisplay.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
StringDisplay(Evaluator* eval, ArrayOf Var, bool bFromBuiltin)
{
    if (!Var.isString()) {
        Error(_W("StringDisplay method: String expected."));
    }
    Interface* io = eval->getInterface();
    if (Var.isSingleString()) {
        std::wstring msg = Var.getContentAsWideString();
        if (msg.size() == 0) {
            if (!bFromBuiltin) {
                io->outputMessage("\'\'");
            } else {
                return;
            }
        } else {
            io->outputMessage(msg + L"\n");
        }
    } else {
        wstringVector msg = Var.getContentAsWideStringVector();
        for (indexType i = 0; i < msg.size(); ++i) {
            io->outputMessage(msg[i] + L"\n");
            if (eval->GetInterruptPending()) {
                break;
            }
        }
    }
}
}
//=============================================================================
