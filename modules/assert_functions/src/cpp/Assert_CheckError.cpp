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
#include "Assert_CheckError.hpp"
#include "Error.hpp"
#include "EvaluateCommand.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Assert_CheckError(Evaluator* eval, const std::wstring& command, const std::wstring& expectedmsg,
    std::wstring& msg)
{
    bool bEval = false;
    std::wstring computedmsg = L"";
    try {
        bEval = EvaluateCommand(eval, command, true);
    } catch (Exception& e) {
        bEval = false;
        computedmsg = e.getMessage();
    }
    bool bRes = false;
    if (bEval == false) {
        if (computedmsg == expectedmsg) {
            bRes = true;
            msg = L"";
        } else {
            bRes = false;
            msg = _W("Assertion failed : expected error message =") + L" \"" + expectedmsg + +L"\" "
                + _W("computed error message =") + L" \"" + computedmsg + L"\"";
        }
    } else {
        Error(_W("No error was produced while evaluating command."));
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
