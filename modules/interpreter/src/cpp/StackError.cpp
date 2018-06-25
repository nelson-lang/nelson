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
#include "StackError.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<ErrorInfo>
StackError(Evaluator* eval)
{
    std::vector<ErrorInfo> vectErrorInfo;
    stringVector outstack;
    size_t i = 0;
    while (i < eval->cstack.size()) {
        if (eval->cstack[i].tokid == 0) {
            // This is a new line in the stack trace - we search forward
            // until we get the last line in the current function.  This
            // is the "branch point" for that function.
            size_t j = i + 1;
            while ((j < eval->cstack.size()) && (eval->cstack[j].cname == eval->cstack[i].cname)
                && (eval->cstack[j].detail == eval->cstack[i].detail)
                && (eval->cstack[j].tokid != 0)) {
                j++;
            }
            ErrorInfo ei;
            std::wstring filename = utf8_to_wstring(eval->cstack[j - 1].cname.c_str());
            std::wstring functionname = utf8_to_wstring(eval->cstack[j - 1].detail.c_str());
            int lineposition = eval->cstack[j - 1].tokid & 0x0000FFFF;
            int columposition = eval->cstack[j - 1].tokid >> 16;
            ei.set(filename, functionname, lineposition, columposition);
            vectErrorInfo.push_back(ei);
            i = j;
        } else {
            i++;
        }
    }
    return vectErrorInfo;
}
//=============================================================================
};
//=============================================================================
