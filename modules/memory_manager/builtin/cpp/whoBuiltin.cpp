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
#include "whoBuiltin.hpp"
#include "Error.hpp"
#include "Who.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MemoryGateway::whoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    stringVector variablesName;
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0)
    {
        variablesName = Who(eval, LOCAL_SCOPE);
    }
    else if (argIn.size() == 1)
    {
        std::wstring param1;
        if (argIn[0].isSingleString())
        {
            param1 = argIn[0].getContentsAsWideString();
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        if (param1.compare(L"global") == 0)
        {
            variablesName = Who(eval, GLOBAL_SCOPE);
        }
        else if (param1.compare(L"base") == 0)
        {
            variablesName = Who(eval, BASE_SCOPE);
        }
        else if (param1.compare(L"local") == 0)
        {
            variablesName = Who(eval, LOCAL_SCOPE);
        }
        else if (param1.compare(L"caller") == 0)
        {
            variablesName = Who(eval, CALLER_SCOPE);
        }
        else
        {
            Error(eval, _W("Argument #1 : 'global', 'base', 'local' or 'caller' expected."));
        }
    }
    else
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs == 0)
    {
        Interface *io = eval->getInterface();
        std::sort(variablesName.begin(), variablesName.end());
        size_t ncharmax = io->getTerminalWidth();
        size_t nbchar = 0;
        for (size_t k = 0; k < variablesName.size(); k++)
        {
            if (nbchar + variablesName[k].size() < ncharmax)
            {
                io->outputMessage(variablesName[k]);
                io->outputMessage(" ");
                nbchar = 1 + nbchar + variablesName[k].size();
            }
            else
            {
                nbchar = 0;
                io->outputMessage("\n");
                io->outputMessage(variablesName[k]);
                io->outputMessage(" ");
                nbchar = 1 + nbchar + variablesName[k].size();
            }
        }
    }
    else
    {
        retval.push_back(ToCellStringAsColumn(variablesName));
    }
    return retval;
}
//=============================================================================
