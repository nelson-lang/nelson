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
#include "whichBuiltin.hpp"
#include "Which.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::FunctionsGateway::whichBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if ((argIn.size() != 1) && (argIn.size() != 2))
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    else if (argIn.size() == 1)
    {
        if (argIn[0].isSingleString())
        {
            std::wstring wfunctionname = argIn[0].getContentsAsWideString();
			if (nLhs == 0) 
			{
				Interface *io = eval->getInterface();
				if (io)
				{
					FuncPtr fptr = nullptr;
					bool found = BuiltInFunctionDefManager::getInstance()->find(wstring_to_utf8(wfunctionname), fptr);
					std::wstring path = Which(wfunctionname);
					if (found)
					{
						io->outputMessage(_W("built-in") + L" (" + path + L")");
					}
					else
					{
						if (path == L"")
						{
							io->outputMessage(L"'" + wfunctionname + L"' " + _W("not found."));
						}
						else
						{
							io->outputMessage(path);
						}
					}
				}
			}
			else
			{
				retval.push_back(ArrayOf::stringConstructor(Which(wfunctionname)));
			}
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    else
    {
        // case argIn.size() == 2
        std::wstring wfunctionname;
        if (argIn[0].isSingleString())
        {
            wfunctionname = argIn[0].getContentsAsWideString();
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring wparam2;
        if (argIn[1].isSingleString())
        {
            wparam2 = argIn[1].getContentsAsWideString();
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        if (wparam2.compare(L"-all") == 0)
        {
            wstringVector res = WhichAll(wfunctionname);
            retval.push_back(ToCellStringAsColumn(res));
        }
        else if (wparam2.compare(L"-module") == 0)
        {
            wstringVector res = WhichModule(wfunctionname);
            retval.push_back(ToCellStringAsColumn(res));
        }
        else
        {
            Error(eval, _W("#2 Argument must be \'-all\' or  \'-module\'."));
        }
    }
    return retval;
}
//=============================================================================
