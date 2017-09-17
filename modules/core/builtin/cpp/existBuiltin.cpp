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
#include "existBuiltin.hpp"
#include "Error.hpp"
#include "Exist.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::CoreGateway::existBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0 || argIn.size() > 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
	ArrayOf param1 = argIn[0];
	int res = 0;
	if (argIn.size() == 1)
	{
		std::wstring name = param1.getContentAsWideString();
		res = Exist(eval, name);
	}
	else
	{
		ArrayOf param2 = argIn[1];
		std::wstring name = param1.getContentAsWideString();
		std::wstring category = param2.getContentAsWideString();
		if (category == L"var" || category == L"builtin" || category == L"dir" || category == L"file")
		{
			res = Exist(eval, name, category);
		}
		else
		{
			Error(eval, _W("Argument #2 must contain a valid string 'var', 'builtin', 'dir' or 'file' expected."));
		}
	}
	retval.push_back(ArrayOf::doubleConstructor((double)res));
    return retval;
}
//=============================================================================

