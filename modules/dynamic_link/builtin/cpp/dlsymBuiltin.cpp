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
#include "dlsymBuiltin.hpp"
#include "Error.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "ClassName.hpp"
#include "HandleManager.hpp"
#include "CreateDynamicLinkLibraryObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::DynamicLinkGateway::dlsymBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 4)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
	ArrayOf param1 = argIn[0];
	if (!param1.isScalar())
	{
		Error(eval, _W("Wrong size for argument #1: dllib scalar handle expected."));
	}
	std::wstring classname;
	ClassName(param1, classname);
	if (DLLIB_CATEGORY_STR != classname)
	{
		Error(eval, _W("Wrong type for argument #1: dllib scalar handle expected."));
	}
	nelson_handle *qp = (nelson_handle*)param1.getDataPointer();
	nelson_handle hl = qp[0];
	HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
	DynamicLinkLibraryObject *obj = (DynamicLinkLibraryObject *)hlObj;

	ArrayOf param2 = argIn[1];
	std::wstring symbolName = param2.getContentAsWideString();

	ArrayOf param3 = argIn[2];
	std::wstring returnTypeString = param3.getContentAsWideString();

	ArrayOf param4 = argIn[3];
	std::wstring argumentsString = param4.getContentAsWideString();

	retval.push_back(createDynamicLinkSymbolObject(param1, symbolName, returnTypeString, argumentsString));
	return retval;
}
//=============================================================================
