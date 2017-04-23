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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson
{
	//=============================================================================
	const bool ArrayOf::isHandle() const
	{
		bool ishandle = (dp->dataClass == NLS_HANDLE);
		return ishandle;
	}
	//=============================================================================
	ArrayOf ArrayOf::handleConstructor(HandleGenericObject *ptr)
	{
		nelson_handle *ptrObject = (nelson_handle *)ArrayOf::allocateArrayOf(NLS_HANDLE, 1);
		Dimensions dims(1, 1);
		ptrObject[0] = HandleManager::getInstance()->addHandle(ptr);
		return ArrayOf(NLS_HANDLE, dims, (void *)ptrObject);
	}
	//=============================================================================
	HandleGenericObject *ArrayOf::getContentsAsHandleScalar()
	{
		if (!isHandle())
		{
			throw Exception(_W("Expected a handle scalar."));
		}
		if (!isScalar())
		{
			throw Exception(_W("Expected a handle scalar."));
		}
		nelson_handle *qp = (nelson_handle*)dp->getData();
		nelson_handle hl = (*qp);
		return HandleManager::getInstance()->getPointer(hl);
	}
	//=============================================================================
}
//=============================================================================
