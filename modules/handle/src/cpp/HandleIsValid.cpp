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
#include "HandleIsValid.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	ArrayOf HandleIsValid(ArrayOf A)
	{
		Dimensions dimsA = A.getDimensions();
		Dimensions dimsRes = A.getDimensions();
		nelson_handle *handles = (nelson_handle *)A.getDataPointer();
		logical *l = (logical *)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount());
		for (indexType k = 0; k < dimsA.getElementCount(); k++)
		{
			l[k] = HandleManager::getInstance()->isValid(handles[k]);
		}
		return ArrayOf::ArrayOf(NLS_LOGICAL, dimsRes, (void*)l);
	}
	//=============================================================================
}
//=============================================================================
