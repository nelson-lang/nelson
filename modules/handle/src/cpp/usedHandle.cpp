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
#include "usedHandle.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
usedHandle(const std::wstring& category)
{
    ArrayOf res;
    std::vector<nelson_handle> used
        = HandleManager::getInstance()->getAllHandlesOfCategory(category);
    size_t nbHandles = used.size();
    if (nbHandles > 0) {
        Dimensions dims(1, nbHandles);
        nelson_handle* nh = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, nbHandles);
        for (int k = 0; k < nbHandles; k++) {
            nh[k] = used[k];
        }
        res = ArrayOf(NLS_HANDLE, dims, (void*)nh);
    } else {
        res = ArrayOf::emptyConstructor(0, 0);
        res.promoteType(NLS_HANDLE);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
