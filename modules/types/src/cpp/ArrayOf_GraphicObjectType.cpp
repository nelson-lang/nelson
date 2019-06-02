//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isGraphicObject() const
{
    if (dp) {
        return (dp->dataClass == NLS_GO_HANDLE);
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::graphicObjectConstructor(void* ptrObject)
{
    nelson_handle* ptr = static_cast<nelson_handle*>(ArrayOf::allocateArrayOf(NLS_GO_HANDLE, 1));
    Dimensions dims(1, 1);
    ptr[0] = PTR_TO_NELSON_HANDLE(ptrObject);
    ArrayOf res = ArrayOf(NLS_GO_HANDLE, dims, (void*)ptr);
    return res;
}
//=============================================================================
void*
ArrayOf::getContentAsGraphicObjectScalar() const
{
    if (!isGraphicObject()) {
        Error(_W("Expected an graphic object."));
    }
    if (!isScalar()) {
        Error(_W("Expected an graphic object scalar."));
    }
    nelson_handle* qp = (nelson_handle*)getDataPointer();
    if (qp == nullptr) {
        Error(_W("Expected a valid handle."));
    }
    return (void*)NELSON_HANDLE_TO_PTR(qp[0]);
}
//=============================================================================
}
//=============================================================================
