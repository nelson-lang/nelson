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
#include "ismethodComHandleObject.hpp"
#include "ComHandleObject.hpp"
#include "ComHelpers.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include <Windows.h>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ismethodComHandleObject(ComHandleObject* comhandleobj, const std::wstring& methodname)
{
    void* ptr = comhandleobj->getPointer();
    if (ptr == nullptr) {
        Error(_W("COM valid handle expected."));
    }
    auto* pVariant = static_cast<VARIANT*>(ptr);
    return isMethodCom(pVariant->pdispVal, methodname);
}
//=============================================================================
ArrayOf
ismethodComHandleObject(ArrayOf A, const std::wstring& methodname)
{
    if (A.getHandleCategory() != COM_CATEGORY_STR) {
        Error(_W("COM handle expected."));
    }
    auto* comhandleobj = (ComHandleObject*)A.getContentAsHandleScalar();
    bool res = ismethodComHandleObject(comhandleobj, methodname);
    return ArrayOf::logicalConstructor(res);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
