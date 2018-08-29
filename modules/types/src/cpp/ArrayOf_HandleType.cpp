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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isHandle() const
{
    bool ishandle = (dp->dataClass == NLS_HANDLE);
    return ishandle;
}
//=============================================================================
ArrayOf
ArrayOf::handleConstructor(nelson_handle hl)
{
    nelson_handle* ptrObject = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, 1);
    Dimensions dims(1, 1);
    ptrObject[0] = hl;
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
ArrayOf
ArrayOf::handleConstructor(HandleGenericObject* ptr)
{
    nelson_handle* ptrObject = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_HANDLE, 1);
    Dimensions dims(1, 1);
    ptrObject[0] = HandleManager::getInstance()->addHandle(ptr);
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
HandleGenericObject*
ArrayOf::getContentAsHandleScalar() const
{
    if (!isHandle()) {
        Error(_W("Expected a handle scalar."));
    }
    if (!isScalar()) {
        Error(_W("Expected a handle scalar."));
    }
    nelson_handle* qp = (nelson_handle*)dp->getData();
    if (qp == nullptr) {
        Error(_W("Expected a valid handle."));
    }
    nelson_handle hl = (*qp);
    return HandleManager::getInstance()->getPointer(hl);
}
//=============================================================================
bool
ArrayOf::isHandleProperty(std::wstring propertyName) const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->isProperty(propertyName);
}
//=============================================================================
bool
ArrayOf::isHandleMethod(std::wstring methodName) const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->isMethod(methodName);
}
//=============================================================================
std::wstring
ArrayOf::getHandleCategory() const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->getCategory();
}
//=============================================================================
}
//=============================================================================
