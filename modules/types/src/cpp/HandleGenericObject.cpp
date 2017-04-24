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
#include "HandleGenericObject.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    HandleGenericObject::HandleGenericObject(std::wstring _category, void *_ptr)
    {
        if (_category.empty())
        {
            throw Exception(_W("handle must have a type."));
        }
        if (_ptr == nullptr)
        {
            throw Exception(_W("handle must have a pointer."));
        }
        this->category = _category;
        this->ptr = _ptr;
    }
    //=============================================================================
    HandleGenericObject::~HandleGenericObject()
    {
        this->category.clear();
        this->ptr = nullptr;
    }
    //=============================================================================
    void HandleGenericObject::setCategory(std::wstring _category)
    {
        if (_category.empty())
        {
            throw Exception(_W("handle must have a type."));
        }
        this->category = _category;
    }
    //=============================================================================
    std::wstring HandleGenericObject::getCategory()
    {
        return this->category;
    }
    //=============================================================================
    void HandleGenericObject::setPointer(void *_ptr)
    {
        this->ptr = _ptr;
    }
    //=============================================================================
    void *HandleGenericObject::getPointer()
    {
        return this->ptr;
    }
    //=============================================================================
}
//=============================================================================
