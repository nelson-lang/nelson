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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define LIBPOINTER_CATEGORY_STR L"libpointer"
//=============================================================================
class NLSDYNAMIC_LINK_IMPEXP LibPointerObject : public HandleGenericObject
{
public:
    LibPointerObject();
    LibPointerObject(std::wstring DataType);
    LibPointerObject(std::wstring DataType, ArrayOf Value);
    LibPointerObject(void* pointer);
    LibPointerObject(void* pointer, std::wstring DataType, Class currentType);

    ~LibPointerObject();

    void
    disp(Evaluator* eval);
    void*
    getPointer();
    bool
    get(std::wstring propertyName, ArrayOf& res);
    void
    get(ArrayOf& res);
    wstringVector
    fieldnames();
    bool
    isProperty(std::wstring propertyName);
    bool
    isMethod(std::wstring methodName);

    bool
    isWriteableProperty(std::wstring propertyName);
    bool
    isNull();
    LibPointerObject*
    plus(indexType offset);
    void
    reshape(indexType dimX, indexType dimY);
    std::wstring
    getDataType();
    void
    setDataType(std::wstring dataType);

private:
    wstringVector _propertiesNames;
    wstringVector _methodsNames;
    void
    initializeCommon();

    std::wstring _DataType;
    void* _voidPointer;
    long int _dimX;
    long int _dimY;
    Class _currentType;
    long int _initialDimX;
    long int _initialDimY;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
