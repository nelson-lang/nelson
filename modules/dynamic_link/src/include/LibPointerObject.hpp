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
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
#define LIBPOINTER_CATEGORY_STR L"libpointer"
    //=============================================================================
    class NLSDYNAMIC_LINK_IMPEXP LibPointerObject : public HandleGenericObject {
    public:
        LibPointerObject();
        LibPointerObject(std::wstring DataType);
        LibPointerObject(std::wstring DataType, ArrayOf Value);
        ~LibPointerObject();

        void disp(Evaluator *eval);
        void *getPointer();
		bool get(std::wstring propertyName, ArrayOf &res);
        wstringVector fieldnames();
        bool isproperty(std::wstring propertyName);
        bool isWriteableProperty(std::wstring propertyName);
        bool isNull();
        bool plus();
        void reshape(size_t dimX, size_t dimY);
        std::wstring getDataType();

    private:
		wstringVector _propertiesNames;
        wstringVector _methodsNames;
        std::wstring _DataType;
		ArrayOf _value;
        void LibPointerObject::initializeCommon();
    };
    //=============================================================================
}
//=============================================================================
