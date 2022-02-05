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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "DynamicLinkLibraryObject.hpp"
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
#include <ffi.h>
//=============================================================================
namespace Nelson {
//=============================================================================
#define DLSYM_CATEGORY_STR L"dlsym"
//=============================================================================
class NLSDYNAMIC_LINK_IMPEXP DynamicLinkSymbolObject : public HandleGenericObject
{
public:
    DynamicLinkSymbolObject(const ArrayOf& dllibObject, void* pointerFunction,
        const std::wstring& symbol, const std::wstring& returnType, wstringVector paramsType);
    ~DynamicLinkSymbolObject() override;
    ArrayOfVector
    call(Evaluator* eval, int Lhs, ArrayOfVector params);
    void
    disp(Interface* io);
    static Class
    GetNelsonType(const std::wstring& type);
    static bool
    isValidDataType(const std::wstring& DataType);
    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    bool
    isWriteableProperty(const std::wstring& propertyName);
    wstringVector
    fieldnames();
    bool
    isProperty(const std::wstring& propertyName) override;
    bool
    isMethod(const std::wstring& methodName) override;

private:
    ArrayOf _dllibObject;
    void* _pointerFunction;
    std::wstring _symbol;
    std::wstring _returnType;
    wstringVector _paramsTypes;
    wstringVector _paramsInTypes;
    wstringVector _paramsOutTypes;
    ffi_cif _cif;
    size_t _nArgIn;
    size_t _nArgOut;
    std::wstring _prototype;
    size_t
    lengthTextToDisplay(const wstringVector& params);
    wstringVector _propertiesNames;
    void
    buildPrototype();
};
//=============================================================================
}; // namespace Nelson
//=============================================================================
