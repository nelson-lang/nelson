//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    static NelsonType
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
    wstringVector
    getProperties() override;
    wstringVector
    getMethods() override;

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
