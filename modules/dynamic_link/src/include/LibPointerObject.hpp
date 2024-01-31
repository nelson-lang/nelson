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
#include "Evaluator.hpp"
#include "HandleGenericObject.hpp"
#include "nlsDynamic_link_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSDYNAMIC_LINK_IMPEXP LibPointerObject : public HandleGenericObject
{
public:
    LibPointerObject();
    LibPointerObject(const std::wstring& DataType);
    LibPointerObject(const std::wstring& DataType, ArrayOf Value);
    LibPointerObject(void* pointer);
    LibPointerObject(void* pointer, const std::wstring& DataType, NelsonType currentType);

    ~LibPointerObject() override;

    void
    disp(Interface* io);
    void*
    getPointer();
    bool
    get(const std::wstring& propertyName, ArrayOf& res);
    void
    get(ArrayOf& res);
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

    bool
    isWriteableProperty(const std::wstring& propertyName);
    bool
    isNull();
    LibPointerObject*
    plus(indexType offset);
    void
    reshape(indexType dimX, indexType dimY);
    std::wstring
    getDataType();
    void
    setDataType(const std::wstring& dataType);

private:
    wstringVector _propertiesNames;
    wstringVector _methodsNames;
    void
    initializeCommon();

    std::wstring _DataType;
    void* _voidPointer;
    long int _dimX;
    long int _dimY;
    NelsonType _currentType;
    long int _initialDimX;
    long int _initialDimY;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
