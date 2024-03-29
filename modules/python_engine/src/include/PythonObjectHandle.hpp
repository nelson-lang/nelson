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
#include "HandleGenericObject.hpp"
#include "nlsPython_engine_exports.h"
#include "Interface.hpp"
#include "ArrayOf.hpp"
#include <functional>
#include <map>
#include <string>

//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPYTHON_ENGINE_IMPEXP PythonObjectHandle : public HandleGenericObject
{
public:
    //=============================================================================
    PythonObjectHandle(void* _ptr);
    ~PythonObjectHandle() override;
    //=============================================================================
    void
    display(Interface* io);
    //=============================================================================
    std::wstring
    getTypeName();
    //=============================================================================
    std::wstring
    getClassName();
    //=============================================================================
    wstringVector
    getMethods();
    //=============================================================================
    bool
    isMethod(const std::wstring& methodName);
    //=============================================================================
    bool
    isProperty(const std::wstring& propertyName);
    //=============================================================================
    wstringVector
    getProperties();
    //=============================================================================
    bool
    get(const std::wstring& propertyName, ArrayOf& result);
    //=============================================================================
    bool
    invoke(const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs,
        ArrayOfVector& results);
    //=============================================================================
    bool
    isMainPythonInterpreter();
    //=============================================================================
private:
    //=============================================================================
    wstringVector
    getCastMethods();
    //=============================================================================
    bool
    isCastMethod(const std::wstring& methodName);
    //=============================================================================
    bool
    invokeCastMethod(const std::wstring& methodName, ArrayOfVector& results);
    //=============================================================================
    using MethodMap = std::map<std::wstring, std::function<bool(ArrayOfVector&)>>;
    MethodMap methodMap;
    //=============================================================================
    bool
    invokeCastCellMethod(ArrayOfVector& results);
    bool
    invokeCastStructMethod(ArrayOfVector& results);
    bool
    invokeCastNumericMethod(ArrayOfVector& results);
    bool
    invokeCastCharMethod(ArrayOfVector& results);
    bool
    invokeCastStringMethod(ArrayOfVector& results);
    bool
    invokeCastDoubleMethod(ArrayOfVector& results);
    bool
    invokeCastSingleMethod(ArrayOfVector& results);
    bool
    invokeCastLogicalMethod(ArrayOfVector& results);
    bool
    invokeCastInt8Method(ArrayOfVector& results);
    bool
    invokeCastInt16Method(ArrayOfVector& results);
    bool
    invokeCastInt32Method(ArrayOfVector& results);
    bool
    invokeCastInt64Method(ArrayOfVector& results);
    bool
    invokeCastUInt8Method(ArrayOfVector& results);
    bool
    invokeCastUInt16Method(ArrayOfVector& results);
    bool
    invokeCastUInt32Method(ArrayOfVector& results);
    bool
    invokeCastUInt64Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeMethodNoArgument(const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs,
        ArrayOfVector& results);
    bool
    invokeMethodOneArgument(const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs,
        ArrayOfVector& results);
    bool
    invokeMethodMultipleArguments(const std::wstring& methodName, const ArrayOfVector& inputs,
        int nLhs, ArrayOfVector& results);
    //=============================================================================
    bool
    invokeFunction(const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs,
        ArrayOfVector& results);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
