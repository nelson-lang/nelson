//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "HandleGenericObject.hpp"
#include "nlsJulia_engine_exports.h"
#include "Interface.hpp"
#include "ArrayOf.hpp"
#include "ArrayOfVector.hpp"
#include <functional>
#include <map>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSJULIA_ENGINE_IMPEXP JuliaObjectHandle : public HandleGenericObject
{
public:
    //=============================================================================
    JuliaObjectHandle(void* _ptr);
    ~JuliaObjectHandle() override;
    //=============================================================================
    void
    display(Interface* io);
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    bool
    invokeMethod(Interface* io, const ArrayOfVector& argIn, int nLhs, const std::string& methodName,
        ArrayOfVector& results) override;
    //=============================================================================
    bool
    invoke(Interface* io, const std::wstring& methodName, const ArrayOfVector& inputs, int nLhs,
        ArrayOfVector& results);
    //=============================================================================

    bool
    isProperty(const std::wstring& propertyName) override;
    //=============================================================================
    wstringVector
    getMethods();
    //=============================================================================
    bool
    isMethod(const std::wstring& methodName);
    //=============================================================================
    bool
    isCastMethod(const std::wstring& methodName);
    //=============================================================================
    wstringVector
    getProperties();
    //=============================================================================
    bool
    get(const std::wstring& propertyName, ArrayOf& result);
    //=============================================================================
private:
    //=============================================================================
    wstringVector methodCastNames;
    //=============================================================================
    using MethodMap = std::map<std::wstring, std::function<bool(ArrayOfVector&)>>;
    MethodMap methodMap;
    //=============================================================================
    wstringVector
    getCastMethods();
    //=============================================================================
    bool
    invokeCastMethod(const std::wstring& methodName, ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastNumericMethod(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastStringMethod(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastCellMethod(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastDoubleMethod(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastSingleMethod(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastInt8Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastInt16Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastInt32Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastInt64Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastUInt8Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastUInt16Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastUInt32Method(ArrayOfVector& results);
    //=============================================================================
    bool
    invokeCastUInt64Method(ArrayOfVector& results);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
