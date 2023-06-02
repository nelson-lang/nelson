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
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
#define OVERLOAD_OPERATOR_NAME(typeName, operatorName) typeName "_" operatorName
//=============================================================================
#define OVERLOAD_FUNCTION_NAME(typeName, functionName) typeName "_" functionName
//=============================================================================
#define OVERLOAD_METHOD_NAME(typeName, methodName) typeName "_" methodName
//=============================================================================
inline std::string
overloadOperatorName(const std::string& typeName, OperatorType operatorType)
{
    return typeName + "_" + getOperatorName(operatorType);
}
//=============================================================================
inline std::string
overloadFunctionName(const std::string& typeName, const std::string& functionName)
{
    return typeName + "_" + functionName;
}
//=============================================================================
inline std::string
overloadMethodName(const std::string& typeName, const std::string& methodName)
{
    return typeName + "_" + methodName;
}
//=============================================================================
}
//=============================================================================
namespace Nelson::Overload {
//=============================================================================
enum OverloadClass
{
    FUNCTION = 0,
    UNARY,
    BINARY,
    TERNARY
};
//=============================================================================
} // namespace Nelson
//=============================================================================
