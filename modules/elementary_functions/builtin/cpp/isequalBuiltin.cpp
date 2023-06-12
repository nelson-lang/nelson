//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalBuiltin.hpp"
#include "isequalBuiltinHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);
    return isequalCommonBuiltin(eval, nLhs, argIn, "isequal");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::struct_isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return struct_isequalCommonBuiltin(eval, argIn, "isequal");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::cell_isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return cell_isequalCommonBuiltin(eval, argIn, "isequal");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::char_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return char_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::logical_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return logical_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::double_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return double_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::single_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return single_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int8_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int8_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int16_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int16_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int32_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int32_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int64_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int64_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint8_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint8_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint16_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint16_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint32_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint16_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint64_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint64_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================s
ArrayOfVector
Nelson::ElementaryFunctionsGateway::string_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return string_isequalCommonBuiltin(argIn, "isequal", false, false);
}
//=============================================================================s
