//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isequalnBuiltin.hpp"
#include "isequalBuiltinHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2);
    return isequalCommonBuiltin(eval, nLhs, argIn, "isequaln");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::struct_isequalnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return struct_isequalCommonBuiltin(eval, argIn, "isequaln");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::cell_isequalnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return cell_isequalCommonBuiltin(eval, argIn, "isequaln");
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::char_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return char_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::logical_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return logical_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::double_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return double_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::single_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return single_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int8_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int8_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int16_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int16_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int32_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int32_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::int64_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return int64_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint8_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint8_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint16_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint16_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint32_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint16_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uint64_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return uint64_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================s
ArrayOfVector
Nelson::ElementaryFunctionsGateway::string_isequalnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return string_isequalCommonBuiltin(argIn, "isequaln", false, true);
}
//=============================================================================s
