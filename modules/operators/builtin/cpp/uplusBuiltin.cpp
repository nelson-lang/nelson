//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uplusBuiltin.hpp"
#include "UnaryPlus.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uplusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    retval << eval->uplusOperator(argIn[0]);
    return retval;
}
//=============================================================================
static ArrayOfVector
generic_uplusBuiltin(NelsonType nlsType, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf A(argIn[0]);
    bool needPromote = !(A.isComplex() && (nlsType == NLS_DOUBLE) || (nlsType == NLS_SINGLE));
    if (needPromote) {
        A.promoteType(nlsType);
    }
    retval << UnaryPlus(A);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::logical_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_DOUBLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::double_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_DOUBLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::single_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_SINGLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint8_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_UINT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint16_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_UINT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint32_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_UINT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint64_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_UINT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int8_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_INT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int16_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_INT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int32_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_INT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int64_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_INT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::char_uplusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uplusBuiltin(NLS_CHAR, nLhs, argIn);
}
//=============================================================================
