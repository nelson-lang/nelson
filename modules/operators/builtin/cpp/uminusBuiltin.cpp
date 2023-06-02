//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uminusBuiltin.hpp"
#include "UnaryMinus.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uminusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    retval << eval->uminusOperator(argIn[0]);
    return retval;
}
//=============================================================================
static ArrayOfVector
generic_uminusBuiltin(NelsonType nlsType, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf A(argIn[0]);
    bool needPromote = !(A.isComplex() && (nlsType == NLS_DOUBLE) || (nlsType == NLS_SINGLE));
    if (needPromote) {
        A.promoteType(nlsType);
    }
    retval << UnaryMinus(A);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::logical_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_DOUBLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::double_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_DOUBLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::single_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_SINGLE, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint8_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_UINT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint16_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_UINT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint32_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_UINT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uint64_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_UINT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int8_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_INT8, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int16_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_INT16, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int32_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_INT32, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::int64_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_INT64, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::char_uminusBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return generic_uminusBuiltin(NLS_CHAR, nLhs, argIn);
}
//=============================================================================
