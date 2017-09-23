//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ndarrayinteger_vertcat_ndarrayintegerBuiltin.hpp"
#include "Error.hpp"
#include "VertCatInteger.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector ndarrayinteger_vertcat_ndarrayintegerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, Class intclass)
{
    ArrayOfVector retval;
    if (argIn.size() != 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    retval.push_back(VertCatNdArrayInteger(A, B));
    return retval;
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayint8_vertcat_ndarrayint8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_INT8);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayint16_vertcat_ndarrayint16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_INT16);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayint32_vertcat_ndarrayint32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_INT32);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayint64_vertcat_ndarrayint64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_INT64);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayuint8_vertcat_ndarrayuint8Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_UINT8);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayuint16_vertcat_ndarrayuint16Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_UINT16);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayuint32_vertcat_ndarrayuint32Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_UINT32);
}
//=============================================================================
ArrayOfVector Nelson::IntegerGateway::ndarrayuint64_vertcat_ndarrayuint64Builtin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ndarrayinteger_vertcat_ndarrayintegerBuiltin(eval, nLhs, argIn, NLS_UINT64);
}
//=============================================================================
