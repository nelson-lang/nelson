//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "displayBuiltin.hpp"
#include "Error.hpp"
#include "OverloadDisplay.hpp"
#include "DisplayVariable.hpp"
#include "characters_encoding.hpp"
#include "DisplayCell.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);

    ArrayOf variable = argIn[0];
    std::wstring variableName = variable.wname();
    if (argIn.size() == 2) {
        variableName = argIn[1].getContentAsWideString();
    }
    OverloadDisplay(eval, variable, variableName);
    return retval;
}
//=============================================================================
static ArrayOfVector
generic_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf variable = argIn[0];
    std::wstring variableName = variable.wname();
    if (argIn.size() == 2) {
        variableName = argIn[1].getContentAsWideString();
    }
    bool needToOverload;
    DisplayVariable(eval->getInterface(), variable, variableName, needToOverload);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::int8_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::int16_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::int32_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::int64_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::uint8_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::uint16_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::uint32_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::uint64_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::logical_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::char_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::double_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::single_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::struct_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::cell_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf variable = argIn[0];
    std::wstring variableName = variable.wname();
    if (argIn.size() == 2) {
        variableName = argIn[1].getContentAsWideString();
    }
    DisplayCell(eval->getInterface(), variable, variableName);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::handle_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::string_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::sparsedouble_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::sparselogical_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return generic_displayBuiltin(eval, nLhs, argIn);
}
//=============================================================================
