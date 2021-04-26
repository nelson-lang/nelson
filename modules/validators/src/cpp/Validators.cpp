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
#include "Validators.hpp"
#include "Error.hpp"
#include "ValidatorsInternal.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
checkArgumentPosition(const ArrayOfVector& args, int argPosition)
{
    if (argPosition < 0 || argPosition >= args.size()) {
        std::wstring msg = _W("Invalid input argument position.");
        std::wstring id = _W("Nelson:validators:invalidInputPosition");
        Error(msg, id);
    }
}
//=============================================================================
void
mustBeLogical(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLogical(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeLogicalScalar(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFinite(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFinite(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonempty(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonempty(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeScalarOrEmpty(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeValidVariableName(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeTextScalar(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeTextScalar(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeVector(const ArrayOfVector& args, bool allowsAllEmpties, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeVector(args[argPosition], allowsAllEmpties, argPosition + 1);
}
//=============================================================================
void
mustBeFloat(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFloat(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNumeric(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNumeric(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFolder(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeFolder(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeFile(const ArrayOfVector& args, int argPosition)
{ 
    checkArgumentPosition(args, argPosition);
    mustBeFile(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeA(const ArrayOfVector& args, const wstringVector &classNames, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeA(args[argPosition], classNames, argPosition + 1);
}
//=============================================================================
void
mustBePositive(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBePositive(args[argPosition], argPosition + 1);
}
//=============================================================================
void
mustBeNonnegative(const ArrayOfVector& args, int argPosition)
{
    checkArgumentPosition(args, argPosition);
    mustBeNonnegative(args[argPosition], argPosition + 1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
