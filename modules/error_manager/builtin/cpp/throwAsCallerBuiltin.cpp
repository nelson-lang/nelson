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
#include "throwAsCallerBuiltin.hpp"
#include "MException.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
#include "DebugStack.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::throwAsCallerBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (argIn[0].isClassStruct() && ClassName(argIn[0]) == "MException") {
        if (argIn[0].isScalar()) {
            Exception mException = ArrayOfToException(argIn[0]);
            Nelson::stackTrace trace;
            DebugStack(eval->callstack, 2, trace);
            mException.setTrace(trace);
            throw mException;
        }
        Error(_W("MException scalar expected."));
    }
    Error(_W("MException expected."));
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
