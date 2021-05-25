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
#include "narginchkBuiltin.hpp"
#include "Error.hpp"
#include "NargIn.hpp"
#include "Validators.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::narginchkBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 2);
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        Error(_W("not allowed in base scope."));
    } else {
        mustBeScalarOrEmpty(argIn, 0);
        mustBeNonempty(argIn, 0);
        mustBeInteger(argIn, 0);
        mustBeScalarOrEmpty(argIn, 1);
        mustBeNonempty(argIn, 1);
        mustBeInteger(argIn, 1);
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        int minArgs = param1.getContentAsInteger32Scalar();
        int maxArgs = param2.getContentAsInteger32Scalar();

        int nargin = context->getCurrentScope()->getNargIn();
        if (nargin < minArgs) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:notEnoughInputs", true);
        }
        if (nargin > maxArgs) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS, L"Nelson:narginchk:tooManyInputs", true);
        }
    }
    return retval;
}
//=============================================================================
