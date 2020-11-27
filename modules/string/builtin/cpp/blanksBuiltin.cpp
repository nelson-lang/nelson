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
#include "blanksBuiltin.hpp"
#include "Error.hpp"
#include "Blanks.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::blanksBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    double nbSpacesAsDouble = param1.getContentAsDoubleScalar();
    if (!std::isfinite(nbSpacesAsDouble)) {
        Error(_W("NaN and Inf not allowed."));
    }
    if (floor(nbSpacesAsDouble) != nbSpacesAsDouble) {
        Error(_W("Expected a integer value."));
    }
    if (nbSpacesAsDouble < 0) {
        nbSpacesAsDouble = 0;
    }
    auto nbSpaces = (indexType)nbSpacesAsDouble;
    retval.push_back(Blanks(nbSpaces));
    return retval;
}
//=============================================================================
