//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "ticBuiltin.hpp"
#include "Error.hpp"
#include "TicToc.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::ticBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if ((nLhs != 0) && (nLhs != 1)) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    Tic(eval);
    if (nLhs == 1) {
        retval.push_back(ArrayOf::uint64Constructor(eval->TimerValue));
        eval->TimerValue = (uint64)0;
    }
    return retval;
}
//=============================================================================
