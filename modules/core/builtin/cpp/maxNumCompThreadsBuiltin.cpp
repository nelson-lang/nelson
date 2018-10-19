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
#include "maxNumCompThreadsBuiltin.hpp"
#include "ComputionalThreads.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::maxNumCompThreadsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    unsigned int currentValue = getMaxNumCompThreads();
    switch (argIn.size()) {
    case 0: {
        retval.push_back(ArrayOf::doubleConstructor((double)currentValue));
    } break;
    case 1: {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if (str == L"automatic") {
                setDefaultMaxNumCompThreads();
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
        } else {
            indexType N = param1.getContentAsScalarIndex(false);
            setMaxNumCompThreads((unsigned int)N);
        }
        retval.push_back(ArrayOf::doubleConstructor((double)currentValue));
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
