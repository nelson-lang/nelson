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
#include "tocBuiltin.hpp"
#include "Error.hpp"
#include "StringFormat.hpp"
#include "TicToc.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::tocBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if ((nLhs != 0) && (nLhs != 1)) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 1) {
        ArrayOf paramOne = argIn[0];
        uint64 t = paramOne.getContentAsUnsignedInt64Scalar();
        double r = 0;
        if (Toc(t, r)) {
            ArrayOfVector retval;
            if (nLhs == 0) {
                std::wstring msg = StringFormat(_W("Elapsed time is %f seconds.").c_str(), r);
                eval->getInterface()->outputMessage(msg + L"\n");
            } else {
                retval.push_back(ArrayOf::doubleConstructor(r));
            }
            return retval;
        } else {
            Error(_W("Cannot call toc."));
        }
    } else // argIn.size() == 0
    {
        if (eval->TimerValue == 0) {
            Error(_W("You must call \'tic\' without an output argument before calling \'toc\' "
                     "without an input argument."));
        }
        double r = 0;
        if (Toc(eval, r)) {
            ArrayOfVector retval;
            if (nLhs == 0) {
                std::wstring msg = StringFormat(_W("Elapsed time is %f seconds.").c_str(), r);
                eval->getInterface()->outputMessage(msg + L"\n");
            } else {
                retval.push_back(ArrayOf::doubleConstructor(r));
            }
            return retval;
        } else {
            Error(_W("You must call \'tic\' without an output argument before calling \'toc\' "
                     "without an input argument."));
        }
    }
    // NEVER HERE
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
