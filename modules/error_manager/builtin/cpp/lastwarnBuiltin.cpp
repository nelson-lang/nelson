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
#include "lastwarnBuiltin.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lastwarnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    Exception lastWarning = eval->getLastWarningException();
    switch (argIn.size()) {
    case 0: {
    } break;
    case 1: {
        ArrayOf arg1 = argIn[0];
        if (arg1.isSingleString()) {
            std::wstring message = arg1.getContentAsWideString();
            if (message == L"") {
                eval->resetLastWarningException();
			} else {
                Exception newLastWarning(message, L"", -1, -1, L"", L"");
                eval->setLastWarningException(newLastWarning);
			}
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } break;
    case 2: {
        ArrayOf arg1 = argIn[0];
        std::wstring message;
        std::wstring identifier;
        if (arg1.isSingleString()) {
            message = arg1.getContentAsWideString();
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        ArrayOf arg2 = argIn[1];
        if (arg2.isSingleString()) {
			identifier = arg2.getContentAsWideString();
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        Exception newLastWarning(message, L"", -1, -1, L"", identifier);
        eval->setLastWarningException(newLastWarning);
    } break;
    default: {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    switch (nLhs) {
    case 0:
    case 1: {
        std::wstring message = lastWarning.getFormattedErrorMessage();
        retval.push_back(ArrayOf::stringConstructor(message));
    } break;
    case 2: {
        std::wstring message = lastWarning.getFormattedErrorMessage();
        std::wstring identifier = lastWarning.getIdentifier();
        retval.push_back(ArrayOf::stringConstructor(message));
        retval.push_back(ArrayOf::stringConstructor(identifier));
    } break;
    default: {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
