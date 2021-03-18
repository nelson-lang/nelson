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
#include "errorBuiltin.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "characters_encoding.hpp"
#include "IsErrorStruct.hpp"
#include "MException.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::errorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "error", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "error", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        std::wstring message;
        std::wstring identifier;

        if (argIn.size() == 1) {
            if (argIn[0].isRowVectorCharacterArray()) {
                message = argIn[0].getContentAsWideString();
            } else {
                Exception e;
                if (IsErrorStruct(argIn[0], e)) {
                    eval->setLastErrorException(e);
                    throw e;
                } else {
                    Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
                }
            }
        } else {
            // nargin == 2
            identifier = argIn[0].getContentAsWideString();
            if (!isValidMExceptionIdentifier(identifier)) {
                Error(_W("First input argument must be a valid message identifier."));
            }
            message = argIn[1].getContentAsWideString();
        }
        if (message != L"") {
            Error(message, identifier);
        }
    }
    return retval;
}
//=============================================================================
