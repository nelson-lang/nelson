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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdio>
#include "sscanfBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "SscanfFunction.hpp"
#include "Exception.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::sscanfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 4);
    bool isParam1SupportedType
        = (argIn[0].isStringArray() && argIn[0].isScalar() || argIn[0].isRowVectorCharacterArray());

    if (!isParam1SupportedType) {
        Error(_("First argument must be a text scalar."));
    }
    std::wstring wstr = argIn[0].getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::wstring format = param2.getContentAsWideString();

    double m = -1, n = -1;
    bool haveThirdArgument = false;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        param3.promoteType(NLS_DOUBLE);
        Dimensions dims3 = param3.getDimensions();
        if (param3.isDoubleType(true)) {
            if (dims3.isScalar() || dims3.getElementCount() == 2) {
                if (dims3.isScalar()) {
                    m = param3.getContentAsDoubleScalar();
                    if (m < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                } else {
                    double* ptr = (double*)param3.getDataPointer();
                    m = ptr[0];
                    if (m < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                    n = ptr[1];
                    if (n < 0) {
                        Error(_W("Wrong value >= 0 expected."));
                    }
                }
            } else {
                Error(_W("Wrong size. scalar or [a, b] expected."));
            }
        } else {
            Error(_W("Wrong type. double expected."));
        }
        haveThirdArgument = true;
    }

    indexType count = 0;
    indexType nextIndex = 0;
    std::wstring errorMessage;

    ArrayOf value = SscanF(wstr, format, m, n, haveThirdArgument, count, nextIndex, errorMessage);
    retval << value;
    if (nLhs > 1) {
        retval << ArrayOf::doubleConstructor((double)count);
    }
    if (nLhs > 2) {
        retval << ArrayOf::characterArrayConstructor(errorMessage);
    }
    if (nLhs > 3) {
        retval << ArrayOf::doubleConstructor((double)nextIndex);
    }
    return retval;
}
//=============================================================================
