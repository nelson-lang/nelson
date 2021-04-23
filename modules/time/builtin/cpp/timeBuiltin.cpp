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
#include "timeBuiltin.hpp"
#include "Error.hpp"
#include "Time.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleConstructor(TimeAsSeconds());
    } break;
    case 1: {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"ns") {
            retval << ArrayOf::uint64Constructor(TimeAsNanoSeconds());
        } else if (param1 == L"s") {
            retval << ArrayOf::doubleConstructor(TimeAsSeconds());
        } else {
            Error(_W("Argument #2: 'ns' or 's' expected."));
        }
    } break;
    }
    return retval;
}
//=============================================================================
