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
#include "COM_rangeBuiltin.hpp"
#include "ComExcelHelpers.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_rangeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring range = param1.getContentAsWideString();
        retval << ArrayOf::logicalConstructor(isValidRange(range));
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        indexType m = param1.getContentAsScalarIndex();
        indexType n = param2.getContentAsScalarIndex();
        retval << ArrayOf::characterArrayConstructor(xlsIndexToRange(m, n));
    }
    return retval;
}
//=============================================================================
