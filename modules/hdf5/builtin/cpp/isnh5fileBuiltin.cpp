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
#include "isnh5fileBuiltin.hpp"
#include "isNh5File.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// isnh5file(filename)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::isnh5fileBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 3);
    nargincheck(argIn, 1, 1);
    wstringVector filenames = argIn[0].getContentAsWideStringVector(true);
    ArrayOf isNh5;
    ArrayOf nh5Versions;
    ArrayOf nh5Headers;
    isNh5File(filenames, isNh5, nh5Versions, nh5Headers);
    retval << isNh5;
    if (nLhs > 1) {
        retval << nh5Versions;
    }
    if (nLhs > 2) {
        retval << nh5Headers;
    }
    return retval;
}
//=============================================================================
