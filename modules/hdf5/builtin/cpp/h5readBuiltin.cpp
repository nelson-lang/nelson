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
#include "h5readBuiltin.hpp"
#include "Error.hpp"
#include "h5ReadDataset.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// data = h5read(filename, datasetname)
//=============================================================================
ArrayOfVector
Nelson::Hdf5Gateway::h5readBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    std::wstring filename;
    std::wstring datasetname;
    switch (argIn.size()) {
    case 2: {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        filename = param1.getContentAsWideString();
        datasetname = param2.getContentAsWideString();
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << h5ReadDataset(filename, datasetname);
    return retval;
}
//=============================================================================
