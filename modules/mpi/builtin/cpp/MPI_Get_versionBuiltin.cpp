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
#include "MPI_Get_versionBuiltin.hpp"
#include "Error.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Get_versionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 2);
    int version = 0;
    int subversion = 0;
    MPI_Get_version(&version, &subversion);
    retval << ArrayOf::doubleConstructor(version);
    if (nLhs > 1) {
        retval << ArrayOf::doubleConstructor(subversion);
    }
    return retval;
}
//=============================================================================
