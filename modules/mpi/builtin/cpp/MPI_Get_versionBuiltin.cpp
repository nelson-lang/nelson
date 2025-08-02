//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Get_versionBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
