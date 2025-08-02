//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Get_library_versionBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "MPI_helpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Get_library_versionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    retval << ArrayOf::characterArrayConstructor(getMpiLibraryVersion());
    return retval;
}
//=============================================================================
