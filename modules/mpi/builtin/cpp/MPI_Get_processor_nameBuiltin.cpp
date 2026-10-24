//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_Get_processor_nameBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Get_processor_nameBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 3);
    int flag = 0;
    MPI_Initialized(&flag);
    if (flag == 0) {
        Error(L"Nelson:MPI:notInitialized", L"MPI must be initialized.");
    }
    std::string processorName;
    char argv[MPI_MAX_PROCESSOR_NAME];
    int lenReturned = 0;
    int info = MPI_Get_processor_name(argv, &lenReturned);
    argv[lenReturned] = '\0';
    processorName = argv;
    retval << ArrayOf::characterArray(processorName);
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(lenReturned);
    }
    if (nLhs > 2) {
        retval << ArrayOf::doubleScalar(info);
    }
    return retval;
}
//=============================================================================
