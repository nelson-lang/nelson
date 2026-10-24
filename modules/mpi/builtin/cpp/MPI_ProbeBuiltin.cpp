//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_ProbeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [STAT, INFO] = MPI_Probe(rank, tag [, COMM])
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_ProbeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(2);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(L"Nelson:MPI:notInitialized", L"MPI must be initialized.");
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    int src = param1.asInt32Scalar();
    int tag = param2.asInt32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 2) {
        comm = HandleToMpiComm(argIn[2]);
    }
    MPI_Status stat = { 0, 0, 0, 0 };
    int info = MPI_Probe(src, tag, comm, &stat);
    int count = 0;
    MPI_Get_count(&stat, MPI_CHAR, &count);
    int cancelled = 0;
    MPI_Test_cancelled(&stat, &cancelled);
    wstringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back(L"MPI_SOURCE");
    fieldnames.push_back(L"MPI_TAG");
    fieldnames.push_back(L"MPI_ERROR");
    fieldnames.push_back(L"count");
    fieldnames.push_back(L"cancelled");
    fieldvalues.push_back(ArrayOf::doubleScalar(stat.MPI_SOURCE));
    fieldvalues.push_back(ArrayOf::doubleScalar(stat.MPI_TAG));
    fieldvalues.push_back(ArrayOf::doubleScalar(stat.MPI_ERROR));
    fieldvalues.push_back(ArrayOf::doubleScalar(count));
    fieldvalues.push_back(ArrayOf::doubleScalar(cancelled));
    retval << ArrayOf::structArray(fieldnames, fieldvalues);
    retval << ArrayOf::doubleScalar((double)info);
    return retval;
}
//=============================================================================
