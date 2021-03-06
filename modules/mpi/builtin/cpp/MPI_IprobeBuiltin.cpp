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
#include "MPI_IprobeBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
// [FLAG, STAT, INFO] = MPI_IProbe(rank, tag, COMM)
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_IprobeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 3) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval(3);
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    int src = param1.getContentAsInteger32Scalar();
    int tag = param2.getContentAsInteger32Scalar();
    MPI_Comm comm = HandleToMpiComm(argIn[2]);
    MPI_Status stat = { 0, 0, 0, 0 };
    int flag = 0;
    int info = MPI_Iprobe(src, tag, comm, &flag, &stat);
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
    fieldvalues.push_back(ArrayOf::doubleConstructor(stat.MPI_SOURCE));
    fieldvalues.push_back(ArrayOf::doubleConstructor(stat.MPI_TAG));
    fieldvalues.push_back(ArrayOf::doubleConstructor(stat.MPI_ERROR));
    fieldvalues.push_back(ArrayOf::doubleConstructor(count));
    fieldvalues.push_back(ArrayOf::doubleConstructor(cancelled));
    retval << ArrayOf::doubleConstructor((double)flag);
    retval << ArrayOf::structConstructor(fieldnames, fieldvalues);
    retval << ArrayOf::doubleConstructor((double)info);
    return retval;
}
//=============================================================================
