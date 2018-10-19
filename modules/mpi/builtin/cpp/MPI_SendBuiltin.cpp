//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "MPI_SendBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_SendBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf A;
    int dest = 0;
    int tag = 0;
    MPI_Comm comm = MPI_COMM_WORLD;
    switch (argIn.size()) {
    case 3: {
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        dest = param2.getContentAsInteger32Scalar();
        tag = param3.getContentAsInteger32Scalar();
    } break;
    case 4: {
        A = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        dest = param2.getContentAsInteger32Scalar();
        tag = param3.getContentAsInteger32Scalar();
        comm = HandleToMpiComm(param4);
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    int Asize = getArrayOfFootPrint(A, comm);
    int bufsize = Asize;
    void* cp = malloc(Asize);
    if (cp) {
        int packpos = 0;
        packMPI(A, cp, bufsize, &packpos, comm);
        MPI_Send(&packpos, 1, MPI_INT, dest, tag, comm);
        MPI_Send(cp, packpos, MPI_PACKED, dest, tag, comm);
        free(cp);
    }
    return retval;
}
//=============================================================================
