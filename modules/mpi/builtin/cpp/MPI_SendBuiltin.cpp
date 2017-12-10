//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <mpi.h>
#include "Error.hpp"
#include "MPI_SendBuiltin.hpp"
#include "MPI_helpers.hpp"
#include "MPI_CommHandleObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_SendBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() < 2) || (argIn.size() > 3))
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit)
    {
        Error(eval, _W("MPI must be initialized."));
    }
    ArrayOf A = argIn[0];
    ArrayOf tmp = argIn[1];
    int dest = tmp.getContentAsInteger32Scalar();
    tmp = argIn[2];
    int tag = tmp.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 3)
    {
        comm = HandleToMpiComm(argIn[3]);
    }
    int Asize = getArrayOfFootPrint(A, comm);
    int bufsize = Asize;
    void *cp = malloc(Asize);
    if (cp)
    {
        int packpos = 0;
        packMPI(A, cp, bufsize, &packpos, comm);
        MPI_Send(&packpos, 1, MPI_INT, dest, tag, comm);
        MPI_Send(cp, packpos, MPI_PACKED, dest, tag, comm);
        free(cp);
    }
    return retval;
}
//=============================================================================
