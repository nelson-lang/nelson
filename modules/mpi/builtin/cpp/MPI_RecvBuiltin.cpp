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
#include "MPI_RecvBuiltin.hpp"
#include "Error.hpp"
#include "HandleManager.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_RecvBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() < 2) || (argIn.size() > 3)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    ArrayOf tmp = argIn[0];
    int source = tmp.getContentAsInteger32Scalar();
    tmp = argIn[1];
    int tag = tmp.getContentAsInteger32Scalar();
    MPI_Comm comm = MPI_COMM_WORLD;
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        comm = HandleToMpiComm(param3);
    }
    int msgsize = 0;
    MPI_Status status;
    MPI_Recv(&msgsize, 1, MPI_INT, source, tag, comm, &status);
    void* cp = malloc(msgsize);
    MPI_Recv(cp, msgsize, MPI_PACKED, status.MPI_SOURCE, status.MPI_TAG, comm, MPI_STATUS_IGNORE);
    int packpos = 0;
    ArrayOf A(unpackMPI(cp, msgsize, &packpos, comm));
    free(cp);
    retval.push_back(A);
    if (nLhs > 1) {
        retval.push_back(ArrayOf::int32Constructor(status.MPI_SOURCE));
    }
    if (nLhs > 2) {
        retval.push_back(ArrayOf::int32Constructor(status.MPI_TAG));
    }
    return retval;
}
//=============================================================================
