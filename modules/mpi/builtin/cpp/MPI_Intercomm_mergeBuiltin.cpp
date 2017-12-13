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
#include "MPI_Intercomm_mergeBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// comm2 = MPI_Intercomm_merge(comm1 [, highflag])
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Intercomm_mergeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() < 1) || (argIn.size() > 2))
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit)
    {
        Error(eval, _W("MPI must be initialized."));
    }
    MPI_Comm comm = HandleToMpiComm(argIn[0]);
    int highflag = 0;
    if (argIn.size() == 2)
    {
        ArrayOf param2 = argIn[1];
        highflag = param2.getContentAsInteger32Scalar();
    }
    MPI_Comm newcomm;
    int res = MPI_Intercomm_merge(comm, highflag, &newcomm);
    if (res != MPI_SUCCESS)
    {
        if (res == MPI_ERR_COMM)
        {
            Error(eval, _W("Invalid communicator."));
        }
        if (res == MPI_ERR_INTERN)
        {
            Error(eval, _W("Unable to acquire memory."));
        }
    }
    retval.push_back(MpiCommToHandle(newcomm));
    return retval;
}
//=============================================================================
