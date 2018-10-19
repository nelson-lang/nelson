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
#include "MPI_Comm_splitBuiltin.hpp"
#include "Error.hpp"
#include "MPI_CommHandleObject.hpp"
#include "MPI_helpers.hpp"
#include <mpi.h>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MpiGateway::MPI_Comm_splitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if ((argIn.size() != 3)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flagInit = 0;
    MPI_Initialized(&flagInit);
    if (!flagInit) {
        Error(_W("MPI must be initialized."));
    }
    MPI_Comm comm = HandleToMpiComm(argIn[0]);
    ArrayOf param2 = argIn[1];
    int color = param2.getContentAsInteger32Scalar();
    ArrayOf param3 = argIn[2];
    int key = param3.getContentAsInteger32Scalar();
    MPI_Comm newcomm = MPI_COMM_NULL;
    if (MPI_Comm_split(comm, color, key, &newcomm) != MPI_SUCCESS) {
        Error(_W("MPI_Comm_split fails."));
    }
    retval.push_back(MpiCommToHandle(newcomm));
    return retval;
}
//=============================================================================
