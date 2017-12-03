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
#include "MPI_Get_processor_nameBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Get_processor_nameBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 3)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    int flag = 0;
    MPI_Initialized(&flag);
    if (flag == 0)
    {
        Error(eval, _W("Attempting to use an MPI routine before initializing MPI."));
    }
    std::string processorName;
    char argv[MPI_MAX_PROCESSOR_NAME];
    int lenReturned = 0;
    int info = MPI_Get_processor_name(argv, &lenReturned);
    argv[lenReturned] = '\0';
    processorName = argv;
    retval.push_back(ArrayOf::stringConstructor(processorName));
    if (nLhs > 1)
    {
        retval.push_back(ArrayOf::doubleConstructor(lenReturned));
    }
    if (nLhs > 2)
    {
        retval.push_back(ArrayOf::doubleConstructor(info));
    }
    return retval;
}
//=============================================================================
