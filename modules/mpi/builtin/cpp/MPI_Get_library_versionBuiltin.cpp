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
#include "MPI_Get_library_versionBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#ifndef MPI_MAX_LIBRARY_VERSION_STRING
#define MPI_MAX_LIBRARY_VERSION_STRING 64
#endif
//=============================================================================
ArrayOfVector Nelson::MpiGateway::MPI_Get_library_versionBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }

    char library_version[MPI_MAX_LIBRARY_VERSION_STRING];
    int resultlen = 0;
#if MPI_VERSION > 1
    MPI_Get_library_version(library_version, &resultlen);
	library_version[resultlen] = 0;
	std::string returnedString = library_version;
#else 
	std::string returnedString = "Unknown MPI version < 2";
#endif
    retval.push_back(ArrayOf::stringConstructor(returnedString));
    return retval;
}
//=============================================================================
